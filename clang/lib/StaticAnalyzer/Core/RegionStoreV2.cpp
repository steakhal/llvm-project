// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This Store implements largely what is described in
// https://discourse.llvm.org/t/rfc-regionstore/70954/4
//
// TODO: Proper description.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/DeclCXX.h"
#include "clang/Basic/JsonSupport.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/MemRegion.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramState.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/SValBuilder.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/SValVisitor.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/SVals.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/Store.h"
#include "llvm/ADT/ImmutableMap.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Compiler.h"
#include <limits>
#include <variant>

using namespace clang;
using namespace ento;
using InvalidatedRegions = StoreManager::InvalidatedRegions;

namespace {

// TODO: At the callsites, honor the limited
// `RegionStoreMaxBindingFanOutPlusOne` option. Aka. BindingsLeft per Store
// operation.
// This function should be removed when the option is implemented.
BindResult alwaysBindsAllValues(StoreRef Store) {
  return BindResult{Store, /*FailedToBindValues=*/{}};
}

struct InvalidationEvent {
  const CallEvent *Call = nullptr;
  ConstCFGElementRef Cause;
  const LocationContext *Stack;
  unsigned BlockCount;
};

class BindingKey {
public:
  using OffsetType = unsigned;

  /// Construct a binding key for covering the bits in the exclusive range of
  /// \p Offset and \p ExlusiveEndOffset.
  static constexpr BindingKey fromExclusiveRange(OffsetType Offset,
                                                 OffsetType ExlusiveEndOffset) {
    assert(Offset < ExlusiveEndOffset);
    return BindingKey{Offset, ExlusiveEndOffset};
  }

  /// Construct a binding key for covering the bits in the inclusive range of
  /// \p Offset and \p InclusiveEndOffset.
  static constexpr BindingKey
  fromInclusiveRange(OffsetType Offset, OffsetType InclusiveEndOffset) {
    assert(Offset <= InclusiveEndOffset);
    return fromExclusiveRange(Offset, InclusiveEndOffset + 1);
  }

  /// Construct a binding key for covering the region \p R.
  /// If \p R has symbolic offset or extent, return std::nullopt.
  static std::optional<BindingKey> fromRegion(const MemRegion *R);

  /// Construct a binding key for region \p R stretching for extent \p extent.
  /// If \p R has symbolic offset or extent, return std::nullopt.
  static std::optional<BindingKey> fromRegionAndExtent(const MemRegion *R,
                                                       size_t extent);

  constexpr OffsetType beginOffset() const { return BeginBitOffset; }
  constexpr OffsetType endOffset() const { return EndBitOffset; }
  constexpr OffsetType extent() const { return EndBitOffset - BeginBitOffset; }

  void Profile(llvm::FoldingSetNodeID &ID) const {
    ID.AddInteger(BeginBitOffset);
    ID.AddInteger(EndBitOffset);
  }

  bool operator<(BindingKey X) const {
    return std::tie(BeginBitOffset, EndBitOffset) <
           std::tie(X.BeginBitOffset, X.EndBitOffset);
  }

  bool operator!=(BindingKey X) const { return !(*this == X); }
  bool operator==(BindingKey X) const {
    return std::tie(BeginBitOffset, EndBitOffset) ==
           std::tie(X.BeginBitOffset, X.EndBitOffset);
  }

  constexpr bool contains(BindingKey X) const;

  constexpr BindingKey merge(BindingKey X) const;

  static constexpr BindingKey widestBinding() {
    return fromExclusiveRange(0, std::numeric_limits<OffsetType>::max());
  }

  static constexpr BindingKey bindingForUnknown() {
    // This is a special key that represents the all-covering range,
    // but it is different from widestBinding to avoid falling into
    // "UndefinedVal" when we are looking for a value.
    // getBindingForRange if it finds widestBinding as the most specific
    // range, it will return UndefinedVal.
    // You should use this key when you want it to return UnknownVal instead,
    // i.e., to indicate that the value is initialized but not known.
    return fromExclusiveRange(0, std::numeric_limits<OffsetType>::max() - 1);
  }

private:
  OffsetType BeginBitOffset; // Inclusive begin of the bit range.
  OffsetType EndBitOffset;   // Exclusive end of the bit range.

  constexpr BindingKey(OffsetType BeginBitOffset, OffsetType EndBitOffset)
      : BeginBitOffset(BeginBitOffset), EndBitOffset(EndBitOffset) {}
};

constexpr auto fromInclusiveRange = BindingKey::fromInclusiveRange;

LLVM_DUMP_METHOD raw_ostream &operator<<(raw_ostream &OS, BindingKey Key) {
  return OS << Key.beginOffset() << ".." << (Key.endOffset() - 1);
}

constexpr bool BindingKey::contains(BindingKey X) const {
  bool BeginIsWithinRange =
      BeginBitOffset <= X.BeginBitOffset && X.BeginBitOffset < EndBitOffset;
  bool EndIsWithinRange =
      BeginBitOffset < X.EndBitOffset && X.EndBitOffset <= EndBitOffset;
  return BeginIsWithinRange && EndIsWithinRange;
}

static_assert(fromInclusiveRange(0, 7).contains(fromInclusiveRange(0, 7)));
static_assert(fromInclusiveRange(0, 7).contains(fromInclusiveRange(0, 6)));
static_assert(!fromInclusiveRange(0, 7).contains(fromInclusiveRange(0, 8)));
static_assert(!fromInclusiveRange(1, 1).contains(fromInclusiveRange(0, 1)));
static_assert(!fromInclusiveRange(1, 1).contains(fromInclusiveRange(2, 3)));

constexpr BindingKey BindingKey::merge(BindingKey X) const {
  auto Begin = std::min(BeginBitOffset, X.BeginBitOffset);
  auto ExclusiveEnd = std::max(EndBitOffset, X.EndBitOffset);
  return BindingKey::fromExclusiveRange(Begin, ExclusiveEnd);
}

size_t deduceBitWidth(const MemRegion *R) {
  QualType ApproximateType =
      loc::MemRegionVal{R}.getType(R->getContext())->getPointeeType();
  return R->getContext().getTypeSize(ApproximateType);
}

std::optional<BindingKey> BindingKey::fromRegion(const MemRegion *R) {
  auto TySizeInBits = deduceBitWidth(R);
  assert(TySizeInBits > 0 &&
         "TODO Cover flexible array members and other types with 0 size");

  return fromRegionAndExtent(R, TySizeInBits);
}

std::optional<BindingKey> BindingKey::fromRegionAndExtent(const MemRegion *R,
                                                          size_t extent) {
  assert(0 < extent);
  assert(deduceBitWidth(R) == 0 || extent <= deduceBitWidth(R));
  RegionOffset RegOff = R->getAsOffset();
  if (RegOff.hasSymbolicOffset())
    return std::nullopt;

  assert(0 <= RegOff.getOffset() &&
         (RegOff.getOffset() + extent) <=
             std::numeric_limits<OffsetType>::max() &&
         "Use a bigger offset type?");
  OffsetType BeginBitOffset = RegOff.getOffset();
  OffsetType ExclusiveEndBitOffset = BeginBitOffset + extent;
  return BindingKey::fromExclusiveRange(BeginBitOffset, ExclusiveEndBitOffset);
}

class BindingValue {
public:
  friend BindingValue directBinding(SVal Value);
  friend BindingValue defaultBinding(SVal Value);

  SVal value() const { return Value; }
  bool isDirect() const { return !IsDefaultBinding; }
  bool isDefault() const { return IsDefaultBinding; }

  bool operator==(BindingValue X) const {
    return std::tie(Value, IsDefaultBinding) ==
           std::tie(X.Value, X.IsDefaultBinding);
  }

  void Profile(llvm::FoldingSetNodeID &ID) const {
    ID.Add(Value);
    ID.AddBoolean(IsDefaultBinding);
  }

  friend raw_ostream &operator<<(raw_ostream &OS, BindingValue Val);

private:
  BindingValue(SVal Value, bool IsDefaultBinding);

  SVal Value;
  bool IsDefaultBinding;
};

bool shouldBindAsDefault(SVal V) {
  return isa<UndefinedVal, UnknownVal, nonloc::LazyCompoundVal,
             nonloc::CompoundVal>(V);
}

BindingValue::BindingValue(SVal Value, bool IsDefaultBinding)
    : Value(Value), IsDefaultBinding(IsDefaultBinding) {
  if (shouldBindAsDefault(Value)) {
    assert(IsDefaultBinding && "Can't directly bind these SVal kinds");
  }
}

BindingValue directBinding(SVal Value) {
  return BindingValue{Value, /*IsDefaultBinding=*/false};
}

BindingValue defaultBinding(SVal Value) {
  return BindingValue{Value, /*IsDefaultBinding=*/true};
}

raw_ostream &operator<<(raw_ostream &OS, BindingValue Val) {
  OS << Val.Value;
  if (Val.IsDefaultBinding)
    OS << " (default)";
  return OS;
}

class SymbolicBinding {
public:
  SymbolicBinding(const MemRegion *Key, SVal Value) : Key{Key}, Value{Value} {
    assert(Key);
  }

  void Profile(llvm::FoldingSetNodeID &ID) const {
    ID.Add(*Key);
    ID.Add(Value);
  }

  friend bool operator==(const SymbolicBinding &X, const SymbolicBinding &Y) {
    return X.Key == Y.Key && X.Value == Y.Value;
  }

  SVal value() const { return Value; }

  bool matchesRegion(const MemRegion *R) const { return Key == R; }

  void printJson(raw_ostream &Out, const char *NL, unsigned int Space,
                 bool IsDot) const;

private:
  const MemRegion *Key; // nonnull
  SVal Value;
};

using ClusterBindings = llvm::ImmutableMapRef<BindingKey, BindingValue>;

class RegionValue {
public:
  /*implicit*/ RegionValue(const ClusterBindings &CB) : Value{CB} {}
  /*implicit*/ RegionValue(const SymbolicBinding &SymBind) : Value{SymBind} {}

  const ClusterBindings *asClusterBindings() const {
    return std::get_if<ClusterBindings>(&Value);
  }

  const SymbolicBinding *asSymbolicBinding() const {
    return std::get_if<SymbolicBinding>(&Value);
  }

  void Profile(llvm::FoldingSetNodeID &ID) const;
  bool operator==(const RegionValue &X) const;

  void printJson(raw_ostream &Out, const char *NL, unsigned int Space,
                 bool IsDot) const;

private:
  std::variant<SymbolicBinding, ClusterBindings> Value;
};

void RegionValue::Profile(llvm::FoldingSetNodeID &ID) const {
  ID.AddInteger(Value.index());
  std::visit([&ID](const auto &X) { X.Profile(ID); }, Value);
}

bool RegionValue::operator==(const RegionValue &X) const {
  if (X.Value.index() != Value.index())
    return false;

  if (const auto *Self = asSymbolicBinding())
    return *Self == *X.asSymbolicBinding();

  return *asClusterBindings() == *X.asClusterBindings();
}

class RegionBindingsRef
    : public llvm::ImmutableMapRef<const MemRegion *, RegionValue> {
public:
  using ParentTy = llvm::ImmutableMapRef<const MemRegion *, RegionValue>;
  using ParentTy::ImmutableMapRef;

  explicit RegionBindingsRef(ParentTy const &other) : ParentTy(other) {}

  Store asStore() const {
    return reinterpret_cast<Store>(asImmutableMap().getRootWithoutRetain());
  }

  static RegionBindingsRef fromStore(Store S, FactoryTy *F) {
    return RegionBindingsRef(
        reinterpret_cast<ParentTy::TreeTy *>(const_cast<void *>(S)), F);
  }

  static RegionBindingsRef getEmptyBindings(FactoryTy *F) {
    return RegionBindingsRef(nullptr, F);
  }

  void printJson(raw_ostream &Out, const char *NL = "\n",
                 unsigned int Space = 0, bool IsDot = false) const;

  LLVM_DUMP_METHOD void dump() const { printJson(llvm::errs()); }
};

class RegionStoreManagerV2 final : public StoreManager {
  mutable RegionBindingsRef::FactoryTy RBFactory;
  mutable ClusterBindings::FactoryTy CBFactory;

public:
  explicit RegionStoreManagerV2(ProgramStateManager &StMgr)
      : StoreManager(StMgr) {}
  ~RegionStoreManagerV2() final = default;

  /// Return the value bound to specified location in a given state.
  /// \param[in] store The store in which to make the lookup.
  /// \param[in] loc The symbolic memory location.
  /// \param[in] T An optional type that provides a hint indicating the
  ///   expected type of the returned value.  This is used if the value is
  ///   lazily computed.
  /// \return The value bound to the location \c loc.
  SVal getBinding(Store store, Loc loc, QualType T = QualType()) override;

  /// Return the default value bound to a region in a given store. The default
  /// binding is the value of sub-regions that were not initialized separately
  /// from their base region. For example, if the structure is zero-initialized
  /// upon construction, this method retrieves the concrete zero value, even if
  /// some or all fields were later overwritten manually. Default binding may be
  /// an unknown, undefined, concrete, or symbolic value.
  /// \param[in] store The store in which to make the lookup.
  /// \param[in] R The region to find the default binding for.
  /// \return The default value bound to the region in the store, if a default
  /// binding exists.
  std::optional<SVal> getDefaultBinding(Store store,
                                        const MemRegion *R) override;

  /// Return a store with the specified value bound to the given location.
  /// \param[in] store The store in which to make the binding.
  /// \param[in] loc The symbolic memory location.
  /// \param[in] val The value to bind to location \c loc.
  /// \return A StoreRef object that contains the same
  ///   bindings as \c store with the addition of having the value specified
  ///   by \c val bound to the location given for \c loc.
  BindResult Bind(Store store, Loc loc, SVal val) override;

  /// Return a store with the specified value bound to all sub-regions of the
  /// region. The region must not have previous bindings. If you need to
  /// invalidate existing bindings, consider invalidateRegions().
  BindResult BindDefaultInitial(Store store, const MemRegion *R, SVal extent,
                                SVal V) override;

  /// Return a store with in which all values within the given region are
  /// reset to zero. This method is allowed to overwrite previous bindings.
  BindResult BindDefaultZero(Store store, const MemRegion *R,
                             SVal extent) override;

  /// Create a new store with the specified binding removed.
  /// \param ST the original store, that is the basis for the new store.
  /// \param L the location whose binding should be removed.
  StoreRef killBinding(Store ST, Loc L) override;

  /// getInitialStore - Returns the initial "empty" store representing the
  ///  value bindings upon entry to an analyzed function.
  StoreRef getInitialStore(const LocationContext *InitLoc) override;

  /// If the StoreManager supports it, increment the reference count of
  /// the specified Store object.
  void incrementReferenceCount(Store store) override;

  /// If the StoreManager supports it, decrement the reference count of
  /// the specified Store object.  If the reference count hits 0, the memory
  /// associated with the object is recycled.
  void decrementReferenceCount(Store store) override;

  /// ArrayToPointer - Used by ExprEngine::VistCast to handle implicit
  ///  conversions between arrays and pointers.
  SVal ArrayToPointer(Loc Array, QualType ElementTy) override;

  StoreRef removeDeadBindings(Store store, const StackFrameContext *LCtx,
                              SymbolReaper &SymReaper) override;

  bool includedInBindings(Store store, const MemRegion *region) const override;

  /// invalidateRegions - Clears out the specified regions from the store,
  ///  marking their values as unknown. Depending on the store, this may also
  ///  invalidate additional regions that may have changed based on accessing
  ///  the given regions. Optionally, invalidates non-static globals as well.
  /// \param[in] store The initial store
  /// \param[in] Values The values to invalidate.
  /// \param[in] E The current statement being evaluated. Used to conjure
  ///   symbols to mark the values of invalidated regions.
  /// \param[in] Count The current block count. Used to conjure
  ///   symbols to mark the values of invalidated regions.
  /// \param[in] Call The call expression which will be used to determine which
  ///   globals should get invalidated.
  /// \param[in,out] IS A set to fill with any symbols that are no longer
  ///   accessible. Pass \c NULL if this information will not be used.
  /// \param[in] ITraits Information about invalidation for a particular
  ///   region/symbol.
  /// \param[in,out] InvalidatedTopLevel A vector to fill with regions
  ////  explicitly being invalidated. Pass \c NULL if this
  ///   information will not be used.
  /// \param[in,out] Invalidated A vector to fill with any regions being
  ///   invalidated. This should include any regions explicitly invalidated
  ///   even if they do not currently have bindings. Pass \c NULL if this
  ///   information will not be used.
  StoreRef invalidateRegions(Store store, ArrayRef<SVal> Values,
                             ConstCFGElementRef Elem, unsigned Count,
                             const LocationContext *LCtx, const CallEvent *Call,
                             InvalidatedSymbols &IS,
                             RegionAndSymbolInvalidationTraits &ITraits,
                             InvalidatedRegions *InvalidatedTopLevel,
                             InvalidatedRegions *Invalidated) override;

  /// Finds the transitive closure of symbols within the given region.
  ///
  /// Returns false if the visitor aborted the scan.
  bool scanReachableSymbols(Store S, const MemRegion *R,
                            ScanReachableSymbols &Visitor) override;

  void printJson(raw_ostream &Out, Store S, const char *NL, unsigned int Space,
                 bool IsDot) const override;

  LLVM_DUMP_METHOD void dump(Store S) const;

  /// iterBindings - Iterate over the bindings in the Store.
  void iterBindings(Store store, BindingsHandler &f) override;

private:
  ClusterBindings makeEmptyClusterBindings() const {
    return {CBFactory.getEmptyTree(), &CBFactory};
  }

  RegionBindingsRef fromStore(Store S) {
    return RegionBindingsRef::fromStore(S, &RBFactory);
  }

  SVal getElementRegionValue(BindingKey ContainerRange, SVal ContainerVal,
                             BindingKey Element, const MemRegion *R);

  SVal getBindingForRange(const ClusterBindings &Cluster, BindingKey Key,
                          const MemRegion *R);

  ClusterBindings
  updatedClusterBindingOfRegionValue(const RegionValue *Previous,
                                     BindingKey Key, BindingValue Value) const;

  RegionBindingsRef bindConjured(RegionBindingsRef RB, const MemRegion *Key,
                                 DefinedOrUnknownSVal V) const;

  BindResult bindDefault(RegionBindingsRef RB, const MemRegion *Key,
                         SVal ExtentInBytes, SVal V);

  RegionBindingsRef invalidateRegion(RegionBindingsRef RB, const MemRegion *R,
                                     const InvalidationEvent &Event,
                                     InvalidatedRegions *InvalidatedTopLevel);
  friend class Invalidator;
};

//==------------------------------------------------------------------------==//
// RegionStoreManagerV2 implementation
// ==-----------------------------------------------------------------------==//
class RegionRebaser
    : private MemRegionVisitor<RegionRebaser, const SubRegion *> {
  friend class MemRegionVisitor<RegionRebaser, const SubRegion *>;

public:
  /// Rebase the region \p R to the new base region \p NewBase.
  /// NewBase is not necessarily the base region, it can be any subregion
  LLVM_ATTRIBUTE_RETURNS_NONNULL
  static const SubRegion *rebase(const TypedValueRegion *R,
                                 const TypedValueRegion *NewBase) {
    // The base region of R does not have anything to do with the base region of
    // NewBase. They may come from different memory regions of different kinds.
    return RegionRebaser{NewBase}.Visit(R); // Never null.
  }

private:
  explicit RegionRebaser(const TypedValueRegion *NewBase)
      : Mgr{NewBase->getMemRegionManager()}, NewBase{NewBase},
        NewBaseType{NewBase->getDesugaredValueType(Mgr.getContext())
                        ->getCanonicalTypeUnqualified()} {}

  const SubRegion *VisitMemRegion(const MemRegion *R) const {
    assert(R == R->getBaseRegion() &&
           "Visit methods should have handled all the non-base cases. "
           "Handle this region kind too.");
    return NewBase;
  }

  bool equivalentToNewBase(const MemRegion *OldBase) {
    if (const auto *R = dyn_cast<TypedValueRegion>(OldBase)) {
      QualType OldBaseTy = R->getDesugaredValueType(Mgr.getContext());
      return OldBaseTy->getCanonicalTypeUnqualified() == NewBaseType;
    }
    return false;
  }

  const SubRegion *Visit(const MemRegion *R) {
    if (equivalentToNewBase(R))
      return NewBase;
    return MemRegionVisitor::Visit(R);
  }

  const SubRegion *VisitElementRegion(const ElementRegion *R) {
    return Mgr.getElementRegionWithSuper(R, Visit(R->getSuperRegion()));
  }

  const SubRegion *VisitFieldRegion(const FieldRegion *R) {
    return Mgr.getFieldRegionWithSuper(R, Visit(R->getSuperRegion()));
  }

  const SubRegion *VisitObjCIvarRegion(const ObjCIvarRegion *R) {
    return Mgr.getObjCIvarRegion(R->getDecl(), Visit(R->getSuperRegion()));
  }

  const SubRegion *VisitCXXBaseObjectRegion(const CXXBaseObjectRegion *R) {
    return Mgr.getCXXBaseObjectRegionWithSuper(R, Visit(R->getSuperRegion()));
  }

  const SubRegion *
  VisitCXXDerivedObjectRegion(const CXXDerivedObjectRegion *R) {
    return Mgr.getCXXDerivedObjectRegion(R->getDecl(),
                                         Visit(R->getSuperRegion()));
  }

  MemRegionManager &Mgr;
  const TypedValueRegion *NewBase;
  CanQualType NewBaseType;
};

int getNumBases(const FieldDecl *FD) {
  const auto *Parent =
      dyn_cast<CXXRecordDecl>(FD->getParent()->getDefinition());
  return Parent ? Parent->getNumBases() : 0;
}

SVal getStructField(nonloc::CompoundVal Container, const FieldRegion *FR) {
  const auto *FieldDecl = FR->getDecl()->getCanonicalDecl();
  int FieldsToSkip = FieldDecl->getFieldIndex() + getNumBases(FieldDecl);
  for (auto Field : Container) {
    if (FieldsToSkip == 0)
      return Field;
    --FieldsToSkip;
  }
  return UnknownVal();
}

SVal getArrayElement(nonloc::CompoundVal Container,
                     const ElementRegion *ElemReg) {
  auto Index = ElemReg->getIndex();
  const auto *IntIndex = Index.getAsInteger();
  assert(IntIndex && "TODO: not implemented");
  auto FieldsToSkip = IntIndex->getExtValue();
  for (auto Field : Container) {
    if (FieldsToSkip == 0)
      return Field;
    --FieldsToSkip;
  }
  return UnknownVal();
}

SVal getCompoundValPart(nonloc::CompoundVal Container, const SubRegion *R) {
  if (const auto *Super = R->getSuperRegion();
      isa<FieldRegion, ElementRegion, CXXBaseObjectRegion>(Super)) {
    auto immediateContainer =
        getCompoundValPart(Container, Super->getAs<SubRegion>());
    auto IC = immediateContainer.getAs<nonloc::CompoundVal>();
    assert(
        IC &&
        "TODO: LazyCompoundVals nested with compound vals are not supported");
    Container = *IC;
  }
  if (const auto *BaseReg = R->getAs<CXXBaseObjectRegion>()) {
    const auto *SupReg = R->getSuperRegion()->getAs<TypedValueRegion>();
    const auto *RecDecl = SupReg->getValueType()->getAsCXXRecordDecl();
    auto ContainerPiece = Container.begin();
    for (const auto &B : RecDecl->bases()) {
      if (B.getType()->getAsCXXRecordDecl() == BaseReg->getDecl())
        return *ContainerPiece;
      ++ContainerPiece;
      assert(ContainerPiece != Container.end() && "Base is not in the region");
    }
    return UnknownVal();
  }
  if (const auto *FieldReg = R->getAs<FieldRegion>()) {
    return getStructField(Container, FieldReg);
  }
  if (const auto *ElemReg = R->getAs<ElementRegion>()) {
    return getArrayElement(Container, ElemReg);
  }
  return UnknownVal();
}

bool canDigDeeper(SVal ContainerVal, const MemRegion *R) {
  return (ContainerVal.getAs<nonloc::LazyCompoundVal>() &&
          isa<SubRegion>(R)); // TODO: what is the actual condition?
}

SVal deriveValueFromSymbol(nonloc::SymbolVal SV, const TypedValueRegion *R,
                           SValBuilder &SVB) {
  if (isa<VarRegion>(R))
    return SV;
  return SVB.getDerivedRegionValueSymbolVal(SV.getSymbol(), R);
}

SVal RegionStoreManagerV2::getElementRegionValue(BindingKey ContainerRange,
                                                 SVal ContainerVal,
                                                 BindingKey Element,
                                                 const MemRegion *R) {
  assert(ContainerRange.contains(Element));
  if ((ContainerRange == Element) && !canDigDeeper(ContainerVal, R)) {
    // The key exactly matches the range
    return ContainerVal;
  }
  if (ContainerVal.isUnknownOrUndef())
    return ContainerVal;

  if (auto LCV = ContainerVal.getAs<nonloc::LazyCompoundVal>()) {
    // Happens when returning struct by value
    const auto *LCVReg = LCV->getRegion();
    if (const auto *RSym = R->getAs<SymbolicRegion>();
        RSym && RSym->getPointeeStaticType().getCanonicalType().getTypePtr() ==
                    LCVReg->getDesugaredValueType(svalBuilder.getContext())
                        .getCanonicalType()
                        .getTypePtr()) {
      return getBinding(LCV->getStore(), loc::MemRegionVal{LCVReg}, QualType{});
    }
    const auto *RTyped = R->getAs<TypedValueRegion>();
    assert(RTyped);
    const SubRegion *RebasedR = RegionRebaser::rebase(RTyped, LCVReg);
    assert(RebasedR && "Always nonnull");
    // TODO: We should actually pass the requested type instead of a NULL type.
    return getBinding(LCV->getStore(), loc::MemRegionVal{RebasedR}, QualType{});
  }

  auto CompVal = ContainerVal.getAs<nonloc::CompoundVal>();
  if (!CompVal) {
    // We can interpolate zero constants
    if (const auto *TypedReg = R->getAs<TypedRegion>();
        TypedReg && ContainerVal.isZeroConstant()) {
      QualType T{TypedReg->getDesugaredLocationType(Ctx)
                     ->getPointeeOrArrayElementType(),
                 0};
      return svalBuilder.makeZeroVal(T);
    }
    if (isa_and_nonnull<SymbolicRegion>(ContainerVal.getAsRegion())) {
      return ContainerVal;
    }
    if (auto SV = ContainerVal.getAs<nonloc::SymbolVal>()) {
      const auto *TypedR = dyn_cast<TypedValueRegion>(R);
      assert(TypedR && "TODO: Handle non-TypedValueRegion");
      return deriveValueFromSymbol(*SV, TypedR, svalBuilder);
    }
    return UnknownVal();
  }
  if (const auto *SubReg = R->getAs<SubRegion>()) {
    return getCompoundValPart(*CompVal, SubReg);
  }
  return UnknownVal();
}

SVal readingFromPartiallyCoveredRegion(const MemRegion *R) {
  if (isa<GlobalsSpaceRegion, UnknownSpaceRegion>(R->getRawMemorySpace()))
    return UnknownVal();
  return UndefinedVal();
}

SVal RegionStoreManagerV2::getBindingForRange(const ClusterBindings &Cluster,
                                              BindingKey Key,
                                              const MemRegion *R) {
  BindingKey::OffsetType CoveredUntilExclusive = Key.beginOffset();

  BindingKey MostSpecificRangeRange = BindingKey::widestBinding();
  SVal MostSpecificRangeVal = UnknownVal();
  bool ValueIsSpoiled = false;

  // Iterates the sooner and shorter bindings first.
  for (auto const &[Range, Value] : Cluster) {
    if (Range.endOffset() <= Key.beginOffset()) {
      // Skip ranges that end before the key starts.
      // [  Range  ]
      //             [  Key  ]
      continue;
    }
    if (Range.beginOffset() >= Key.endOffset()) {
      // Skip remaining ranges that all start after the key ends.
      //            [  Range  ]
      //   [  Key  ]
      break;
    }
    assert(Value.value() != UndefinedVal() &&
           "How could undefined value be bound?");

    // If the next soonest starting binding still won't cover the next bits of
    // the key, then those bits aren't covered by any remaining binding.
    if (CoveredUntilExclusive < Range.beginOffset()) {
      return readingFromPartiallyCoveredRegion(R);
    }
    CoveredUntilExclusive = std::max(CoveredUntilExclusive, Range.endOffset());

    if (Range.extent() < Key.extent()) {
      // Misaligned range spoils anything we could know about the key
      //  [ Range ]    [ Range ]         [ Range ]
      // [   Key   ]     [   Key  ]  [   Key   ]
      ValueIsSpoiled = true;
      continue;
    }
    assert(Key.extent() <= Range.extent());
    if (Range.contains(Key)) {
      // Key is fully covered by the range:
      // [  Range  ]   or [  Range ]  or [  Range  ]
      // [  Key    ]      [ Key ]         [   Key ]
      if (MostSpecificRangeRange.extent() > Range.extent()) {
        MostSpecificRangeRange = Range;
        MostSpecificRangeVal = Value.value();
      }
      continue;
    }
    // Key is misaligned with the range
    // [  Range  ]    or [  Range  ]
    //     [  Key  ]   [  Key  ]
    ValueIsSpoiled = true;
  }
  if (bool noParent = (MostSpecificRangeRange == BindingKey::widestBinding());
      noParent || ValueIsSpoiled) {
    const bool AllBitsAreCovered = CoveredUntilExclusive >= Key.endOffset();
    return AllBitsAreCovered ? UnknownVal()
                             : readingFromPartiallyCoveredRegion(R);
  }
  return getElementRegionValue(MostSpecificRangeRange, MostSpecificRangeVal,
                               Key, R);
}

std::optional<BindingKey> tryShrinkFromRight(BindingKey ExistingRange,
                                             BindingKey Key) {
  // It is supposed to be run in the context of addBindingForRange
  assert(Key.beginOffset() < ExistingRange.endOffset());
  assert(ExistingRange.beginOffset() < Key.endOffset());

  if (ExistingRange.beginOffset() < Key.beginOffset() &&
      ExistingRange.endOffset() <= Key.endOffset()) {
    return BindingKey::fromExclusiveRange(ExistingRange.beginOffset(),
                                          Key.beginOffset());
  }
  return std::nullopt;
}

std::optional<BindingKey> tryShrinkFromLeft(BindingKey ExistingRange,
                                            BindingKey Key) {
  // It is supposed to be run in the context of addBindingForRange
  assert(Key.beginOffset() < ExistingRange.endOffset());
  assert(ExistingRange.beginOffset() < Key.endOffset());

  if (Key.beginOffset() <= ExistingRange.beginOffset() &&
      Key.endOffset() < ExistingRange.endOffset()) {
    return BindingKey::fromExclusiveRange(Key.endOffset(),
                                          ExistingRange.endOffset());
  }
  return std::nullopt;
}

ClusterBindings addBindingForRange(ClusterBindings Cluster, BindingKey Key,
                                   BindingValue Value) {
  SmallVector<BindingKey, 10> RemovedRanges;
  struct CutRange {
    BindingKey OriginalRange;
    BindingKey NewRange;
    BindingValue Value;
  };
  SmallVector<CutRange, 10> CutDefaultRanges;
  BindingKey InvalidationRange = Key;
  for (auto const &[ExistingRange, ExistingV] : Cluster) {
    if (ExistingRange.endOffset() <= Key.beginOffset()) {
      // Skip ranges that end before the key starts.
      // [  Range  ]
      //             [  Key  ]
      continue;
    }
    if (ExistingRange.beginOffset() >= Key.endOffset()) {
      // Skip remaining ranges that all start after the key ends.
      //            [  Range  ]
      //   [  Key  ]
      break;
    }
    if (Key.contains(ExistingRange)) {
      // Range is fully covered by the key:
      //  [ Range ]
      // [  Key    ]
      // No effect on invalidation range, this Range will be replaced by Value.
      RemovedRanges.push_back(ExistingRange);
      continue;
    }
    if (ExistingV.isDefault()) {
      // Default binding is still useful even if partially covered by the Key
      if (auto LeftRemains = tryShrinkFromRight(ExistingRange, Key)) {
        CutDefaultRanges.push_back({ExistingRange, *LeftRemains, ExistingV});
      } else if (auto RightRemains = tryShrinkFromLeft(ExistingRange, Key)) {
        CutDefaultRanges.push_back({ExistingRange, *RightRemains, ExistingV});
      } else {
        // Key is fully within ExistingRange, so there is no ambiguity and we
        // can keep this default binding.
      }
    } else {
      // Range is not fully covered by the key, so it is invalidated. As an
      // effect, invalidation range might extend to lower offsets. There is no
      // need to rescan the previous ranges because they do not need to be
      // invalidated, as they are not directly affected by the key.
      InvalidationRange = InvalidationRange.merge(ExistingRange);
      RemovedRanges.push_back(ExistingRange);
    }
  }
  for (auto Range : RemovedRanges) {
    Cluster = Cluster.remove(Range);
  }
  for (const auto &[OriginalRange, NewRange, DefaultValue] : CutDefaultRanges) {
    Cluster = Cluster.remove(OriginalRange).add(NewRange, DefaultValue);
  }
  if (InvalidationRange != Key) {
    Cluster = Cluster.add(InvalidationRange, defaultBinding(UnknownVal()));
  }
  return Cluster.add(Key, Value);
}

class BindingForEmptyStore
    : private MemRegionVisitor<BindingForEmptyStore, SVal> {
  friend class MemRegionVisitor<BindingForEmptyStore, SVal>;
  using Base = MemRegionVisitor<BindingForEmptyStore, SVal>;

  SVal VisitVarRegion(const VarRegion *R) {
    const VarDecl *VD = R->getDecl();
    const Expr *Init = VD->getAnyInitializer();

    // Trust constant initializers.
    if (VD->getType().isConstQualified() && Init)
      return SVB.getConstantVal(Init).value_or(UnknownVal());

    return Base::VisitVarRegion(R);
  }

  SVal VisitTypedValueRegion(const TypedValueRegion *R) {
    const MemSpaceRegion *MS = R->getRawMemorySpace();

    // Top-level parameters are always symbolic.
    if (isa<StackArgumentsSpaceRegion>(MS))
      return SVB.getRegionValueSymbolVal(R);

    // Function-scoped static variables are default-initialized to 0; if they
    // have an initializer, it would have been processed by now.
    // FIXME: This is only true when we're starting analysis from main().
    // We're losing a lot of coverage here.
    if (isa<StaticGlobalSpaceRegion>(MS))
      return SVB.makeZeroVal(R->getValueType());

    if (isa<GlobalsSpaceRegion, UnknownSpaceRegion>(MS))
      return SVB.getRegionValueSymbolVal(R);

    return Base::VisitTypedValueRegion(R);
  }

  SValBuilder &SVB;
  explicit BindingForEmptyStore(SValBuilder &SVB) : SVB{SVB} {}

public:
  static SVal get(SValBuilder &SVB, const MemRegion *R) {
    return BindingForEmptyStore{SVB}.Visit(R);
  }
};

SVal RegionStoreManagerV2::getBinding(Store store, Loc loc, QualType) {
  const MemRegion *MR = loc.getAsRegion();
  assert(MR && "Alternative not implemented TODO.");
  auto RB = fromStore(store);
  const auto *Cluster = RB.lookup(MR->getBaseRegion());
  if (!Cluster)
    return BindingForEmptyStore::get(svalBuilder, MR);

  if (const auto *TVR = dyn_cast<TypedValueRegion>(MR);
      TVR && TVR->getValueType()->isStructureOrClassType()) {

    // Try to get the existing LCV for that class.
    if (auto LoadKey = BindingKey::fromRegion(MR)) {
      if (const auto *CB = Cluster->asClusterBindings()) {
        if (SVal Binding = getBindingForRange(*CB, *LoadKey, MR);
            Binding.getAs<nonloc::LazyCompoundVal>()) {
          return Binding;
        }
      }
    }
    return svalBuilder.makeLazyCompoundVal(StoreRef(store, *this), TVR);
  }

  if (auto LoadKey = BindingKey::fromRegion(MR)) {
    if (const auto *CB = Cluster->asClusterBindings()) {
      return getBindingForRange(*CB, *LoadKey, MR);
    }
    // Symbolic binding obscures the view.
    // There might have been a value written by this offset
    // bot it might have also been overwritten by the symbolic binding.
    return UnknownVal();
  }

  if (const auto *SB = Cluster->asSymbolicBinding())
    return SB->matchesRegion(MR) ? SB->value() : UnknownVal();

  // It is too expensive to fork the state here for every possible
  // value of the symbolic range.
  // TODO: Shold we make an exception for an all-zeroes binding
  // or a single default binding?
  return UnknownVal();
}

std::optional<SVal>
RegionStoreManagerV2::getDefaultBinding(Store store, const MemRegion *R) {
  return std::nullopt;
}

ClusterBindings RegionStoreManagerV2::updatedClusterBindingOfRegionValue(
    const RegionValue *Previous, BindingKey Key, BindingValue Value) const {
  if (!Previous) {
    // There was no binding at all
    return makeEmptyClusterBindings().add(Key, Value);
  }
  if (const auto *PrevBindings = Previous->asClusterBindings()) {
    // Some concrete bindings exist, just add this one
    return addBindingForRange(*PrevBindings, Key, Value);
  }
  // There was a symbolic binding. This concrete binding invalidates it.
  // However, as we don't know what was bound, we add a wide "unknown" binding
  // to make sure we don't trigger a FP report of an uninitialized variable.
  return makeEmptyClusterBindings()
      .add(BindingKey::bindingForUnknown(), defaultBinding(UnknownVal()))
      .add(Key, Value);
}

BindResult RegionStoreManagerV2::Bind(Store store, Loc loc, SVal val) {
  const MemRegion *MR = loc.getAsRegion();
  assert(MR && "Alternative not implemented TODO.");
  assert(!val.isUndef() &&
         "Binding Undef is only implemented for initial binding TODO.");
  auto RB = fromStore(store);
  auto Key = BindingKey::fromRegion(MR);
  const auto *Cluster = RB.lookup(MR->getBaseRegion());
  if (Key) {
    BindingValue BindingV =
        shouldBindAsDefault(val) ? defaultBinding(val) : directBinding(val);

    ClusterBindings Updated =
        updatedClusterBindingOfRegionValue(Cluster, *Key, BindingV);
    // Here RegionBindingsRef is a temprary that holds a pointer to immutable
    // map, which StoreRef keeps alive. If it wasn't passed to StoreRef within
    // the same expression, it would be destroyed and the pointer returned by
    // asStore would be invalid.
    return alwaysBindsAllValues(StoreRef(
        RegionBindingsRef(RB.add(MR->getBaseRegion(), Updated)).asStore(),
        *this));
  }
  // Overwrite whatever previous binding was:
  // with symbolic bindings we keep memory only for a single binding.
  return alwaysBindsAllValues(StoreRef(
      RegionBindingsRef(RB.add(MR->getBaseRegion(), SymbolicBinding{MR, val}))
          .asStore(),
      *this));
}

BindResult RegionStoreManagerV2::bindDefault(RegionBindingsRef RB,
                                             const MemRegion *R,
                                             SVal ExtentInBytes, SVal Value) {
  const auto *Cluster = RB.lookup(R->getBaseRegion());
  const auto *KnownExtent = ExtentInBytes.getAsInteger();
  assert(KnownExtent && "TODO: handle symbolic extents");
  assert(KnownExtent->getExtValue() > 0 &&
         "TODO: handle zero and negative extents");
  size_t BitExtent = KnownExtent->getExtValue() * Ctx.getTypeSize(Ctx.CharTy);
  if (const auto Key = BindingKey::fromRegionAndExtent(R, BitExtent)) {
    ClusterBindings Updated = updatedClusterBindingOfRegionValue(
        Cluster, *Key, defaultBinding(Value));

    return alwaysBindsAllValues(StoreRef(
        RegionBindingsRef(RB.add(R->getBaseRegion(), Updated)).asStore(),
        *this));
  }
  assert(false && "TODO: Symbolic memset not implemented");
  return alwaysBindsAllValues(StoreRef(RB.asStore(), *this));
}

BindResult RegionStoreManagerV2::BindDefaultInitial(Store S, const MemRegion *R,
                                                    SVal ExtentInBytes,
                                                    SVal Value) {
  RegionBindingsRef Cluster = fromStore(S);
  assert(!Cluster.lookup(R->getBaseRegion()) && "Double initialization!");
  (void)Cluster;

  // No need to add an undef initial binding.
  if (Value.isUndef())
    return alwaysBindsAllValues(StoreRef(S, *this));

  return bindDefault(Cluster, R, ExtentInBytes, Value);
}

BindResult RegionStoreManagerV2::BindDefaultZero(Store S, const MemRegion *R,
                                                 SVal ExtentInBytes) {
  return bindDefault(fromStore(S), R, ExtentInBytes,
                     svalBuilder.makeZeroVal(Ctx.CharTy));
}

StoreRef RegionStoreManagerV2::killBinding(Store ST, Loc L) {
  return StoreRef(ST, *this);
}

StoreRef RegionStoreManagerV2::getInitialStore(const LocationContext *InitLoc) {
  return StoreRef(RegionBindingsRef::getEmptyBindings(&RBFactory).asStore(),
                  *this);
}

void RegionStoreManagerV2::incrementReferenceCount(Store store) {

  auto RB = RegionBindingsRef::fromStore(store, &RBFactory);
  RB.manualRetain();
}

void RegionStoreManagerV2::decrementReferenceCount(Store store) {
  auto RB = RegionBindingsRef::fromStore(store, &RBFactory);
  RB.manualRelease();
}

SVal RegionStoreManagerV2::ArrayToPointer(Loc Array, QualType ElementTy) {
  return Array;
}

StoreRef RegionStoreManagerV2::removeDeadBindings(Store store,
                                                  const StackFrameContext *LCtx,
                                                  SymbolReaper &SymReaper) {
  return StoreRef(store, *this);
}

bool RegionStoreManagerV2::includedInBindings(Store S,
                                              const MemRegion *R) const {
  // This function is only effectively used in StoreSiteFinder,
  // even though it was designed for SymbolReaper.
  // It looks like right now returning false has the same effect on
  // SymbolReaper. Perhaps this API can be removed/replaced
  auto RB = RegionBindingsRef::fromStore(S, &RBFactory);
  R = R->getBaseRegion();

  // Quick path: if the base is the head of a cluster, the region is live.
  // I guess the fact of having a binding for R means R most have been kept
  // alive and not garbage-collected by removeDeadBindings.
  if (RB.lookup(R))
    return true;

  // Slow path: if the region is the VALUE of any binding, it is live.
  for (const auto &Cluster : llvm::make_second_range(RB)) {
    if (Cluster.asSymbolicBinding()) {
      // TODO
      continue;
    }
    const auto *ClusterBindings = Cluster.asClusterBindings();
    assert(ClusterBindings);
    for (const auto &Val : llvm::make_second_range(*ClusterBindings)) {
      if (const MemRegion *BoundR = Val.value().getAsRegion();
          BoundR && BoundR->getBaseRegion() == R) {
        return true;
      }
    }
  }

  return false;
}

RegionBindingsRef
RegionStoreManagerV2::bindConjured(RegionBindingsRef RB, const MemRegion *Key,
                                   DefinedOrUnknownSVal V) const {
  auto UnknownCluster = makeEmptyClusterBindings().add(
      BindingKey::bindingForUnknown(), defaultBinding(V));
  return RegionBindingsRef(RB.add(Key, UnknownCluster));
}

RegionBindingsRef RegionStoreManagerV2::invalidateRegion(
    RegionBindingsRef RB, const MemRegion *R, const InvalidationEvent &Event,
    InvalidatedRegions *InvalidatedTopLevel) {
  // TODO: Respect TK_DoNotInvalidateSuperRegion
  R = R->getBaseRegion();

  if (InvalidatedTopLevel)
    InvalidatedTopLevel->push_back(R);

  if (const auto *TR = R->getAs<TypedValueRegion>()) {
    auto T = TR->getValueType();
    DefinedOrUnknownSVal Conjured = svalBuilder.conjureSymbolVal(
        R, Event.Cause, Event.Stack, T, Event.BlockCount);
    return bindConjured(RB, R, Conjured);
  }
  // TODO: void* arguments for some functions like memset
  return RB;
}

auto overlapsWith(BindingKey Range) {
  return [Range](const std::pair<BindingKey, BindingValue> &Binding) {
    auto Key = Binding.first;
    return Key.endOffset() > Range.beginOffset() &&
           Range.endOffset() > Key.beginOffset();
  };
}

class Invalidator : private SValVisitor<Invalidator> {
  friend class SValVisitor;

public:
  Invalidator(RegionStoreManagerV2 &Mgr, RegionBindingsRef Store,
              const InvalidationEvent &Place, InvalidatedSymbols &IS,
              InvalidatedRegions *InvalidatedTopLevel)
      : Mgr{Mgr}, Place{Place}, Store{Store},
        InvalidatedTopLevel{InvalidatedTopLevel}, IS{IS} {}

  RegionBindingsRef invalidate(ArrayRef<SVal> Values) {
    for (SVal Value : Values)
      Visit(Value);
    return Store;
  }

private:
  void VisitSymbolVal(nonloc::SymbolVal Sym) { IS.insert(Sym.getSymbol()); }

  void VisitCompoundVal(nonloc::CompoundVal CV) {
    for (SVal S : CV) {
      Visit(S);
    }
  }

  void VisitLazyCompoundVal(nonloc::LazyCompoundVal LCV) {
    const auto *LazyR = LCV.getRegion();
    RegionBindingsRef BR = Mgr.fromStore(LCV.getStore());
    const auto *Cluster = BR.lookup(LazyR->getBaseRegion());
    std::optional<BindingKey> LazyKey = BindingKey::fromRegion(LazyR);

    if (!Cluster || !LazyKey)
      return;

    if (const auto *SymBinding = Cluster->asSymbolicBinding()) {
      (void)SymBinding;
      return; // TODO: Handle.
    }

    for (const auto &[Key, Binding] : make_filter_range(
             *Cluster->asClusterBindings(), overlapsWith(*LazyKey))) {
      if (Binding.isDefault() || LazyKey->contains(Key)) {
        Visit(Binding.value());
      }
    }
  }

  void VisitMemRegionVal(loc::MemRegionVal Val) {
    Visit(Mgr.getBinding(Store.asStore(), Val));
    Store = Mgr.invalidateRegion(Store, Val.getRegion(), Place,
                                 InvalidatedTopLevel);
  }

  RegionStoreManagerV2 &Mgr;
  InvalidationEvent Place;

  RegionBindingsRef Store;
  InvalidatedRegions *InvalidatedTopLevel;
  InvalidatedSymbols &IS;
  // TODO: Implement caching.
};

StoreRef RegionStoreManagerV2::invalidateRegions(
    Store store, ArrayRef<SVal> Values, ConstCFGElementRef Elem, unsigned Count,
    const LocationContext *LCtx, const CallEvent *Call, InvalidatedSymbols &IS,
    RegionAndSymbolInvalidationTraits &ITraits,
    InvalidatedRegions *InvalidatedTopLevel, InvalidatedRegions *Invalidated) {
  auto RB = fromStore(store);
  InvalidationEvent Event{Call, Elem, LCtx, Count};
  Invalidator Visitor(*this, RB, Event, IS, InvalidatedTopLevel);
  // TODO: Make use of these at some point.
  (void)ITraits;
  (void)Invalidated;
  return StoreRef(Visitor.invalidate(Values).asStore(), *this);
}

bool RegionStoreManagerV2::scanReachableSymbols(Store S, const MemRegion *R,
                                                ScanReachableSymbols &Visitor) {
  assert(R == R->getBaseRegion() && "Should only be called for base regions");
  const auto *Cluster = fromStore(S).lookup(R);

  if (!Cluster)
    return true;

  if (const auto *SB = Cluster->asSymbolicBinding()) {
    return Visitor.scan(SB->value());
  }

  const auto *CB = Cluster->asClusterBindings();
  assert(CB);
  for (const auto &Binding : llvm::make_second_range(*CB)) {
    if (!Visitor.scan(Binding.value()))
      return false;
  }

  return true;
}

void RegionBindingsRef::printJson(raw_ostream &Out, const char *NL,
                                  unsigned int Space, bool IsDot) const {
  auto CommaNewLine = [NL, &Out] { Out << ',' << NL; };
  auto PrintOneCluster =
      [=, &Out](const std::pair<const MemRegion *, RegionValue> &Cluster) {
        const auto &[Region, ClusterValue] = Cluster;
        Indent(Out, Space, IsDot)
            << R"({ "cluster": ")" << Region << R"(", "pointer": ")"
            << (const void *)Region << R"(", "items": )";
        ClusterValue.printJson(Out, NL, Space, IsDot);
        Out << "}";
      };

  llvm::interleave(*this, PrintOneCluster, CommaNewLine);
}

void RegionStoreManagerV2::printJson(raw_ostream &Out, Store S, const char *NL,
                                     unsigned int Space, bool IsDot) const {
  auto Clusters = RegionBindingsRef::fromStore(S, &RBFactory);
  Indent(Out, Space, IsDot) << R"("store": )";
  if (Clusters.isEmpty()) {
    Out << "null," << NL;
    return;
  }

  Out << R"({ "pointer": ")" << Clusters.asStore() << R"(", "items": [)" << NL;
  Clusters.printJson(Out, NL, Space + 1, IsDot);
  Indent(Out << NL, Space, IsDot) << "]}," << NL;
}

void SymbolicBinding::printJson(raw_ostream &Out, const char *, unsigned int,
                                bool) const {
  Out << R"({"key": ")" << Key << R"(", "value": ")" << Value << "\"}";
}

void RegionValue::printJson(raw_ostream &Out, const char *NL,
                            unsigned int Space, bool IsDot) const {
  if (const auto *Self = asSymbolicBinding()) {
    Self->printJson(Out, NL, Space, IsDot);
    return;
  }

  assert(!asClusterBindings()->isEmpty());
  auto CommaNewLine = [NL, &Out] { Out << ',' << NL; };
  auto PrintOneBinding =
      [Space, IsDot, &Out](const std::pair<BindingKey, BindingValue> &Entry) {
        const auto &[Key, ToVal] = Entry;
        Indent(Out, Space + 1, IsDot) << "{\"" << Key << "\": \"";
        Out << ToVal << "\"}";
      };

  Out << "[" << NL;
  llvm::interleave(*asClusterBindings(), PrintOneBinding, CommaNewLine);
  Indent(Out << NL, Space, IsDot) << "]";
}

void RegionStoreManagerV2::dump(Store S) const {
  printJson(llvm::errs(), S, "\n", 0, false);
}

void RegionStoreManagerV2::iterBindings(Store S, BindingsHandler &F) {
  auto Clusters = RegionBindingsRef::fromStore(S, &RBFactory);
  for (const auto &[BaseRegion, Bindings] : Clusters) {
    if (isa<MemSpaceRegion>(BaseRegion))
      continue;

    if (const SymbolicBinding *SymBinding = Bindings.asSymbolicBinding()) {
      if (F.HandleBinding(BaseRegion, SymBinding->value()) ==
          BindingsHandler::DoneHandling) {
        return;
      }
      continue;
    }

    for (const auto &[_, Binding] : *Bindings.asClusterBindings()) {
      if (Binding.isDefault())
        continue;
      if (F.HandleBinding(BaseRegion, Binding.value()) ==
          BindingsHandler::DoneHandling)
        return;
    }
  }
}

} // end anonymous namespace

std::unique_ptr<StoreManager>
clang::ento::CreateRegionStoreV2(ProgramStateManager &StMgr) {
  return std::make_unique<RegionStoreManagerV2>(StMgr);
}
