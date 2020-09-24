//== ArrayBoundCheckerV2.cpp ------------------------------------*- C++ -*--==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines ArrayBoundCheckerV2, which is a path-sensitive check
// which looks for an out-of-bound array element access.
//
//===----------------------------------------------------------------------===//

#include "Taint.h"
#include "clang/StaticAnalyzer/Checkers/BuiltinCheckerRegistration.h"
#include "clang/StaticAnalyzer/Core/BugReporter/BugType.h"
#include "clang/StaticAnalyzer/Core/Checker.h"
#include "clang/StaticAnalyzer/Core/CheckerManager.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/APSIntType.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/DynamicSize.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ExprEngine.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/SValVisitor.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

// Enable dumps using the following flag combination:
// '-mllvm -debug-only=clang-analyzer-array-bound-checker-v2'
#define DEBUG_TYPE "clang-analyzer-array-bound-checker-v2"

using namespace clang;
using namespace ento;
using namespace taint;

using ConcreteInt = nonloc::ConcreteInt;
using SymbolVal = nonloc::SymbolVal;
using APSInt = llvm::APSInt;

namespace {
/// Returns true if the integer can be casted to signed with the same bit width
/// without wrapping.
static bool representableBySigned(const APSInt &X) {
  return X.isSigned() || X.isSignBitClear();
}

/// Gets the minimal bit width signed type to be able to represent both integers
/// without losing precision or wrapping.
static APSIntType commonSignedTypeToFit(const APSInt &Lhs, const APSInt &Rhs) {
  unsigned GreaterBitWidth = std::max(Lhs.getBitWidth(), Rhs.getBitWidth());
  APSIntType Signed = APSIntType(GreaterBitWidth, /*Unsigned=*/false);
  APSIntType ExtendedSigned =
      APSIntType(GreaterBitWidth + 1, /*Unsigned=*/false);

  const bool NeedsExtraBitToPreserveSigness =
      Signed.testInRange(Lhs, /*AllowMixedSign=*/false) !=
          clang::ento::APSIntType::RTR_Within ||
      Signed.testInRange(Rhs, /*AllowMixedSign=*/false) !=
          clang::ento::APSIntType::RTR_Within;

  return NeedsExtraBitToPreserveSigness ? Signed : ExtendedSigned;
}

/// Evaluates the plus/minus operator of two integers.
/// Uses a wide enough bit width to preserve the values of the operands and the
/// result as well. The returned integer will be a large enough **signed**
/// integer. Note that the bit width of the result can be larger then the
/// minimal bit width of the stored value.
template <typename Operator>
static std::enable_if_t<std::is_same<Operator, std::plus<>>::value ||
                            std::is_same<Operator, std::minus<>>::value,
                        APSInt>
applyOperatorWithoutWrapping(APSInt Lhs, Operator Op, APSInt Rhs) {
  unsigned BitWidth = commonSignedTypeToFit(Lhs, Rhs).getBitWidth();
  // Use a greater type to prevent overflow/underflow.
  APSIntType CommonType(BitWidth + 1, /*Unsigned=*/false);
  CommonType.apply(Lhs);
  CommonType.apply(Rhs);
  return Op(Lhs, Rhs);
}

/// Returns the absolut value of the integer. It handles the case when the value
/// is MIN, whose negated likely to not have a positive representation. The
/// returned integer will be a large enough **signed** integer. Note that the
/// bit width of the result can be larger then the minimal bit width of the
/// stored value.
static APSInt absWithoutWrapping(const llvm::APSInt &Value) {
  // If unsigned, we might need a sign bit.
  // If the value is MIN, then we can not simply take the abs, we need another
  // extra bit.
  APSIntType ExtendedSigned(Value.getBitWidth() + 2, /*Unsigned=*/false);
  return APSInt(ExtendedSigned.convert(Value).abs(),
                /*isUnsigned=*/false);
}

// FIXME: Eventually replace RegionRawOffset with this class.
class RegionRawOffsetV2 {
private:
  const SubRegion *BaseRegion = nullptr;
  SVal ByteOffset = UnknownVal();

  RegionRawOffsetV2() = default;

  /// Compute a raw byte offset from a base region.
  /// Flatten the nested ElementRegion structure into a byte-offset in a
  /// SubRegion. E.g:
  ///   dereferenced location:
  ///     &Element{Element{Sym{reg_p}, conj{n, int}, char}, 3, int}
  /// Finds the base region:
  ///   Region: Sym{reg_p}
  /// Builds the corresponding symbolic offset expression:
  ///   Offset: SymIntExpr{conj{n, int}, +, 12, long long}
  class RawOffsetCalculator final
      : public MemRegionVisitor<RawOffsetCalculator, RegionRawOffsetV2> {
    ProgramStateRef State;
    SValBuilder &SVB;

    RegionRawOffsetV2 Leaf(const SubRegion *R) const {
      return {R, SVB.makeArrayIndex(0)};
    }

  public:
    RawOffsetCalculator(ProgramStateRef State, SValBuilder &SVB)
        : State(State), SVB(SVB) {}
    using MemRegionVisitor::Visit;

    auto VisitMemRegion(const MemRegion *R) {
      return Leaf(dyn_cast<SubRegion>(R));
    }

    RegionRawOffsetV2 VisitElementRegion(const ElementRegion *ER) {
      // For: Elem{SuperReg, ElemTy, ElemIdx}
      // 1) Calculate the raw offset of the SuperReg.
      // 2) Handle the current level.
      //    Offset := Offset + sizeof(ElemTy) * ElemIdx
      const RegionRawOffsetV2 RawOffset = Visit(ER->getSuperRegion());

      const QualType ElemTy = ER->getElementType();
      const NonLoc Index = ER->getIndex();

      // If we can not calculate the sizeof ElemTy, erase result and give up.
      if (ElemTy->isIncompleteType())
        return Leaf(nullptr);

      const NonLoc SizeofElemTy = SVB.makeArrayIndex(
          SVB.getContext().getTypeSizeInChars(ElemTy).getQuantity());

      const QualType ArrayIndexTy = SVB.getArrayIndexType();
      const NonLoc ByteElementOffset =
          SVB.evalBinOpNN(State, BO_Mul, Index, SizeofElemTy, ArrayIndexTy)
              .castAs<NonLoc>();

      SVal NewByteOffset = SVB.evalBinOpNN(
          State, BO_Add, RawOffset.getByteOffset().castAs<NonLoc>(),
          ByteElementOffset, ArrayIndexTy);
      return {RawOffset.getRegion(), NewByteOffset};
    }
  };

public:
  RegionRawOffsetV2(const SubRegion *BaseRegion, SVal ByteOffset)
      : BaseRegion(BaseRegion), ByteOffset(ByteOffset) {}

  NonLoc getByteOffset() const { return ByteOffset.castAs<NonLoc>(); }
  const SubRegion *getRegion() const { return BaseRegion; }

  static RegionRawOffsetV2 computeOffset(ProgramStateRef State,
                                         SValBuilder &SVB,
                                         loc::MemRegionVal Location) {
    return RawOffsetCalculator(State, SVB).Visit(Location.getRegion());
  }

  void dump() const;
  void dumpToStream(raw_ostream &os) const;
};

/// Check if the pointer dereference would access (read or write) a memory
/// region outside of an array.
/// It has much smarter logic dealing with constant addition and multiplication
/// in the indexer expression, such as: For 'buf[x+3]' it would check if 'x' is
/// either **below** '-3' or 'x' is greater then or equal to 'extentOf(buf)' to
/// emit a bug report. Otherwise, we will assume that the indexer expression
/// **in bound of** the valid range.
class ArrayBoundCheckerV2 : public Checker<check::Location> {
  mutable std::unique_ptr<BuiltinBug> BT;

  enum OOB_Kind { OOB_Precedes, OOB_Excedes, OOB_Tainted };

  void reportOOB(CheckerContext &C, ProgramStateRef errorState, OOB_Kind kind,
                 std::unique_ptr<BugReporterVisitor> Visitor = nullptr) const;

  // Returns null state if reported bug, non-null otherwise.
  ProgramStateRef checkLowerBound(CheckerContext &Ctx, SValBuilder &SVB,
                                  ProgramStateRef State,
                                  const APSInt &InclusiveLowerBound,
                                  NonLoc RootExpr) const;

  // Returns null state if reported bug, non-null otherwise.
  ProgramStateRef checkUpperBound(CheckerContext &Ctx, SValBuilder &SVB,
                                  ProgramStateRef State, NonLoc RootExpr,
                                  const APSInt &ExclusiveUpperBound) const;

public:
  void checkLocation(SVal l, bool isLoad, const Stmt *S,
                     CheckerContext &Ctx) const;
};

/// Peel of SymIntExprs and IntSymExprs one by one while folding the constants
/// into the LowerBound and UpperBound.
/// Both bounds treated as signed integers of large enough bit width to prevent
/// accidental overflow/underflow during simplification.
///
/// This rearrangement of the inequality holds only if the symbolic expression
/// did not overflow/underflow. We enforce this assumption by having a State
/// which extended during each step of the simplification with the necessary
/// constraint to prove this.
/// This is a heuristic to make this rearrangement (simplification) process
/// valid.
///
/// SimplificationFailed represents if an aforementioned constraint is proven to
/// be unsatisfiable, in other words the symbolic expression (RootSymbol) must
/// wrap to satisfy the inequality.
class BestEffortSymExprSimplifier final
    : public SymExprVisitor<BestEffortSymExprSimplifier> {
  friend class SymExprVisitor<BestEffortSymExprSimplifier>;
  /// The aggregated state which has all the constraints made by the heuristic.
  ProgramStateRef LastValidState;
  SValBuilder &SVB;
  SymbolRef RootSymbol; /// The symbol we want to simplify.
  /// The inclusive lower bound (signed integer of large enough bit width).
  APSInt LowerBound;
  /// The exclusive upper bound (signed integer of large enough bit width).
  APSInt UpperBound;
  bool SimplificationFailed = false;

public:
  BestEffortSymExprSimplifier(ProgramStateRef State, SValBuilder &SVB,
                              const APSInt &InclusiveLowerBound,
                              SymbolRef RootSymbol,
                              const APSInt &ExclusiveUpperBound)
      : LastValidState(std::move(State)), SVB(SVB), RootSymbol(RootSymbol) {
    assert(LastValidState);

    // Store InclusiveLowerBound as a signed value, extend the bit width if
    // necessary to preserve the value.
    const unsigned LowNeededBitWidth =
        InclusiveLowerBound.getBitWidth() +
        (representableBySigned(InclusiveLowerBound) ? 0 : 1);
    LowerBound = APSIntType(LowNeededBitWidth, /*Unsigned=*/false)
                     .convert(InclusiveLowerBound);

    // Do the same for the ExclusiveUpperBound.
    const unsigned HighNeededBitWidth =
        ExclusiveUpperBound.getBitWidth() +
        (representableBySigned(ExclusiveUpperBound) ? 0 : 1);
    UpperBound = APSIntType(HighNeededBitWidth, /*Unsigned=*/false)
                     .convert(ExclusiveUpperBound);
  }

  struct SimplificationResult {
    ProgramStateRef State;
    APSInt InclusiveLowerBound;
    APSInt ExclusiveUpperBound;
    NonLoc RootSymbol;
    bool SimplificationFailed;
  };

  /// Starts the simplification process.
  SimplificationResult Simplify() {
    LLVM_DEBUG(llvm::dbgs() << __func__ << ": initially: '";
               ConcreteInt(LowerBound).dumpToStream(llvm::dbgs());
               llvm::dbgs() << " <= " << RootSymbol << " < ";
               ConcreteInt(UpperBound).dumpToStream(llvm::dbgs());
               llvm::dbgs() << "'\n";);
    SymExprVisitor<BestEffortSymExprSimplifier>::Visit(RootSymbol);
    assert(LastValidState);
    LLVM_DEBUG(llvm::dbgs()
                   << __func__ << ": simplification "
                   << (SimplificationFailed ? "failed" : "succeeded") << ": '";
               ConcreteInt(LowerBound).dumpToStream(llvm::dbgs());
               llvm::dbgs() << " <= " << RootSymbol << " < ";
               ConcreteInt(UpperBound).dumpToStream(llvm::dbgs());
               llvm::dbgs() << "'\nLastValidState: ";
               LastValidState->printJson(llvm::dbgs()); llvm::dbgs() << '\n';);
    return {std::move(LastValidState), std::move(LowerBound),
            std::move(UpperBound), SymbolVal(RootSymbol), SimplificationFailed};
  }

private:
  void VisitSymIntExpr(const SymIntExpr *E) {
    assert(!SimplificationFailed);

    switch (E->getOpcode()) {
    default:
      return;
    case BO_Sub:
      return VisitSymSubIntExpr(E->getLHS(), E->getRHS());
    case BO_Add:
      return VisitSymAddIntExpr(E->getLHS(), E->getRHS());
    case BO_Mul:
      return VisitSymMulIntExpr(E->getLHS(), E->getRHS());
    }
  }

  void VisitIntSymExpr(const IntSymExpr *E) {
    return; // TODO: Handle this as well.
  }

  // Simplify: LowerBound <= sym - C < UpperBound
  // Into: LowerBound + C <= sym < UpperBound + C
  void VisitSymSubIntExpr(SymbolRef LHS, const APSInt &C) {
    assert(!SimplificationFailed);
    assert(LowerBound.isSigned());
    assert(UpperBound.isSigned());

    // Subtracting a negative number is like adding it.
    if (C.isNegative()) {
      VisitSymAddIntExpr(LHS, absWithoutWrapping(C));
      return;
    }
    assert(C.isNonNegative());

    const APSInt &SymMin =
        SVB.getBasicValueFactory().getMinValue(LHS->getType());
    // Type of SymMin can represent C, this conversion is fine.
    ConcreteInt SmallestPossibleSymbolValue =
        SVB.makeIntVal(SymMin + APSIntType(SymMin).convert(C));
    const NonLoc UnderflowCheck =
        SVB.evalBinOpNN(LastValidState, BO_GE, SymbolVal(LHS),
                        SmallestPossibleSymbolValue, SVB.getConditionType())
            .castAs<NonLoc>();
    if (ProgramStateRef NoUnderflowHappened =
            LastValidState->assume(UnderflowCheck, true)) {
      LastValidState = NoUnderflowHappened;
      LLVM_DEBUG(llvm::dbgs() << __func__ << ": assuming that '" << LHS
                              << " >= " << SmallestPossibleSymbolValue
                              << "' holds (aka. 'sym - C' don't underflow)\n";);
    } else {
      LLVM_DEBUG(llvm::dbgs() << __func__ << ": '" << LHS
                              << " >= " << SmallestPossibleSymbolValue
                              << "' doesn't hold, could not simplify "
                                 ", simplification stopped\n";);
      SimplificationFailed = true;
      return;
    }

    // Otherwise safe to do the folding, and continue the simplification.
    LowerBound = applyOperatorWithoutWrapping(LowerBound, std::plus<>(), C);
    UpperBound = applyOperatorWithoutWrapping(UpperBound, std::plus<>(), C);
    RootSymbol = LHS;
    LLVM_DEBUG(llvm::dbgs() << __func__ << ": '";
               ConcreteInt(LowerBound).dumpToStream(llvm::dbgs());
               llvm::dbgs() << " <= " << RootSymbol << " < ";
               ConcreteInt(UpperBound).dumpToStream(llvm::dbgs());
               llvm::dbgs() << "'\n";);
    Visit(RootSymbol); // Continue the simplification.
  }

  // Simplify: LowerBound <= sym + C < UpperBound
  // Into: LowerBound - C <= sym < UpperBound - C
  void VisitSymAddIntExpr(SymbolRef LHS, const APSInt &C) {
    assert(!SimplificationFailed);
    assert(LowerBound.isSigned());
    assert(UpperBound.isSigned());

    // Adding a negative number is like subtracting it.
    if (C.isNegative()) {
      VisitSymSubIntExpr(LHS, absWithoutWrapping(C));
      return;
    }
    assert(C.isNonNegative());

    const APSInt &SymMax =
        SVB.getBasicValueFactory().getMaxValue(LHS->getType());

    // Type of SymMax can represent C, this conversion is fine.
    ConcreteInt GreatestPossibleSymbolValue =
        SVB.makeIntVal(SymMax - APSIntType(SymMax).convert(C));
    const NonLoc OverflowCheck =
        SVB.evalBinOpNN(LastValidState, BO_LE, SymbolVal(LHS),
                        GreatestPossibleSymbolValue, SVB.getConditionType())
            .castAs<NonLoc>();
    if (ProgramStateRef NoOverflowHappened =
            LastValidState->assume(OverflowCheck, true)) {
      LastValidState = NoOverflowHappened;
      LLVM_DEBUG(llvm::dbgs() << __func__ << ": assuming that '" << LHS
                              << " <= " << GreatestPossibleSymbolValue
                              << "' holds (aka. 'sym + C' don't overflow)\n";);
    } else {
      LLVM_DEBUG(llvm::dbgs() << __func__ << ": '" << LHS
                              << " <= " << GreatestPossibleSymbolValue
                              << "' doesn't hold, could not simplify "
                                 ", simplification stopped\n";);
      SimplificationFailed = true;
      return;
    }

    // Otherwise safe to do the folding, and continue the simplification.
    LowerBound = applyOperatorWithoutWrapping(LowerBound, std::minus<>(), C);
    UpperBound = applyOperatorWithoutWrapping(UpperBound, std::minus<>(), C);
    RootSymbol = LHS;
    LLVM_DEBUG(llvm::dbgs() << __func__ << ": '";
               ConcreteInt(LowerBound).dumpToStream(llvm::dbgs());
               llvm::dbgs() << " <= " << RootSymbol << " < ";
               ConcreteInt(UpperBound).dumpToStream(llvm::dbgs());
               llvm::dbgs() << "'\n";);
    Visit(RootSymbol); // Continue the simplification.
  }

  // Simplify: LowerBound <= sym * C < UpperBound
  // Into: floor(LowerBound / C) <= sym < ceil(UpperBound - C)
  void VisitSymMulIntExpr(SymbolRef LHS, const APSInt &C) {
    assert(!SimplificationFailed);
    assert(LowerBound.isSigned());
    assert(UpperBound.isSigned());

    const QualType SymTy = LHS->getType();
    const APSInt &SymMin = SVB.getBasicValueFactory().getMinValue(SymTy);
    const APSInt &SymMax = SVB.getBasicValueFactory().getMaxValue(SymTy);
    assert(!C.isNullValue() && "How can it be zero?");

    // Check if 'sym * C' overflows.
    // Type of SymMax can represent C, this conversion is fine.
    ConcreteInt GreatestPossibleSymbolValue =
        SVB.makeIntVal(SymMax / APSIntType(SymMax).convert(C));
    const NonLoc NoOverflowCheck =
        SVB.evalBinOpNN(LastValidState, BO_LE, SymbolVal(LHS),
                        GreatestPossibleSymbolValue, SVB.getConditionType())
            .castAs<NonLoc>();
    if (ProgramStateRef NoOverflowHappened =
            LastValidState->assume(NoOverflowCheck, true)) {
      LastValidState = NoOverflowHappened;
      LLVM_DEBUG(llvm::dbgs() << __func__ << ": assuming that '" << LHS
                              << " <= " << GreatestPossibleSymbolValue
                              << "' holds (aka. 'sym * C' don't overflow)\n";
                 llvm::dbgs() << "LastValidState: ";
                 LastValidState->printJson(llvm::dbgs());
                 llvm::dbgs() << '\n';);
    } else {
      LLVM_DEBUG(llvm::dbgs() << __func__ << ": '" << LHS
                              << " <= " << GreatestPossibleSymbolValue
                              << "' doesn't hold, could not simplify "
                                 ", simplification stopped\n";);
      SimplificationFailed = true;
      return;
    }

    // Check if 'sym * C' underflows.
    // Type of SymMin can represent C, this conversion is fine.
    ConcreteInt SmallestPossibleSymbolValue =
        SVB.makeIntVal(SymMin / APSIntType(SymMin).convert(C));
    const NonLoc UnderflowCheck =
        SVB.evalBinOpNN(LastValidState, BO_GE, SymbolVal(LHS),
                        SmallestPossibleSymbolValue, SVB.getConditionType())
            .castAs<NonLoc>();
    if (ProgramStateRef NoUnderflowHappened =
            LastValidState->assume(UnderflowCheck, true)) {
      LastValidState = NoUnderflowHappened;
      LLVM_DEBUG(llvm::dbgs() << __func__ << ": assuming that '" << LHS
                              << " >= " << SmallestPossibleSymbolValue
                              << "' holds (aka. 'sym * C' don't underflow)\n";);
    } else {
      LLVM_DEBUG(llvm::dbgs() << __func__ << ": '" << LHS
                              << " >= " << SmallestPossibleSymbolValue
                              << "' doesn't hold, could not simplify "
                                 ", simplification stopped\n";);
      SimplificationFailed = true;
      return;
    }

    const auto Floor = [](const APSInt &X, const APSInt &Y) -> APSInt {
      return X / Y;
    };

    const auto Ceil = [](const APSInt &X, const APSInt &Y) -> APSInt {
      APSInt Quotient, Remainder; // Default constructs signed integers.
      llvm::APInt::sdivrem(X, Y, Quotient, Remainder);
      if (Remainder.isNullValue())
        return Quotient;
      APSInt One = (APSInt(Quotient.getBitWidth(), false) = 1);
      return Quotient + One;
    };

    // Fold the constants carefully.
    APSIntType LowerTy = commonSignedTypeToFit(LowerBound, C);
    APSIntType LowerTy2x = APSIntType(LowerTy.getBitWidth() * 2, false);
    LowerBound = Floor(LowerTy2x.convert(LowerBound), LowerTy2x.convert(C));

    APSIntType UpperTy = commonSignedTypeToFit(UpperBound, C);
    APSIntType UpperTy2x = APSIntType(UpperTy.getBitWidth() * 2, false);
    UpperBound = Ceil(UpperTy2x.convert(UpperBound), UpperTy2x.convert(C));

    RootSymbol = LHS;
    LLVM_DEBUG(llvm::dbgs() << __func__ << ": '";
               ConcreteInt(LowerBound).dumpToStream(llvm::dbgs());
               llvm::dbgs() << " <= " << RootSymbol << " < ";
               ConcreteInt(UpperBound).dumpToStream(llvm::dbgs());
               llvm::dbgs() << "'\n";);
    Visit(RootSymbol); // Continue the simplification.
  }
}; // class BestEffortSymExprSimplifier

using SimplificationResult = BestEffortSymExprSimplifier::SimplificationResult;

SimplificationResult simplify(ProgramStateRef State, SValBuilder &SVB,
                              const APSInt &InclusiveLowerBound,
                              NonLoc RootExpr,
                              const APSInt &ExclusiveUpperBound) {
  if (const auto SymbolicRoot = RootExpr.getAs<SymbolVal>()) {
    return BestEffortSymExprSimplifier(State, SVB, InclusiveLowerBound,
                                       SymbolicRoot->getSymbol(),
                                       ExclusiveUpperBound)
        .Simplify();
  }
  assert(RootExpr.getAs<ConcreteInt>() &&
         "Root must be either SymbolVal or a ConcreteInt.");
  return {State, InclusiveLowerBound, ExclusiveUpperBound, RootExpr, false};
}
} // namespace

ProgramStateRef ArrayBoundCheckerV2::checkLowerBound(
    CheckerContext &Ctx, SValBuilder &SVB, ProgramStateRef State,
    const APSInt &InclusiveLowerBound, NonLoc RootExpr) const {
  if (const auto Index = RootExpr.getAs<ConcreteInt>()) {
    APSIntType STy =
        commonSignedTypeToFit(Index->getValue(), InclusiveLowerBound);

    // If we index below the first valid index, report.
    if (STy.convert(Index->getValue()) < STy.convert(InclusiveLowerBound)) {
      reportOOB(Ctx, State, OOB_Precedes);
      return nullptr;
    }
    return State;
  }

  const bool IsRootTyUnsigned = RootExpr.castAs<SymbolVal>()
                                    .getSymbol()
                                    ->getType()
                                    ->isUnsignedIntegerOrEnumerationType();
  // Unsigned expression is always greater then any negative value.
  // We need this check before the evalBinOp call, since that would potentially
  // promote the negative value to a huge unsigned value before the actual
  // comparison would happen.
  if (IsRootTyUnsigned && InclusiveLowerBound.isNegative())
    return State;

  NonLoc LowerBoundCheck = SVB.evalBinOpNN(State, BO_LT, RootExpr,
                                           SVB.makeIntVal(InclusiveLowerBound),
                                           SVB.getConditionType())
                               .castAs<NonLoc>();

  ProgramStateRef PrecedesLowerBound, WithinLowerBound;
  std::tie(PrecedesLowerBound, WithinLowerBound) =
      State->assume(LowerBoundCheck);

  if (PrecedesLowerBound && !WithinLowerBound) {
    reportOOB(Ctx, PrecedesLowerBound, OOB_Precedes);
    return nullptr;
  }

  // Otherwise, assume the constraint of the lower bound.
  assert(WithinLowerBound);
  LLVM_DEBUG(llvm::dbgs() << __func__ << ": State at the end: ";
             WithinLowerBound->printJson(llvm::dbgs()); llvm::dbgs() << '\n';);
  return WithinLowerBound;
}

// Returns null state if reported bug, non-null otherwise.
ProgramStateRef
ArrayBoundCheckerV2::checkUpperBound(CheckerContext &Ctx, SValBuilder &SVB,
                                     ProgramStateRef State, NonLoc RootExpr,
                                     const APSInt &ExclusiveUpperBound) const {
  if (const auto Index = RootExpr.getAs<ConcreteInt>()) {
    APSIntType STy =
        commonSignedTypeToFit(Index->getValue(), ExclusiveUpperBound);

    // If we index above the last valid index, report.
    if (STy.convert(Index->getValue()) >= STy.convert(ExclusiveUpperBound)) {
      reportOOB(Ctx, State, OOB_Excedes);
      return nullptr;
    }
    return State;
  }

  const bool IsRootTyUnsigned = RootExpr.castAs<SymbolVal>()
                                    .getSymbol()
                                    ->getType()
                                    ->isUnsignedIntegerType();
  // Unsigned expression is always greater then any non-positive value.
  // We need this check before the evalBinOp call, since that would potentially
  // promote the negative value to a huge unsigned value before the actual
  // comparison would happen.
  if (IsRootTyUnsigned && ExclusiveUpperBound.isNonPositive()) {
    reportOOB(Ctx, State, OOB_Excedes);
    return nullptr;
  }

  NonLoc UpperBoundCheck = SVB.evalBinOpNN(State, BO_GE, RootExpr,
                                           SVB.makeIntVal(ExclusiveUpperBound),
                                           SVB.getConditionType())
                               .castAs<NonLoc>();

  ProgramStateRef ExceedsUpperBound, WithinUpperBound;
  std::tie(ExceedsUpperBound, WithinUpperBound) =
      State->assume(UpperBoundCheck);

  // If we are under constrained and the index variables are tainted, report.
  if (ExceedsUpperBound && WithinUpperBound) {
    if (isTainted(State, RootExpr)) {
      reportOOB(Ctx, ExceedsUpperBound, OOB_Tainted,
                std::make_unique<TaintBugVisitor>(RootExpr));
      return nullptr;
    }
  } else if (ExceedsUpperBound) {
    // If we are constrained enough to definitely exceed the upper bound,
    // report.
    assert(!WithinUpperBound);
    reportOOB(Ctx, ExceedsUpperBound, OOB_Excedes);
    return nullptr;
  }

  assert(WithinUpperBound);
  LLVM_DEBUG(llvm::dbgs() << __func__ << ": State at the end: ";
             WithinUpperBound->printJson(llvm::dbgs()); llvm::dbgs() << '\n';);
  return WithinUpperBound;
}

void ArrayBoundCheckerV2::checkLocation(SVal Location, bool, const Stmt *,
                                        CheckerContext &Ctx) const {
  // NOTE: Instead of using ProgramState::assumeInBound(), we are prototyping
  // some new logic here that reasons directly about memory region extents.
  // Once that logic is more mature, we can bring it back to assumeInBound()
  // for all clients to use.
  //
  // The algorithm we are using here for bounds checking is to see if the
  // memory access is within the extent of the base region.  Since we
  // have some flexibility in defining the base region, we can achieve
  // various levels of conservatism in our buffer overflow checking.
  // TODO: Update the comment above.
  LLVM_DEBUG(llvm::dbgs() << __func__ << ": Called with Location: " << Location
                          << '\n';);
  ProgramStateRef State = Ctx.getState();
  SValBuilder &SVB = Ctx.getSValBuilder();
  const RegionRawOffsetV2 RawOffset = RegionRawOffsetV2::computeOffset(
      State, SVB, Location.castAs<loc::MemRegionVal>());
  assert(RawOffset.getRegion() && "It should be a valid region.");

  // If we don't know that the pointer points to the beginning of the region,
  // skip check.
  if (isa<UnknownSpaceRegion>(RawOffset.getRegion()->getMemorySpace()))
    return;

  const ConcreteInt Zero = SVB.makeZeroArrayIndex().castAs<ConcreteInt>();

  const llvm::APSInt *ExtentValue = SVB.getKnownValue(
      State, getDynamicSize(State, RawOffset.getRegion(), SVB));

  // If we could not get the concrete extent, check only the lower bound.
  // Use a dummy upper bound just to be able to do simplification.
  const bool CheckUpperBoundToo = ExtentValue != nullptr;
  const APSInt &ExclusiveUpperBound =
      ExtentValue ? *ExtentValue : Zero.getValue();

  SimplificationResult Simplified =
      simplify(State, SVB, Zero.getValue(), RawOffset.getByteOffset(),
               ExclusiveUpperBound);
  if (Simplified.SimplificationFailed)
    return;

  // Save the collected assumptions about overflow/underflow.
  State = Simplified.State;
  State = checkLowerBound(Ctx, SVB, State, Simplified.InclusiveLowerBound,
                          Simplified.RootSymbol);
  if (!State)
    return;

  if (CheckUpperBoundToo) {
    State = checkUpperBound(Ctx, SVB, State, Simplified.RootSymbol,
                            Simplified.ExclusiveUpperBound);
    if (!State)
      return;
  }

  // Continue the analysis while we know that this access must be valid.
  Ctx.addTransition(State);
}

void ArrayBoundCheckerV2::reportOOB(
    CheckerContext &checkerContext, ProgramStateRef errorState, OOB_Kind kind,
    std::unique_ptr<BugReporterVisitor> Visitor) const {

  ExplodedNode *errorNode = checkerContext.generateErrorNode(errorState);
  if (!errorNode)
    return;

  if (!BT)
    BT = std::make_unique<BuiltinBug>(this, "Out-of-bound access");

  // FIXME: This diagnostics are preliminary.  We should get far better
  // diagnostics for explaining buffer overruns.

  SmallString<128> buf;
  llvm::raw_svector_ostream os(buf);
  os << "Out of bound memory access ";
  switch (kind) {
  case OOB_Precedes:
    os << "(accessed memory precedes memory block)";
    break;
  case OOB_Excedes:
    os << "(access exceeds upper limit of memory block)";
    break;
  case OOB_Tainted:
    os << "(index is tainted)";
    break;
  }

  auto BR = std::make_unique<PathSensitiveBugReport>(*BT, os.str(), errorNode);
  BR->addVisitor(std::move(Visitor));
  checkerContext.emitReport(std::move(BR));
}

#ifndef NDEBUG
LLVM_DUMP_METHOD void RegionRawOffsetV2::dump() const {
  dumpToStream(llvm::errs());
}

void RegionRawOffsetV2::dumpToStream(raw_ostream &os) const {
  os << "raw_offset_v2{" << getRegion() << ',' << getByteOffset() << '}';
}
#endif

void ento::registerArrayBoundCheckerV2(CheckerManager &mgr) {
  mgr.registerChecker<ArrayBoundCheckerV2>();
}

bool ento::shouldRegisterArrayBoundCheckerV2(const CheckerManager &mgr) {
  return true;
}
