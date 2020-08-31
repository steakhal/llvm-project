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
#include "clang/AST/CharUnits.h"
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
#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace ento;
using namespace taint;
using ConcreteInt = nonloc::ConcreteInt;
using SymbolVal = nonloc::SymbolVal;

namespace {
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
                                  const ProgramStateRef State,
                                  RegionRawOffsetV2 RawOffset) const;

  // Returns null state if reported bug, non-null otherwise.
  ProgramStateRef checkUpperBound(CheckerContext &Ctx, SValBuilder &SVB,
                                  const ProgramStateRef State,
                                  RegionRawOffsetV2 RawOffset) const;

public:
  void checkLocation(SVal l, bool isLoad, const Stmt *S,
                     CheckerContext &Ctx) const;
};

std::pair<NonLoc, ConcreteInt> simplify(SValBuilder &SVB, NonLoc Root,
                                        ConcreteInt Index) {
  /// Peel of SymIntExprs one by one while folding the constants on the right.
  ///
  /// It reorders the expression `Root` and `Index` at the same time.
  /// Eg: if `Root` is:
  ///   `x + 5` then `Index := Index - (typeof(Index))5`
  ///   `x * 5` then `Index := Index / (typeof(Index))5`
  /// Then recurse on `x`.
  class SymExprSimplifier final : public SymExprVisitor<SymExprSimplifier> {
    SValBuilder &SVB;
    const SymExpr *RootSymbol;
    ConcreteInt Index;

  public:
    SymExprSimplifier(SValBuilder &SVB, const SymExpr *RootSymbol,
                      ConcreteInt Index)
        : SVB(SVB), RootSymbol(RootSymbol), Index(Index) {}
    using SymExprVisitor::Visit;

    void VisitSymIntExpr(const SymIntExpr *E) {
      const BinaryOperator::Opcode Op = E->getOpcode();
      if (Op != BO_Add && Op != BO_Mul)
        return;

      llvm::APSInt RHSConstant =
          APSIntType(Index.getValue()).convert(E->getRHS());

      if (Op == BO_Add) {
        Index = SVB.makeIntVal(Index.getValue() - RHSConstant);
        RootSymbol = E->getLHS();
        Visit(RootSymbol);
        return;
      }

      assert(Op == BO_Mul);

      // The constant should never be 0 here, since it the result of scaling
      // based on the size of a type which is never 0.
      assert(!RHSConstant.isNullValue());

      // If the NewExtent is not divisible, we can not further simplify
      // expressions.
      if ((Index.getValue() % RHSConstant) != 0)
        return;

      Index = SVB.makeIntVal(Index.getValue() / RHSConstant);
      RootSymbol = E->getLHS();
      Visit(RootSymbol);
    }

    SymbolVal getRootSymbol() const { return RootSymbol; }
    ConcreteInt getFoldedIndex() const { return Index; }
  };
  if (const auto SymbolicRoot = Root.getAs<SymbolVal>()) {
    SymExprSimplifier Visitor(SVB, SymbolicRoot->getSymbol(), Index);
    Visitor.Visit(SymbolicRoot->getSymbol());
    return {Visitor.getRootSymbol(), Visitor.getFoldedIndex()};
  }
  assert(Root.getAs<ConcreteInt>() && "Root must be either int or symbol.");
  return {Root, Index};
}
} // namespace

ProgramStateRef
ArrayBoundCheckerV2::checkLowerBound(CheckerContext &Ctx, SValBuilder &SVB,
                                     const ProgramStateRef State,
                                     RegionRawOffsetV2 RawOffset) const {
  // If we don't know that the pointer points to the beginning of the region,
  // skip lower-bound check.
  if (isa<UnknownSpaceRegion>(RawOffset.getRegion()->getMemorySpace()))
    return State;

  ConcreteInt Zero = SVB.makeZeroArrayIndex().castAs<ConcreteInt>();
  SVal RootNonLoc;
  SVal ConstantFoldedRHS;
  std::tie(RootNonLoc, ConstantFoldedRHS) =
      simplify(SVB, RawOffset.getByteOffset(), Zero);

  // No unsigned symbolic value can be less then a negative constant.
  if (const auto SymbolicRoot = RootNonLoc.getAs<SymbolVal>())
    if (SymbolicRoot->getSymbol()->getType()->isUnsignedIntegerType() &&
        ConstantFoldedRHS.castAs<ConcreteInt>().getValue().isNegative())
      return State;

  NonLoc LowerBoundCheck =
      SVB.evalBinOpNN(State, BO_LT, RootNonLoc.castAs<NonLoc>(),
                      ConstantFoldedRHS.castAs<ConcreteInt>(),
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
  return WithinLowerBound;
}

// Returns null state if reported bug, non-null otherwise.
ProgramStateRef
ArrayBoundCheckerV2::checkUpperBound(CheckerContext &Ctx, SValBuilder &SVB,
                                     const ProgramStateRef State,
                                     RegionRawOffsetV2 RawOffset) const {
  DefinedOrUnknownSVal Extent =
      getDynamicSize(State, RawOffset.getRegion(), SVB);

  if (!Extent.getAs<NonLoc>())
    return State;

  NonLoc RawByteOffset = RawOffset.getByteOffset();
  if (const auto ExtentInt = Extent.getAs<ConcreteInt>()) {
    std::tie(RawByteOffset, Extent) =
        simplify(SVB, RawOffset.getByteOffset(), *ExtentInt);
  }

  NonLoc UpperBoundCheck =
      SVB.evalBinOpNN(State, BO_GE, RawByteOffset, Extent.castAs<NonLoc>(),
                      SVB.getConditionType())
          .castAs<NonLoc>();

  ProgramStateRef ExceedsUpperBound, WithinUpperBound;
  std::tie(ExceedsUpperBound, WithinUpperBound) =
      State->assume(UpperBoundCheck);

  // If we are under constrained and the index variables are tainted, report.
  if (ExceedsUpperBound && WithinUpperBound) {
    SVal ByteOffset = RawOffset.getByteOffset();
    if (isTainted(State, ByteOffset)) {
      reportOOB(Ctx, ExceedsUpperBound, OOB_Tainted,
                std::make_unique<TaintBugVisitor>(ByteOffset));
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
  // TODO: Check if these comments are still valid:
  //  - Why would anyone use this 'experimental logic' if it does not handle
  //  overflows?
  //  - "we can achieve various levels of conservatism" What?
  ProgramStateRef State = Ctx.getState();
  SValBuilder &SVB = Ctx.getSValBuilder();
  const RegionRawOffsetV2 RawOffset = RegionRawOffsetV2::computeOffset(
      State, SVB, Location.castAs<loc::MemRegionVal>());
  assert(RawOffset.getRegion() && "It should be a valid region.");

  // Is byteOffset < extent begin?
  // If so, we are doing a load/store before the first valid offset in the
  // memory region.
  State = checkLowerBound(Ctx, SVB, State, RawOffset);
  if (!State)
    return;

  // Is byteOffset >= extentOf(BaseRegion)?
  // If so, we are doing a load/store after the last valid offset.
  State = checkUpperBound(Ctx, SVB, State, RawOffset);
  if (!State)
    return;

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
    BT.reset(new BuiltinBug(this, "Out-of-bound access"));

  // FIXME: This diagnostics are preliminary.  We should get far better
  // diagnostics for explaining buffer overruns.

  SmallString<256> buf;
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
