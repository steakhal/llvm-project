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
using APSInt = llvm::APSInt;

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

  LLVM_DUMP_METHOD void dump() const { dumpToStream(llvm::errs()); }

  void dumpToStream(raw_ostream &os) const {
    os << "raw_offset_v2{" << getRegion() << ',' << getByteOffset() << '}';
  }
};

namespace {
class ArrayBoundCheckerV2 : public Checker<check::Location> {
  mutable std::unique_ptr<BuiltinBug> BT;

  enum OOB_Kind { OOB_Precedes, OOB_Excedes, OOB_Tainted };

  void reportOOB(CheckerContext &C, ProgramStateRef errorState, OOB_Kind kind,
                 std::unique_ptr<BugReporterVisitor> Visitor = nullptr) const;

  // Returns null state if reported bug, non-null otherwise.
  ProgramStateRef checkLowerBound(CheckerContext &Ctx, SValBuilder &SVB,
                                  ProgramStateRef State,
                                  RegionRawOffsetV2 RawOffset) const;

  // Returns null state if reported bug, non-null otherwise.
  ProgramStateRef checkUpperBound(CheckerContext &Ctx, SValBuilder &SVB,
                                  ProgramStateRef State,
                                  RegionRawOffsetV2 RawOffset) const;

public:
  void checkLocation(SVal l, bool isLoad, const Stmt*S,
                     CheckerContext &C) const;
};

class Simplifier final : public SymExprVisitor<Simplifier> {
  ProgramStateRef LastValidState;
  SValBuilder &SVB;
  SymbolRef RootSymbol;
  APSInt FoldedConstant;
  const APSInt FoldedConstantMin =
      SVB.getBasicValueFactory().getMinValue(FoldedConstant);
  const APSInt FoldedConstantMax =
      SVB.getBasicValueFactory().getMaxValue(FoldedConstant);
  bool SimplificationFailed = false;

public:
  Simplifier(ProgramStateRef State, SValBuilder &SVB, SymbolRef RootSymbol,
             ConcreteInt Constant)
      : LastValidState(std::move(State)), SVB(SVB), RootSymbol(RootSymbol),
        FoldedConstant(Constant.getValue()) {
    assert(LastValidState);
  }
  using SymExprVisitor::Visit;

  void VisitSymIntExpr(const SymIntExpr *E) {
    assert(!SimplificationFailed);

    switch (E->getOpcode()) {
    default:
      return;
    case BO_Sub:
      return VisitSymSubIntExpr(E);
    case BO_Add:
      return VisitSymAddIntExpr(E);
    case BO_Mul:
      return VisitSymMulIntExpr(E);
    }
  }

  void VisitIntSymExpr(const IntSymExpr *E) {
    assert("Not yet implemented since the canonical representation is "
           "SymIntExpr based.");
  }

  ProgramStateRef getLastValidState() const {
    assert(LastValidState); // Post-cond
    return LastValidState;
  }
  nonloc::ConcreteInt getFoldedConstant() const {
    return nonloc::ConcreteInt(FoldedConstant);
  }
  nonloc::SymbolVal getRootSymbol() const {
    return nonloc::SymbolVal(RootSymbol);
  }
  bool succeeded() const { return !SimplificationFailed; }

private:
  // Given the expression: sym - C <REL> C'
  // We want to transform that into: sym <REL> C'+C
  //
  // We do these steps in the following order:
  // Check if 'sym - C' underflows for sure:
  //   - YES: Stop simplification and set 'SimplificationFailed'.
  //   - No:
  //   - Maybe: Assume the constraints on 'sym' to make the expression
  //     'sym - C' impossible to underflow.
  // Check if C'+C would overflow:
  //   - YES: Stop simplification and set 'SimplificationFailed'.
  //   - NO: Fold C into C'.
  void VisitSymSubIntExpr(const SymIntExpr *E) {
    assert(!SimplificationFailed);
    assert(E->getOpcode() == BO_Sub);

    const SymbolRef LHS = E->getLHS();
    const QualType SymTy = LHS->getType();
    const APSInt &SymMin = SVB.getBasicValueFactory().getMinValue(SymTy);
    const APSInt RHSConstant = APSIntType(FoldedConstant).convert(E->getRHS());

    APSInt UpperBound = SymMin + APSIntType(SymMin).convert(RHSConstant);

    const NonLoc NoUnderflowHappensCheck =
        SVB.evalBinOpNN(LastValidState, BO_GE, nonloc::SymbolVal(LHS),
                        nonloc::ConcreteInt(UpperBound), SVB.getConditionType())
            .castAs<NonLoc>();
    if (ProgramStateRef NoUnderflowHappenedState =
            LastValidState->assume(NoUnderflowHappensCheck, true)) {
      LastValidState = NoUnderflowHappenedState;
    } else {
      // 'sym - C' underflows, we can not simplify such expression.
      SimplificationFailed = true;
      return;
    }

    if (FoldedConstantMax - RHSConstant < FoldedConstant) {
      // Could not fold the constant into the right-hand side since that would
      // have overflown.
      SimplificationFailed = true;
      return;
    }

    // Otherwise safe to do the folding, and continue the simplification.
    RootSymbol = E->getLHS();
    FoldedConstant =
        FoldedConstant + APSIntType(FoldedConstant).convert(RHSConstant);
    Visit(RootSymbol); // Continue the simplification.
  }

  // Given the expression: sym + C <REL> C'
  // We want to transform that into: sym <REL> C'-C
  //
  // We do these steps in the following order:
  // Check if 'sym + C' overflows for sure:
  //   - YES: Stop simplification and set 'SimplificationFailed'.
  //   - No:
  //   - Maybe: Assume the constraints on 'sym' to make the expression
  //     'sym + C' impossible to overflow.
  // Check if C'-C would underflow:
  //   - YES: Stop simplification and set 'SimplificationFailed'.
  //   - NO: Fold C into C'.
  void VisitSymAddIntExpr(const SymIntExpr *E) {
    assert(!SimplificationFailed);
    assert(E->getOpcode() == BO_Add);

    const SymbolRef LHS = E->getLHS();
    const QualType SymTy = LHS->getType();
    const APSInt &SymMax = SVB.getBasicValueFactory().getMaxValue(SymTy);
    const APSInt RHSConstant = APSIntType(FoldedConstant).convert(E->getRHS());

    const NonLoc NoOverflowHappensCheck =
        SVB.evalBinOpNN(LastValidState, BO_LE, nonloc::SymbolVal(LHS),
                        nonloc::ConcreteInt(
                            SymMax - APSIntType(SymMax).convert(RHSConstant)),
                        SVB.getConditionType())
            .castAs<NonLoc>();
    if (ProgramStateRef NoOverflowHappenedState =
            LastValidState->assume(NoOverflowHappensCheck, true)) {
      LastValidState = NoOverflowHappenedState;
    } else {
      // 'sym + C' overflows, we can not simplify such expression.
      SimplificationFailed = true;
      return;
    }

    if (FoldedConstantMin + RHSConstant > FoldedConstant) {
      // Could not fold the constant into the right-hand side since that would
      // have underflown.
      SimplificationFailed = true;
      return;
    }

    // Otherwise safe to do the folding, and continue the simplification.
    RootSymbol = E->getLHS();
    FoldedConstant =
        FoldedConstant - APSIntType(FoldedConstant).convert(RHSConstant);
    Visit(RootSymbol); // Continue the folding process.
  }

  // Given the expression: sym * C <REL> C'
  // We want to transform that into: sym <REL> C'/C
  //
  // We do these steps in the following order:
  // Check if 'sym * C' overflows/underflows for sure:
  //   - YES: Stop simplification and set 'SimplificationFailed'.
  //   - No:
  //   - Maybe: Assume the constraints on 'sym' to make the expression
  //     'sym * C' impossible to overflow/underflow.
  // Fold C into C'.
  void VisitSymMulIntExpr(const SymIntExpr *E) {
    assert(!SimplificationFailed);
    assert(E->getOpcode() == BO_Mul);

    const SymbolRef LHS = E->getLHS();
    const QualType SymTy = LHS->getType();
    const APSInt &SymMin = SVB.getBasicValueFactory().getMinValue(SymTy);
    const APSInt &SymMax = SVB.getBasicValueFactory().getMaxValue(SymTy);
    const APSInt RHSConstant = APSIntType(FoldedConstant).convert(E->getRHS());

    assert(!RHSConstant.isNullValue());

    // Assume that the left-hand side did not overflow, and check if the
    // right-hand side division would not truncate.
    const APSInt UpperBound = SymMax / APSIntType(SymMax).convert(RHSConstant);

    // Check if 'sym * C' overflows.
    const NonLoc NoOverflowHappensCheck =
        SVB.evalBinOpNN(LastValidState, BO_LE, nonloc::SymbolVal(LHS),
                        nonloc::ConcreteInt(UpperBound), SVB.getConditionType())
            .castAs<NonLoc>();
    if (ProgramStateRef NoOverflowHappenedState =
            LastValidState->assume(NoOverflowHappensCheck, true)) {
      LastValidState = NoOverflowHappenedState;
    } else {
      // 'sym * C' overflows, we can not simplify such expression.
      SimplificationFailed = true;
      return;
    }

    const APSInt LowerBound = SymMin / APSIntType(SymMin).convert(RHSConstant);

    // Check if 'sym * C' underflows.
    const NonLoc NoUnderflowHappensCheck =
        SVB.evalBinOpNN(LastValidState, BO_GE, nonloc::SymbolVal(LHS),
                        nonloc::ConcreteInt(LowerBound), SVB.getConditionType())
            .castAs<NonLoc>();
    if (ProgramStateRef NoUnderflowHappenedState =
            LastValidState->assume(NoUnderflowHappensCheck, true)) {
      LastValidState = NoUnderflowHappenedState;
    } else {
      // 'sym * C' underflows, we can not simplify such expression.
      SimplificationFailed = true;
      return;
    }

    // Fold the constant into the right-hand side constant. It might truncate.
    FoldedConstant =
        FoldedConstant / APSIntType(FoldedConstant).convert(RHSConstant);
    RootSymbol = E->getLHS();
    Visit(RootSymbol); // Continue the simplification.
  }
};
} // namespace

ProgramStateRef
ArrayBoundCheckerV2::checkLowerBound(CheckerContext &Ctx, SValBuilder &SVB,
                                     ProgramStateRef State,
                                     RegionRawOffsetV2 RawOffset) const {
  // If we don't know that the pointer points to the beginning of the region,
  // skip lower-bound check.
  if (isa<UnknownSpaceRegion>(RawOffset.getRegion()->getMemorySpace()))
    return State;

  const ConcreteInt Zero = SVB.makeZeroArrayIndex().castAs<ConcreteInt>();

  SVal RootSymbol;
  SVal FoldedConstant;

  if (const auto ConcreteRoot =
          RawOffset.getByteOffset().getAs<ConcreteInt>()) {
    RootSymbol = ConcreteRoot.getValue();
    FoldedConstant = Zero;
  } else if (const auto SymbolicRoot =
                 RawOffset.getByteOffset().getAs<SymbolVal>()) {
    Simplifier Visitor(State, SVB, SymbolicRoot->getSymbol(), Zero);
    Visitor.Visit(SymbolicRoot->getSymbol());

    RootSymbol = Visitor.getRootSymbol();
    FoldedConstant = Visitor.getFoldedConstant();

    // Preserve the no overflow/underflow state.
    State = Visitor.getLastValidState();
  } else {
    llvm_unreachable("hm something bad happened....");
  }

  // No unsigned symbolic value can be less then a negative constant.
  if (const auto SymbolicRoot = RootSymbol.getAs<SymbolVal>())
    if (const auto FoldedRHSAsInt = FoldedConstant.getAs<ConcreteInt>())
      if (SymbolicRoot->getSymbol()->getType()->isUnsignedIntegerType() &&
          FoldedRHSAsInt->getValue().isNegative())
        return State;

  NonLoc LowerBoundCheck =
      SVB.evalBinOpNN(State, BO_LT, RootSymbol.castAs<NonLoc>(),
                      FoldedConstant.castAs<NonLoc>(), SVB.getConditionType())
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

ProgramStateRef
ArrayBoundCheckerV2::checkUpperBound(CheckerContext &Ctx, SValBuilder &SVB,
                                     ProgramStateRef State,
                                     RegionRawOffsetV2 RawOffset) const {
  DefinedOrUnknownSVal Extent =
      getDynamicSize(State, RawOffset.getRegion(), SVB);
  if (Extent.isUnknown())
    return State;

  /*mut*/ NonLoc RootExpr = RawOffset.getByteOffset();
  /*mut*/ NonLoc FoldedRHS = Extent.castAs<NonLoc>();

  if (const auto SymbolicRoot = RawOffset.getByteOffset().getAs<SymbolVal>()) {
    if (const auto ExtentAsInt = Extent.getAs<ConcreteInt>()) {
      Simplifier Visitor(State, SVB, SymbolicRoot->getSymbol(),
                         ExtentAsInt.getValue());
      Visitor.Visit(SymbolicRoot->getSymbol());

      // Overwrite the existing values.
      RootExpr = Visitor.getRootSymbol();
      FoldedRHS = Visitor.getFoldedConstant();

      // Preserve the no overflow/underflow state.
      State = Visitor.getLastValidState();
    }
  }

  // No unsigned symbolic value can be less then a negative constant.
  if (const auto SymbolicRoot = RootExpr.getAs<SymbolVal>())
    if (const auto FoldedRHSAsInt = FoldedRHS.getAs<ConcreteInt>())
      if (SymbolicRoot->getSymbol()->getType()->isUnsignedIntegerType() &&
          FoldedRHSAsInt->getValue().isNegative())
        return State;

  NonLoc UpperBoundCheck =
      SVB.evalBinOpNN(State, BO_GE, RootExpr, FoldedRHS, SVB.getConditionType())
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


void ento::registerArrayBoundCheckerV2(CheckerManager &mgr) {
  mgr.registerChecker<ArrayBoundCheckerV2>();
}

bool ento::shouldRegisterArrayBoundCheckerV2(const CheckerManager &mgr) {
  return true;
}
