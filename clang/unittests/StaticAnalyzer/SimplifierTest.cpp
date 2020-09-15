#include "CheckerRegistration.h"
#include "Reusables.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/StaticAnalyzer/Core/BugReporter/BugReporter.h"
#include "clang/StaticAnalyzer/Core/BugReporter/BugType.h"
#include "clang/StaticAnalyzer/Core/Checker.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CallEvent.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/SValVisitor.h"
#include "clang/StaticAnalyzer/Frontend/AnalysisConsumer.h"
#include "clang/StaticAnalyzer/Frontend/CheckerRegistry.h"
#include "gtest/gtest.h"

namespace clang {
namespace ento {
namespace {
using ConcreteInt = nonloc::ConcreteInt;
using SymbolVal = nonloc::SymbolVal;
using APSInt = llvm::APSInt;

class RegionRawOffsetV3 {
private:
  const SubRegion *BaseRegion = nullptr;
  SVal ByteOffset = UnknownVal();

  RegionRawOffsetV3() = default;

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
      : public MemRegionVisitor<RawOffsetCalculator, RegionRawOffsetV3> {
    ProgramStateRef State;
    SValBuilder &SVB;

    RegionRawOffsetV3 Leaf(const SubRegion *R) const {
      return {R, SVB.makeArrayIndex(0)};
    }

  public:
    RawOffsetCalculator(ProgramStateRef State, SValBuilder &SVB)
        : State(State), SVB(SVB) {}
    using MemRegionVisitor::Visit;

    auto VisitMemRegion(const MemRegion *R) {
      return Leaf(dyn_cast<SubRegion>(R));
    }

    RegionRawOffsetV3 VisitElementRegion(const ElementRegion *ER) {
      // For: Elem{SuperReg, ElemTy, ElemIdx}
      // 1) Calculate the raw offset of the SuperReg.
      // 2) Handle the current level.
      //    Offset := Offset + sizeof(ElemTy) * ElemIdx
      const RegionRawOffsetV3 RawOffset = Visit(ER->getSuperRegion());

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
  RegionRawOffsetV3(const SubRegion *BaseRegion, SVal ByteOffset)
      : BaseRegion(BaseRegion), ByteOffset(ByteOffset) {}

  NonLoc getByteOffset() const { return ByteOffset.castAs<NonLoc>(); }
  const SubRegion *getRegion() const { return BaseRegion; }

  static RegionRawOffsetV3 computeOffset(ProgramStateRef State,
                                         SValBuilder &SVB,
                                         loc::MemRegionVal Location) {
    return RawOffsetCalculator(State, SVB).Visit(Location.getRegion());
  }

  LLVM_DUMP_METHOD void dump() const { dumpToStream(llvm::errs()); }

  void dumpToStream(raw_ostream &os) const {
    os << "raw_offset_v3{" << getRegion() << ',' << getByteOffset() << '}';
  }
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

class SimplifierGenerator : public Checker<eval::Call> {
  using Self = SimplifierGenerator;
  const BuiltinBug SimplifierGeneratorBug{this, "SimplifierGenerator"};
  using HandlerFn = bool (Self::*)(const CallEvent &Call,
                                   CheckerContext &) const;
  CallDescriptionMap<HandlerFn> Callbacks = {{{"dump", 1}, &Self::dump}};

  bool report(CheckerContext &C, ProgramStateRef State,
              StringRef Description) const {
    ExplodedNode *Node = C.generateNonFatalErrorNode(State);
    if (!Node)
      return false;

    auto Report = std::make_unique<PathSensitiveBugReport>(
        SimplifierGeneratorBug, Description, Node);
    C.emitReport(std::move(Report));
    return true;
  }

  bool dump(const CallEvent &Call, CheckerContext &C) const {
    const auto MemRegVal = Call.getArgSVal(0).getAs<loc::MemRegionVal>();
    if (!MemRegVal.hasValue())
      return false;

    const RegionRawOffsetV3 RawOffset = RegionRawOffsetV3::computeOffset(
        C.getState(), C.getSValBuilder(), *MemRegVal);
    ProgramStateRef State = C.getState();

    std::string Msg;
    llvm::raw_string_ostream OS(Msg);
    RawOffset.dumpToStream(OS);
    OS << " ";

    const auto RootSymbol = RawOffset.getByteOffset().getAs<SymbolVal>();
    if (!RootSymbol.hasValue())
      return false;

    Simplifier Visitor(
        State, C.getSValBuilder(), RootSymbol->getSymbol(),
        C.getSValBuilder().makeArrayIndex(0).castAs<ConcreteInt>());
    Visitor.Visit(RootSymbol->getSymbol());

    OS << "Previous state:\n";
    State->printJson(OS);

    if (Visitor.getLastValidState() != State) {
      // Preserve the no overflow/underflow state.
      State = Visitor.getLastValidState();
      OS << "Using the state after simplification:\n";
      State->printJson(OS);
      const NoteTag *Tag =
          C.getNoteTag("Assuming that the calculation of the pointer does "
                       "not overflow/underflow");
      C.addTransition(State, Tag);
    }

    // TODO::::: dump the simplification result:
    {
      OS << "simplification " << (Visitor.succeeded() ? "succeeded" : "failed")
         << ": { rootSymbol: " << Visitor.getRootSymbol() << ", "
         << Visitor.getFoldedConstant() << " }  State->dump(): ";
      State->printJson(OS);
    }

    // TODO::: remove this
    // Check if we access out-of-bound element before the first valid element.
    {
      SValBuilder &SVB = C.getSValBuilder();
      const NonLoc InLowerBoundCheck =
          SVB.evalBinOpNN(State, BO_GE, Visitor.getRootSymbol(),
                          Visitor.getFoldedConstant(), SVB.getConditionType())
              .castAs<NonLoc>();
      ProgramStateRef InLowerBound, OutLowerBound;
      std::tie(InLowerBound, OutLowerBound) = State->assume(InLowerBoundCheck);
      if (!InLowerBound && OutLowerBound) {
        // TODO: bad, report bug
        OS << "  Access out-of-bound element before the first valid region";
        return report(C, State, Msg);
      } else if (InLowerBound && !OutLowerBound) {
        // TODO: safe, for sure.
        OS << "  GOOD";
        State = InLowerBound;
      } else {
        // TODO: maybe safe...
        OS << "  MAYBE";
        State = InLowerBound;
      }
      OS << "  We assume that the lowerbound is fine! here is the State: ";
      State->printJson(OS);
    }

    // TODO::: remove this
    // Check if we access out-of-bound element after the last valid element.
    {
      Simplifier Visitor2(
          State, C.getSValBuilder(), RootSymbol->getSymbol(),
          C.getSValBuilder().makeArrayIndex(5).castAs<ConcreteInt>());
      Visitor2.Visit(RootSymbol->getSymbol());

      SValBuilder &SVB = C.getSValBuilder();
      const NonLoc InLowerBoundCheck =
          SVB.evalBinOpNN(State, BO_LT, Visitor2.getRootSymbol(),
                          Visitor2.getFoldedConstant(), SVB.getConditionType())
              .castAs<NonLoc>();
      ProgramStateRef InLowerBound, OutLowerBound;
      std::tie(InLowerBound, OutLowerBound) = State->assume(InLowerBoundCheck);
      if (!InLowerBound && OutLowerBound) {
        // TODO: bad, report bug
        OS << "  Access out-of-bound element after the last valid region";
        return report(C, State, Msg);
      } else if (InLowerBound && !OutLowerBound) {
        // TODO: safe, for sure.
        OS << "  GOOD";
        State = InLowerBound;
      } else {
        // TODO: maybe safe...
        OS << "  MAYBE";
        State = InLowerBound;
      }
      OS << "  We assume that the upperbound is fine! here is the State: ";
      State->printJson(OS);
    }

    return report(C, State, Msg);
  }

public:
  bool evalCall(const CallEvent &Call, CheckerContext &C) const {
    if (const HandlerFn *Callback = Callbacks.lookup(Call))
      return (this->*(*Callback))(Call, C);
    return false;
  }
};

void addSimplifierGenerator(AnalysisASTConsumer &AnalysisConsumer,
                            AnalyzerOptions &AnOpts) {
  AnOpts.CheckersAndPackages = {{"test.SimplifierGenerator", true},
                                {"debug.ExprInspection", true},
                                {"debug.ViewExplodedGraph", false}};
  AnalysisConsumer.AddCheckerRegistrationFn([](CheckerRegistry &Registry) {
    Registry.addChecker<SimplifierGenerator>(
        "test.SimplifierGenerator", "EmptyDescription", "EmptyDocsUri");
  });
}

TEST(SimplifierTest, asdasd) {
  constexpr auto Code = R"(
    const char a[] = "abcd";
    void dump(const char*);
    void clang_analyzer_printState();
    void test(unsigned long long x, int y) {
      //if (x <= -1 && x >= -3) {
        //clang_analyzer_printState();
        dump(&a[x + 1]);
        (void)x;
      //}
    })";
  std::string Diags;
  EXPECT_TRUE(runCheckerOnCode<addSimplifierGenerator>(Code, Diags));
  EXPECT_EQ(Diags, "test.SimplifierGenerator:some warning??\n");
}

} // namespace
} // namespace ento
} // namespace clang
