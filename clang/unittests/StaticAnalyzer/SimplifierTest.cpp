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
  ProgramStateRef State;
  SValBuilder &SVB;
  SymbolRef RootSymbol;
  llvm::APSInt FoldedConstant;
  const llvm::APSInt FoldedConstantMin =
      SVB.getBasicValueFactory().getMinValue(FoldedConstant);
  const llvm::APSInt FoldedConstantMax =
      SVB.getBasicValueFactory().getMaxValue(FoldedConstant);
  bool SimplificationFailed = false;

public:
  Simplifier(ProgramStateRef State, SValBuilder &SVB, SymbolRef RootSymbol,
             ConcreteInt Constant)
      : State(std::move(State)), SVB(SVB), RootSymbol(RootSymbol),
        FoldedConstant(Constant.getValue()) {}
  using SymExprVisitor::Visit;

  void VisitSymIntExpr(const SymIntExpr *E) {
    assert(!SimplificationFailed);

    const SymbolRef LHS = E->getLHS();
    const QualType SymTy = LHS->getType();
    const llvm::APSInt RHSConstant =
        APSIntType(FoldedConstant).convert(E->getRHS());
    // TODO: assert that `RHSConstant >= 0`

    const BinaryOperator::Opcode Op = E->getOpcode();
    if (Op == BO_Sub) {
      // Assume that the left-hand side did not underflow, and check if the
      // right-hand side constant folding would not overflow.
      const auto SymMin = SVB.getBasicValueFactory().getMinValue(SymTy);
      const NonLoc NoUnderflowHappensCheck =
          SVB.evalBinOpNN(State, BO_GE, nonloc::SymbolVal(LHS),
                          nonloc::ConcreteInt(
                              SymMin + APSIntType(SymMin).convert(RHSConstant)),
                          SVB.getConditionType())
              .castAs<NonLoc>();
      if (ProgramStateRef NewState =
              State->assume(NoUnderflowHappensCheck, true)) {
        State = NewState;
        // Ctx.addTransition(State);
      } else {
        // The symbolic expression underflows for sure, we can not simplify.
        llvm::errs() << "The original expression underflows, we can not "
                        "simplify such expressions\n";
        SimplificationFailed = true;
        return;
      }

      // If overflow would happened when we fold the constants, just back off.
      if (FoldedConstantMax - RHSConstant < FoldedConstant) {
        llvm::errs() << "Could not fold the constant into the right-hand side "
                        "since that would have overflown\n";
        SimplificationFailed = true;
        return;
      }

      // Otherwise safe to do the folding, and continue the simplification
      // process.
      RootSymbol = E->getLHS();
      FoldedConstant =
          FoldedConstant + APSIntType(FoldedConstant).convert(RHSConstant);
      Visit(RootSymbol); // Continue the folding process.
      return;
    }

    if (Op == BO_Add) {
      // Assume that the left-hand side did not overflow, and check if the
      // right-hand side constant folding would not underflow.
      const auto SymMax = SVB.getBasicValueFactory().getMaxValue(SymTy);

      const NonLoc NoOverflowHappensCheck =
          SVB.evalBinOpNN(State, BO_LE, nonloc::SymbolVal(LHS),
                          nonloc::ConcreteInt(
                              SymMax - APSIntType(SymMax).convert(RHSConstant)),
                          SVB.getConditionType())
              .castAs<NonLoc>();
      if (ProgramStateRef NewState =
              State->assume(NoOverflowHappensCheck, true)) {
        State = NewState;
        // Ctx.addTransition(State);
      } else {
        // The symbolic expression overflows for sure, we can not simplify.
        llvm::errs() << "The original expression overflows, we can not "
                        "simplify such expressions\n";
        SimplificationFailed = true;
        return;
      }

      // If underflow would happened when we fold the constants, just back off.
      if (FoldedConstantMin + RHSConstant > FoldedConstant) {
        llvm::errs() << "Could not fold the constant into the right-hand side "
                        "since that would have underflown\n";
        SimplificationFailed = true;
        return;
      }

      // Otherwise safe to do the folding, and continue the simplification
      // process.
      RootSymbol = E->getLHS();
      FoldedConstant =
          FoldedConstant - APSIntType(FoldedConstant).convert(RHSConstant);
      Visit(RootSymbol); // Continue the folding process.
      return;
    }

    if (Op == BO_Mul) {
      assert(!RHSConstant.isNullValue());
      // Assume that the left-hand side did not overflow, and check if the
      // right-hand side division would not truncate.
      const auto SymMax = SVB.getBasicValueFactory().getMaxValue(SymTy);
      const llvm::APSInt Floor =
          SymMax / APSIntType(SymMax).convert(RHSConstant) +
          llvm::APSInt(SymMax % APSIntType(SymMax).convert(RHSConstant) != 0, !SymMax.isSigned());

      const NonLoc NoOverflowHappensCheck =
          SVB.evalBinOpNN(State, BO_GT, nonloc::SymbolVal(LHS),
                          nonloc::ConcreteInt(Floor), SVB.getConditionType())
              .castAs<NonLoc>();
      if (ProgramStateRef NewState =
              State->assume(NoOverflowHappensCheck, true)) {
        State = NewState;
        // Ctx.addTransition(State);
      } else {
        llvm::errs() << "The original expression overflows, we can not "
                        "simplify such expressions\n";
        SimplificationFailed = true;
        return;
      }

      // TODO comment.
      if (FoldedConstant % APSIntType(FoldedConstant).convert(RHSConstant) !=
          0) {
        // Would truncate during constant folding.
        llvm::errs() << "Could not fold the constant into the right-hand side "
                        "since that would truncate\n";
        SimplificationFailed = true;
        return;
      }

      // Otherwise safe to do the folding, and continue the simplification
      // process.
      RootSymbol = E->getLHS();
      FoldedConstant =
          FoldedConstant / APSIntType(FoldedConstant).convert(RHSConstant);
      Visit(RootSymbol); // Continue the folding process.
      return;
    }
  }

  void VisitIntSymExpr(const IntSymExpr *E) {
    assert("not yet implemented since the canonical representation is "
           "SymIntExpr based");
  }

  ProgramStateRef getState() const { return State; }
  nonloc::ConcreteInt getFoldedConstant() const {
    return nonloc::ConcreteInt(FoldedConstant);
  }
  nonloc::SymbolVal getRootSymbol() const {
    return nonloc::SymbolVal(RootSymbol);
  }
  bool succeeded() const { return !SimplificationFailed; }
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
    std::string Msg;
    llvm::raw_string_ostream OS(Msg);
    RawOffset.dumpToStream(OS);
    OS << " ";

    if (const auto RootSymbol = RawOffset.getByteOffset().getAs<SymbolVal>()) {
      Simplifier Visitor(
          C.getState(), C.getSValBuilder(), RootSymbol->getSymbol(),
          C.getSValBuilder().makeArrayIndex(1).castAs<ConcreteInt>());
      Visitor.Visit(RootSymbol->getSymbol());
      OS << "simplified (" << (Visitor.succeeded() ? "succeeded" : "failed")
         << ") to: "
         << (Visitor.getState() == nullptr ? "NULL state" : "valid state");
      OS << ", rootSymbol: " << Visitor.getRootSymbol()
         << " with constant: " << Visitor.getFoldedConstant() << "\n";
      if (Visitor.getState()) {
        Visitor.getState()->printJson(OS << "{ ");
        OS << " }\n";
      }
    } else {
      OS << "Could not simplify\n";
    }
    return report(C, C.getState(), Msg);
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
    void test(int x) {
      //if (x <= -2147483647) {
        clang_analyzer_printState();
        dump(&a[x*2]); //
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
