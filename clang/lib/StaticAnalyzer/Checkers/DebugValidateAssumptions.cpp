//==-- DebugValidateAssumptions.cpp -------------------------------*- C++ -*--//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Defines a checker for checking the analyzer's state for satisfiability after
// each assume call.
//
//===----------------------------------------------------------------------===//

#include "clang/StaticAnalyzer/Checkers/BuiltinCheckerRegistration.h"
#include "clang/StaticAnalyzer/Core/Checker.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ExprEngine.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/RangedConstraintManager.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/SMTConv.h"
#include "llvm/Support/SMTAPI.h"

using namespace clang;
using namespace ento;

namespace {
class DebugValidateAssumptions : public Checker<eval::Assume> {
public:
  ProgramStateRef evalAssume(ProgramStateRef State, SVal, bool) const {
    ASTContext &Ctx = State->getBasicVals().getContext();
    llvm::SMTSolverRef Solver = llvm::CreateZ3Solver();

    // Add constraints to the solver
    for (const auto &I : getConstraintMap(State)) {
      const SymbolRef Sym = I.first;
      auto RangeIt = I.second.begin();

      llvm::SMTExprRef SMTConstraints = SMTConv::getRangeExpr(
          Solver, Ctx, Sym, RangeIt->From(), RangeIt->To(),
          /*InRange=*/true);
      while ((++RangeIt) != I.second.end()) {
        SMTConstraints =
            Solver->mkOr(SMTConstraints,
                         SMTConv::getRangeExpr(Solver, Ctx, Sym,
                                               RangeIt->From(), RangeIt->To(),
                                               /*InRange=*/true));
      }

      Solver->addConstraint(SMTConstraints);
    }

    // And check for satisfiability.
    if (Solver->check().getValueOr(true))
      return State;

    // The constraints are unsatisfiable. Dump and validate the state by hand.
    llvm::errs() << "The analyzer reached an infeasible path\n";
    llvm::errs()
        << "=============== State constraints in Z3 form ===============\n";
    Solver->dump();
    llvm::errs() << "=============== State dump ===============\n";
    State->dump();
    llvm::errs() << "\n=============== PLEASE INVESTIGATE ===============\n";
    // State->getStateManager().getOwningEngine().ViewGraph(0);
    return State;
  }
};
} // namespace

void ento::registerDebugValidateAssumptions(CheckerManager &Mgr) {
  Mgr.registerChecker<DebugValidateAssumptions>();
}

bool ento::shouldRegisterDebugValidateAssumptions(const CheckerManager &) {
  return true;
}
