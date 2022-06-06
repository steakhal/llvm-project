//==- UnreachableCodeChecker.cpp - Generalized dead code checker -*- C++ -*-==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
// This file implements a generalized unreachable code checker using a
// path-sensitive analysis. We mark any path visited, and then walk the CFG as a
// post-analysis to determine what was never visited.
//
// A similar flow-sensitive only check exists in Analysis/ReachableCode.cpp
//===----------------------------------------------------------------------===//

#include "clang/StaticAnalyzer/Checkers/BuiltinCheckerRegistration.h"
#include "clang/AST/ParentMap.h"
#include "clang/Basic/Builtins.h"
#include "clang/Basic/SourceManager.h"
#include "clang/StaticAnalyzer/Core/BugReporter/BugReporter.h"
#include "clang/StaticAnalyzer/Core/Checker.h"
#include "clang/StaticAnalyzer/Core/CheckerManager.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerHelpers.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ExplodedGraph.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/SVals.h"
#include "llvm/ADT/SmallSet.h"

using namespace clang;
using namespace ento;

using CFGBlocksSet = llvm::SmallSet<unsigned, 32>;

namespace {
class UnreachableCodeChecker : public Checker<check::EndAnalysis> {
public:
  void checkEndAnalysis(ExplodedGraph &G, BugReporter &B,
                        ExprEngine &Eng) const;
};
} // namespace

// Recursively finds the entry point(s) for this dead CFGBlock.
void findUnreachableEntryPoints(const CFGBlock *CB, CFGBlocksSet &reachable,
                                CFGBlocksSet &visited) {
  visited.insert(CB->getBlockID());

  for (CFGBlock::const_pred_iterator I = CB->pred_begin(), E = CB->pred_end();
       I != E; ++I) {
    if (!*I)
      continue;

    if (!reachable.count((*I)->getBlockID())) {
      // If we find an unreachable predecessor, mark this block as reachable so
      // we don't report this block
      reachable.insert(CB->getBlockID());
      if (!visited.count((*I)->getBlockID()))
        // If we haven't previously visited the unreachable predecessor, recurse
        findUnreachableEntryPoints(*I, reachable, visited);
    }
  }
}

// Find the Stmt* in a CFGBlock for reporting a warning
static const Stmt *getUnreachableStmt(const CFGBlock *CB) {
  for (CFGBlock::const_iterator I = CB->begin(), E = CB->end(); I != E; ++I) {
    if (Optional<CFGStmt> S = I->getAs<CFGStmt>()) {
      if (!isa<DeclStmt>(S->getStmt()))
        return S->getStmt();
    }
  }
  return CB->getTerminatorStmt();
}

// Determines if the path to this CFGBlock contained an element that infers this
// block is a false positive. We assume that findUnreachableEntryPoints has
// already marked only the entry points to any dead code, so we need only to
// find the condition that led to this block (the predecessor of this block.)
// There will never be more than one predecessor.
static bool isInvalidPath(const CFGBlock *CB, const ParentMap &PM) {
  // We only expect a predecessor size of 0 or 1. If it is >1, then an external
  // condition has broken our assumption (for example, a sink being placed by
  // another check). In these cases, we choose not to report.
  if (CB->pred_size() > 1)
    return true;

  // If there are no predecessors, then this block is trivially unreachable
  if (CB->pred_size() == 0)
    return false;

  const CFGBlock *pred = *CB->pred_begin();
  if (!pred)
    return false;

  // Get the predecessor block's terminator condition
  const Stmt *cond = pred->getTerminatorCondition();

  // assert(cond && "CFGBlock's predecessor has a terminator condition");
  //  The previous assertion is invalid in some cases (eg do/while). Leaving
  //  reporting of these situations on at the moment to help triage these cases.
  if (!cond)
    return false;

  // Run each of the checks on the conditions
  return containsMacro(cond) || containsEnum(cond) ||
         containsStaticLocal(cond) || containsBuiltinOffsetOf(cond) ||
         containsStmt<UnaryExprOrTypeTraitExpr>(cond);
}

// Returns true if the given CFGBlock is empty
static bool isEmptyCFGBlock(const CFGBlock *CB) {
  return CB->getLabel() == nullptr    // No labels
         && CB->size() == 0           // No statements
         && !CB->getTerminatorStmt(); // No terminator
}

static bool isBuiltinUnreachable(const CFGBlock *CB, const ASTContext &Ctx) {
  for (const CFGElement &E : *CB) {
    if (const auto S = E.getAs<CFGStmt>())
      if (const auto *CE = dyn_cast<CallExpr>(S->getStmt())) {
        if (CE->getBuiltinCallee() == Builtin::BI__builtin_unreachable ||
            CE->isBuiltinAssumeFalse(Ctx)) {
          return true;
        }
      }
  }
  return false;
}

static bool isDoWhileMacro(const Stmt *S, const ParentMap &PM) {
  if (S->getBeginLoc().isMacroID())
    if (const auto *I = dyn_cast<IntegerLiteral>(S))
      if (I->getValue() == 0)
        if (const Stmt *Parent = PM.getParent(S))
          if (isa<DoStmt>(Parent))
            return true;
  return false;
}

/// Recursively walk backwards the CFG to find the first unreachable block
/// leading to the given block.
/// Return true if the given block should be ignored. It can happen for multiple
/// reasons:
/// 1) Some ascendant block of this should be reported instead.
/// 2) This was an artificial empty block, such as an Entry or Exit block.
/// 3) The heuristic found this a likely false-positive.
/// 4) Deliberately ignore 'default' cases in switches.
/// 5) This is an unreachable marked block.
static bool shouldIgnoreBlock(const CFGBlock *CB, CFGBlocksSet &reachable,
                              CFGBlocksSet &visited, const ParentMap &PM,
                              const ASTContext &Ctx) {
  // Check if the block is unreachable
  if (reachable.count(CB->getBlockID()))
    return true;

  // Check if the block is empty (an artificial block)
  if (isEmptyCFGBlock(CB))
    return true;

  // Find the entry points for this block
  if (!visited.count(CB->getBlockID()))
    findUnreachableEntryPoints(CB, reachable, visited);

  // This block may have been pruned; check if we still want to report it
  if (reachable.count(CB->getBlockID()))
    return true;

  // Check for false positives
  if (isInvalidPath(CB, PM))
    return true;

  // It is good practice to always have a "default" label in a "switch", even
  // if we should never get there. It can be used to detect errors, for
  // instance. Unreachable code directly under a "default" label is therefore
  // likely to be a false positive.
  if (const Stmt *label = CB->getLabel())
    if (label->getStmtClass() == Stmt::DefaultStmtClass)
      return true;

  // Special case for __builtin_unreachable.
  // FIXME: This should be extended to include other unreachable markers,
  // such as llvm_unreachable.
  if (isBuiltinUnreachable(CB, Ctx))
    return true;

  return false;
}

static bool inTopFrame(const ExplodedNode &N) {
  return N.getLocation().getLocationContext()->inTopFrame();
}

static const ExplodedNode *getFirstNonTopNode(const ExplodedGraph &G) {
  for (const ExplodedNode &N : G.nodes())
    if (inTopFrame(N))
      return &N;
  return nullptr;
}

void UnreachableCodeChecker::checkEndAnalysis(ExplodedGraph &G, BugReporter &B,
                                              ExprEngine &Eng) const {
  if (Eng.hasWorkRemaining())
    return;

  const ExplodedNode *N = getFirstNonTopNode(G);
  if (!N)
    return;

  const SourceManager &SM = B.getSourceManager();
  CFGBlocksSet reachable, visited;

  const LocationContext *LC = N->getLocation().getLocationContext();
  const Decl *D = LC->getAnalysisDeclContext()->getDecl();
  CFG *C = LC->getAnalysisDeclContext()->getUnoptimizedCFG();
  const ParentMap *PM = &LC->getParentMap();

  // Bail out if we didn't get the CFG or the ParentMap.
  if (!D || !C || !PM)
    return;

  // Don't do anything for template instantiations.  Proving that code
  // in a template instantiation is unreachable means proving that it is
  // unreachable in all instantiations.
  if (const FunctionDecl *FD = dyn_cast<FunctionDecl>(D))
    if (FD->isTemplateInstantiation())
      return;

  // Collect all block entrances.
  for (const ExplodedNode &N : G.nodes())
    if (inTopFrame(N))
      if (auto BE = N.getLocationAs<BlockEntrance>())
        reachable.insert(BE->getBlock()->getBlockID());

  // Find CFGBlocks that were not covered by any node
  for (const CFGBlock *CB : *C) {
    if (shouldIgnoreBlock(CB, reachable, visited, *PM, Eng.getContext()))
      continue;

    // We found a block that wasn't covered - find the statement to report
    const Stmt *S = getUnreachableStmt(CB);
    if (!S)
      continue;

    // In macros, 'do {...} while (0)' is often used. Don't warn about the
    // condition 0 when it is unreachable.
    if (isDoWhileMacro(S, *PM))
      continue;

    SourceRange SR = S->getSourceRange();
    PathDiagnosticLocation DL = PathDiagnosticLocation::createBegin(S, SM, LC);
    SourceLocation SL = DL.asLocation();
    if (SR.isInvalid() || !SL.isValid() || SM.isInSystemHeader(SL))
      continue;

    B.EmitBasicReport(D, this, "Unreachable code", categories::UnusedCode,
                      "This statement is never executed", DL, SR);
  }
}

void ento::registerUnreachableCodeChecker(CheckerManager &mgr) {
  mgr.registerChecker<UnreachableCodeChecker>();
}

bool ento::shouldRegisterUnreachableCodeChecker(const CheckerManager &mgr) {
  return true;
}
