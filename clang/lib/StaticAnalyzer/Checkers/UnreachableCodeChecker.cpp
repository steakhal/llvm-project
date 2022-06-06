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

#include "clang/AST/ParentMap.h"
#include "clang/Basic/Builtins.h"
#include "clang/Basic/SourceManager.h"
#include "clang/StaticAnalyzer/Checkers/BuiltinCheckerRegistration.h"
#include "clang/StaticAnalyzer/Core/BugReporter/BugReporter.h"
#include "clang/StaticAnalyzer/Core/Checker.h"
#include "clang/StaticAnalyzer/Core/CheckerManager.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerHelpers.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ExplodedGraph.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/SVals.h"
#include <queue>

using namespace clang;
using namespace ento;
using namespace llvm;

using CFGBlocksSet = SmallSet<unsigned, 32>;

template <typename Range> static auto skippingNulls(Range &&R) {
  auto IsNonNull = [](const CFGBlock *B) -> bool { return B; };
  return llvm::make_filter_range(R, IsNonNull);
}

namespace {
class UnreachableCodeChecker : public Checker<check::EndAnalysis> {
public:
  void checkEndAnalysis(ExplodedGraph &G, BugReporter &B,
                        ExprEngine &Eng) const;
};
} // namespace

// Finds the entry point(s) for this dead CFGBlock in a BFS order.
// Marks reachable all blocks, whose parents are all unreachable.
static void filterUnreachableEntryPoints(const CFGBlock *CB,
                                         CFGBlocksSet &Reachable,
                                         CFGBlocksSet &Visited) {
  if (Visited.contains(CB->getBlockID()))
    return;

  auto IsUnreachable = [&Reachable](const CFGBlock *B) -> bool {
    return !Reachable.contains(B->getBlockID());
  };

  std::queue<const CFGBlock *> Worklist;
  Worklist.push(CB);

  while (!Worklist.empty()) {
    const CFGBlock *Current = Worklist.front();
    unsigned CurrentID = Current->getBlockID();
    Worklist.pop();

    // Skip if already visited.
    if (!Visited.insert(CurrentID).second)
      continue;

    auto NonNullPreds = skippingNulls(Current->preds());

    // Schedule the unvisited predecessors.
    for (const CFGBlock *Pred : NonNullPreds)
      Worklist.push(Pred);

    if (Reachable.contains(CurrentID))
      continue;

    // If all predecessors are unreachable, consider the current block as
    // reachable.
    if (!NonNullPreds.empty() && all_of(NonNullPreds, IsUnreachable))
      Reachable.insert(CurrentID);
  }
}

/// Find the Stmt* in a CFGBlock for reporting a warning.
/// Might return null.
static const Stmt *getUnreachableStmt(const CFGBlock *CB) {
  for (const CFGElement &E : *CB) {
    if (auto S = E.getAs<CFGStmt>())
      if (!isa<DeclStmt>(S->getStmt()))
        return S->getStmt();
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

  auto NonNullPreds = skippingNulls(CB->preds());
  if (NonNullPreds.empty())
    return false;

  // Get the predecessor block's terminator condition
  const Stmt *cond = (*NonNullPreds.begin())->getTerminatorCondition();

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

/// TODO: Doc this...
static bool shouldIgnoreBlock(const CFGBlock *CB, const ParentMap &PM,
                              const ASTContext &Ctx) {
  // Check for false positives.
  if (isInvalidPath(CB, PM))
    return true;

  // It is good practice to always have a "default" label in a "switch", even
  // if we should never get there. It can be used to detect errors, for
  // instance. Unreachable code directly under a "default" label is therefore
  // likely to be a false positive.
  if (const Stmt *L = CB->getLabel())
    if (L->getStmtClass() == Stmt::DefaultStmtClass)
      return true;

  // Special case for __builtin_unreachable.
  // FIXME: This should be extended to include other unreachable markers,
  // such as llvm_unreachable.
  if (isBuiltinUnreachable(CB, Ctx))
    return true;

  return false;
}

static const StackFrameContext *getTopFrame(const ExplodedGraph &G) {
  for (const ExplodedNode &N : G.nodes())
    if (N.getLocation().getLocationContext()->inTopFrame())
      return cast<StackFrameContext>(N.getLocationContext());
  llvm_unreachable("The top-level frame should always exist.");
}

/// The sink block should be reachable, but all the non-self successor blocks
/// should be unreachable.
static bool isSink(const CFGBlock *Block, const CFGBlocksSet &Reachables) {
  if (!Reachables.contains(Block->getBlockID()))
    return false;

  for (const CFGBlock *Succ : skippingNulls(Block->succs()))
    if (Reachables.contains(Succ->getBlockID()))
      return false;

  return true;
}

static CFGBlocksSet collectReachableBlocks(const ExplodedGraph &G) {
  CFGBlocksSet Reachable;
  for (const ExplodedNode &N : G.nodes())
    if (N.getLocation().getLocationContext()->inTopFrame())
      if (auto BE = N.getLocationAs<BlockEntrance>())
        Reachable.insert(BE->getBlock()->getBlockID());
  return Reachable;
}

static void filterUnreachableBlocksCausedBySinks(
    const std::vector<const CFGBlock *> &Blocks, CFGBlocksSet &Reachables) {
  CFGBlocksSet SuccessorsOfSinks;
  for (const CFGBlock *Block : Blocks) {
    assert(Block);
    if (isSink(Block, Reachables))
      for (const CFGBlock *Succ : skippingNulls(Block->succs()))
        SuccessorsOfSinks.insert(Succ->getBlockID());
  }

  // Mark these blocks as 'reachable' to prevent reporting these.
  Reachables.insert(SuccessorsOfSinks.begin(), SuccessorsOfSinks.end());
}

void UnreachableCodeChecker::checkEndAnalysis(ExplodedGraph &G, BugReporter &B,
                                              ExprEngine &Eng) const {
  if (Eng.hasWorkRemaining())
    return;

  const SourceManager &SM = B.getSourceManager();
  const ASTContext &ACtx = Eng.getContext();
  const StackFrameContext *Frame = getTopFrame(G);
  const ParentMap &PM = Frame->getParentMap();
  const CFG *C = Frame->getUnoptimizedCFG();
  assert(C);

  // Don't do anything for template instantiations.  Proving that code
  // in a template instantiation is unreachable means proving that it is
  // unreachable in all instantiations.
  if (const FunctionDecl *FD = dyn_cast<FunctionDecl>(Frame->getDecl()))
    if (FD->isTemplateInstantiation())
      return;

  CFGBlocksSet Visited;
  CFGBlocksSet Reachables = collectReachableBlocks(G);
  Visited.insert(C->getEntry().getBlockID());
  Reachables.insert(C->getEntry().getBlockID());
  Reachables.insert(C->getExit().getBlockID());

  filterUnreachableEntryPoints(&C->getExit(), Reachables, Visited);

  std::vector<const CFGBlock *> ConsideredBlocks;
  ConsideredBlocks.reserve(C->size());
  llvm::append_range(ConsideredBlocks, skippingNulls(*C));
  llvm::erase_if(ConsideredBlocks, isEmptyCFGBlock);

  filterUnreachableBlocksCausedBySinks(ConsideredBlocks, Reachables);

  auto ShouldIgnoreBlock = [&](const CFGBlock *B) {
    return shouldIgnoreBlock(B, PM, ACtx);
  };
  auto IsReachable = [&](const CFGBlock *B) {
    return Reachables.contains(B->getBlockID());
  };

  llvm::erase_if(ConsideredBlocks, ShouldIgnoreBlock);
  llvm::erase_if(ConsideredBlocks, IsReachable);

  for (const CFGBlock *UnreachableBlock : ConsideredBlocks) {
    const Stmt *S = getUnreachableStmt(UnreachableBlock);
    if (!S)
      continue;

    if (isDoWhileMacro(S, PM))
      continue;

    // FIXME: Exceptions and try-catch blocks are modeled by a malformed CFG.
    // Let's suppress these for now.
    // For more details see: #55621.
    if (isa<CXXTryStmt>(S))
      continue;

    SourceRange SR = S->getSourceRange();
    PathDiagnosticLocation DL =
        PathDiagnosticLocation::createBegin(S, SM, Frame);
    SourceLocation SL = DL.asLocation();
    if (SR.isInvalid() || !SL.isValid() || SM.isInSystemHeader(SL))
      continue;

    B.EmitBasicReport(Frame->getDecl(), this, "Unreachable code",
                      categories::UnusedCode,
                      "This statement is never executed", DL, SR);
  }
}

void ento::registerUnreachableCodeChecker(CheckerManager &mgr) {
  mgr.registerChecker<UnreachableCodeChecker>();
}

bool ento::shouldRegisterUnreachableCodeChecker(const CheckerManager &mgr) {
  return true;
}
