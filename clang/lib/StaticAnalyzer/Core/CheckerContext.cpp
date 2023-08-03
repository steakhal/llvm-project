//== CheckerContext.cpp - Context info for path-sensitive checkers-----------=//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file defines CheckerContext that provides contextual info for
//  path-sensitive checkers.
//
//===----------------------------------------------------------------------===//

#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h"
#include "clang/Basic/Builtins.h"
#include "clang/Lex/Lexer.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ExprEngine.h"
#include "llvm/ADT/StringExtras.h"
#include <optional>

using namespace clang;
using namespace ento;

CheckerContext::CheckerContext(NodeBuilder &builder, ExprEngine &eng,
                               ExplodedNode *pred, const ProgramPoint &loc,
                               bool wasInlined /*=false*/)
    : Eng(eng), Pred(pred), Changed(false), Location(loc), NB(builder),
      wasInlined(wasInlined) {
  assert(Pred->getState() &&
         "We should not call the checkers on an empty state.");
}

AnalysisManager &CheckerContext::getAnalysisManager() {
  return Eng.getAnalysisManager();
}

ConstraintManager &CheckerContext::getConstraintManager() {
  return Eng.getConstraintManager();
}

StoreManager &CheckerContext::getStoreManager() {
  return Eng.getStoreManager();
}
unsigned CheckerContext::blockCount() const {
  return NB.getContext().blockCount();
}

ASTContext &CheckerContext::getASTContext() { return Eng.getContext(); }
const ASTContext &CheckerContext::getASTContext() const {
  return Eng.getContext();
}

const ProgramStateRef &CheckerContext::getState() const {
  return Pred->getState();
}

const LangOptions &CheckerContext::getLangOpts() const {
  return Eng.getContext().getLangOpts();
}

const LocationContext *CheckerContext::getLocationContext() const {
  return Pred->getLocationContext();
}

const StackFrameContext *CheckerContext::getStackFrame() const {
  return Pred->getStackFrame();
}

bool CheckerContext::inTopFrame() const {
  return getLocationContext()->inTopFrame();
}

BugReporter &CheckerContext::getBugReporter() { return Eng.getBugReporter(); }

const SourceManager &CheckerContext::getSourceManager() {
  return getBugReporter().getSourceManager();
}

Preprocessor &CheckerContext::getPreprocessor() {
  return getBugReporter().getPreprocessor();
}

SValBuilder &CheckerContext::getSValBuilder() { return Eng.getSValBuilder(); }

SymbolManager &CheckerContext::getSymbolManager() {
  return getSValBuilder().getSymbolManager();
}

ProgramStateManager &CheckerContext::getStateManager() {
  return Eng.getStateManager();
}

AnalysisDeclContext *CheckerContext::getCurrentAnalysisDeclContext() const {
  return Pred->getLocationContext()->getAnalysisDeclContext();
}

unsigned CheckerContext::getBlockID() const {
  return NB.getContext().getBlock()->getBlockID();
}

const MemRegion *
CheckerContext::getLocationRegionIfPostStore(const ExplodedNode *N) {
  ProgramPoint L = N->getLocation();
  if (std::optional<PostStore> PSL = L.getAs<PostStore>())
    return reinterpret_cast<const MemRegion *>(PSL->getLocationValue());
  return nullptr;
}

SVal CheckerContext::getSVal(const Stmt *S) const { return Pred->getSVal(S); }

void CheckerContext::emitReport(std::unique_ptr<BugReport> R) {
  Changed = true;
  Eng.getBugReporter().emitReport(std::move(R));
}

LLVM_ATTRIBUTE_RETURNS_NONNULL
const NoteTag *CheckerContext::getNoteTag(NoteTag::Callback &&Cb,
                                          bool IsPrunable /*= false*/) {
  return Eng.getDataTags().make<NoteTag>(std::move(Cb), IsPrunable);
}

ExplodedNode *
CheckerContext::addTransitionImpl(ProgramStateRef State, bool MarkAsSink,
                                  ExplodedNode *P /*= nullptr*/,
                                  const ProgramPointTag *Tag /*= nullptr*/) {
  // The analyzer may stop exploring if it sees a state it has previously
  // visited ("cache out"). The early return here is a defensive check to
  // prevent accidental caching out by checker API clients. Unless there is a
  // tag or the client checker has requested that the generated node be
  // marked as a sink, we assume that a client requesting a transition to a
  // state that is the same as the predecessor state has made a mistake. We
  // return the predecessor rather than cache out.
  //
  // TODO: We could potentially change the return to an assertion to alert
  // clients to their mistake, but several checkers (including
  // DereferenceChecker, CallAndMessageChecker, and DynamicTypePropagation)
  // rely upon the defensive behavior and would need to be updated.
  if (!State || (State == Pred->getState() && !Tag && !MarkAsSink))
    return Pred;

  Changed = true;
  const ProgramPoint &LocalLoc = (Tag ? Location.withTag(Tag) : Location);
  if (!P)
    P = Pred;

  ExplodedNode *node;
  if (MarkAsSink)
    node = NB.generateSink(LocalLoc, State, P);
  else
    node = NB.generateNode(LocalLoc, State, P);
  return node;
}

const FunctionDecl *CheckerContext::getCalleeDecl(const CallExpr *CE) const {
  const FunctionDecl *D = CE->getDirectCallee();
  if (D)
    return D;

  const Expr *Callee = CE->getCallee();
  SVal L = Pred->getSVal(Callee);
  return L.getAsFunctionDecl();
}

StringRef CheckerContext::getCalleeName(const FunctionDecl *FunDecl) const {
  if (!FunDecl)
    return StringRef();
  IdentifierInfo *funI = FunDecl->getIdentifier();
  if (!funI)
    return StringRef();
  return funI->getName();
}

StringRef CheckerContext::getDeclDescription(const Decl *D) {
  if (isa<ObjCMethodDecl, CXXMethodDecl>(D))
    return "method";
  if (isa<BlockDecl>(D))
    return "anonymous block";
  return "function";
}

bool CheckerContext::isCLibraryFunction(const FunctionDecl *FD,
                                        StringRef Name) {
  // To avoid false positives (Ex: finding user defined functions with
  // similar names), only perform fuzzy name matching when it's a builtin.
  // Using a string compare is slow, we might want to switch on BuiltinID here.
  unsigned BId = FD->getBuiltinID();
  if (BId != 0) {
    if (Name.empty())
      return true;
    StringRef BName = FD->getASTContext().BuiltinInfo.getName(BId);
    size_t start = BName.find(Name);
    if (start != StringRef::npos) {
      // Accept exact match.
      if (BName.size() == Name.size())
        return true;

      //    v-- match starts here
      // ...xxxxx...
      //   _xxxxx_
      //   ^     ^ lookbehind and lookahead characters

      const auto MatchPredecessor = [=]() -> bool {
        return start <= 0 || !llvm::isAlpha(BName[start - 1]);
      };
      const auto MatchSuccessor = [=]() -> bool {
        std::size_t LookbehindPlace = start + Name.size();
        return LookbehindPlace >= BName.size() ||
               !llvm::isAlpha(BName[LookbehindPlace]);
      };

      if (MatchPredecessor() && MatchSuccessor())
        return true;
    }
  }

  const IdentifierInfo *II = FD->getIdentifier();
  // If this is a special C++ name without IdentifierInfo, it can't be a
  // C library function.
  if (!II)
    return false;

  // Look through 'extern "C"' and anything similar invented in the future.
  // If this function is not in TU directly, it is not a C library function.
  if (!FD->getDeclContext()->getRedeclContext()->isTranslationUnit())
    return false;

  // If this function is not externally visible, it is not a C library function.
  // Note that we make an exception for inline functions, which may be
  // declared in header files without external linkage.
  if (!FD->isInlined() && !FD->isExternallyVisible())
    return false;

  if (Name.empty())
    return true;

  StringRef FName = II->getName();
  if (FName.equals(Name))
    return true;

  if (FName.startswith("__inline") && FName.contains(Name))
    return true;

  if (FName.startswith("__") && FName.endswith("_chk") && FName.contains(Name))
    return true;

  return false;
}

StringRef CheckerContext::getMacroNameOrSpelling(SourceLocation &Loc) {
  if (Loc.isMacroID())
    return Lexer::getImmediateMacroName(Loc, getSourceManager(),
                                             getLangOpts());
  SmallString<16> buf;
  return Lexer::getSpelling(Loc, buf, getSourceManager(), getLangOpts());
}

/// Evaluate comparison and return true if it's known that condition is true
static bool evalComparison(SVal LHSVal, BinaryOperatorKind ComparisonOp,
                           SVal RHSVal, ProgramStateRef State) {
  if (LHSVal.isUnknownOrUndef())
    return false;
  ProgramStateManager &Mgr = State->getStateManager();
  if (!isa<NonLoc>(LHSVal)) {
    LHSVal = Mgr.getStoreManager().getBinding(State->getStore(),
                                              LHSVal.castAs<Loc>());
    if (LHSVal.isUnknownOrUndef() || !isa<NonLoc>(LHSVal))
      return false;
  }

  SValBuilder &Bldr = Mgr.getSValBuilder();
  SVal Eval = Bldr.evalBinOp(State, ComparisonOp, LHSVal, RHSVal,
                             Bldr.getConditionType());
  if (Eval.isUnknownOrUndef())
    return false;
  ProgramStateRef StTrue, StFalse;
  std::tie(StTrue, StFalse) = State->assume(Eval.castAs<DefinedSVal>());
  return StTrue && !StFalse;
}

bool CheckerContext::isGreaterOrEqual(const Expr *E, unsigned long long Val) {
  DefinedSVal V = getSValBuilder().makeIntVal(Val, getASTContext().LongLongTy);
  return evalComparison(getSVal(E), BO_GE, V, getState());
}

bool CheckerContext::isNegative(const Expr *E) {
  DefinedSVal V = getSValBuilder().makeIntVal(0, false);
  return evalComparison(getSVal(E), BO_LT, V, getState());
}
