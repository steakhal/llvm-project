//===-- RedundantLookupChecker.cpp -------------------------------------------//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/Type.h"
#include "clang/StaticAnalyzer/Checkers/BuiltinCheckerRegistration.h"
#include "clang/StaticAnalyzer/Core/BugReporter/BugReporter.h"
#include "clang/StaticAnalyzer/Core/Checker.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CallDescription.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CallEvent.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ExplodedGraph.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/MemRegion.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramStateTrait.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramState_Fwd.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/SVals.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SetOperations.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SmallVectorExtras.h"
#include "llvm/Support/raw_ostream.h"
#include <tuple>

using namespace clang;
using namespace ento;

namespace {
class RedundantLookupChecker
    : public Checker<check::PreCall, check::PostCall, check::RegionChanges> {
public:
  ProgramStateRef
  checkRegionChanges(ProgramStateRef State,
                     const InvalidatedSymbols *Invalidated,
                     ArrayRef<const MemRegion *> ExplicitRegions,
                     ArrayRef<const MemRegion *> Regions,
                     const LocationContext *LCtx, const CallEvent *Call) const;
  void checkPreCall(const CallEvent &Call, CheckerContext &C) const;
  void checkPostCall(const CallEvent &Call, CheckerContext &C) const;
  void printState(raw_ostream &Out, ProgramStateRef State, const char *NL,
                  const char *Sep) const override;

  const BugType RedundantLookupBug{this,
                                   "Redundant lookup with the same value"};
};
} // namespace

static bool hasIdenticalConstOverload(const CXXMethodDecl *D) {
  auto TypeOf = [](const ParmVarDecl *P) { return P->getType(); };
  auto GetTypesFromSignature = [&TypeOf](const CXXMethodDecl *D) {
    return std::make_pair(map_to_vector(D->parameters(), TypeOf),
                          D->getReturnType());
  };

  const DeclarationName Name = D->getDeclName();
  const CXXRecordDecl *R = D->getParent();
  const auto Types = GetTypesFromSignature(D);

  auto IsDropInReplacementAsConst = [&](const CXXMethodDecl *M) {
    return M->isConst()                // We are looking for const overloads.
           && M->getDeclName() == Name // Of the same overload set.
           && M != D                   // Different than the original method.
           && GetTypesFromSignature(M) == Types; // Matching the same signature.
  };

  bool HasConstOverload =
      !make_filter_range(R->methods(), IsDropInReplacementAsConst).empty();
  return HasConstOverload;
}

static bool isConstOrHasIdenticalConstOverload(const CXXMethodDecl *D) {
  return D->isConst() || hasIdenticalConstOverload(D);
}

static bool anyContainsInsensitive(ArrayRef<StringRef> Needles,
                                   StringRef Haystack) {
  return llvm::any_of(Needles, [Haystack](StringRef Needle) {
    return Haystack.contains_insensitive(Needle);
  });
}

static bool hasExactlyOneArg(const CallEvent &Call) {
  const auto *D = dyn_cast_if_present<FunctionDecl>(Call.getDecl());
  return D && D->getNumParams() == Call.getNumArgs() && Call.getNumArgs() == 1;
}

static bool isContainer(const CXXRecordDecl *R) {
  return anyContainsInsensitive({"map", "set"}, R->getName());
}

static bool isContainer(QualType Ty) {
  if (Ty.isNull())
    return false;
  const auto *R = Ty->getAsCXXRecordDecl();
  return R && isContainer(R);
}

static bool isPointeeContainer(QualType Ty) {
  if (Ty.isNull())
    return false;
  return isContainer(Ty->getPointeeType());
}

static bool isContainerLookup(const CXXMethodDecl *D) {
  if (!isContainer(D->getParent()))
    return false;

  const StringRef Identifier = [D] {
    const auto *II = D->getDeclName().getAsIdentifierInfo();
    return II ? II->getName() : StringRef();
  }();

  return D->getOverloadedOperator() == OO_Subscript ||
         llvm::is_contained({"contains", "count"}, Identifier);
}

static bool isContainerLookup(const CallEvent &Call) {
  const auto *D = dyn_cast_if_present<CXXMethodDecl>(Call.getDecl());
  if (!D || !hasExactlyOneArg(Call))
    return false;
  return isContainerLookup(D);
}

REGISTER_MAP_WITH_PROGRAMSTATE(LastLookupKeys, const MemRegion *, SVal)
REGISTER_SET_WITH_PROGRAMSTATE(EverEscapedContainers, const MemRegion *)

struct ContainerCall {
  const CXXInstanceCall &Call;
  const CXXMethodDecl *Method;
  const MemRegion *ObjLValue;
};

static std::optional<ContainerCall> parseCall(const CallEvent &Call) {
  const auto *InstCall = dyn_cast<CXXInstanceCall>(&Call);
  const auto *M = dyn_cast<CXXMethodDecl>(Call.getDecl());
  if (!InstCall || !M || !hasExactlyOneArg(Call))
    return std::nullopt;

  const MemRegion *ObjLValue = InstCall->getCXXThisVal().getAsRegion();
  if (!ObjLValue || !isContainerLookup(Call))
    return std::nullopt;
  return ContainerCall{*InstCall, M, ObjLValue};
}

ProgramStateRef RedundantLookupChecker::checkRegionChanges(
    ProgramStateRef State, const InvalidatedSymbols *InvSyms,
    ArrayRef<const MemRegion *> ExplicitRegions,
    ArrayRef<const MemRegion *> Regions, const LocationContext *,
    const CallEvent *Call) const {
  const bool DoInvalidate = [&] {
    if (!Call)
      return true;
    const auto *M = dyn_cast<CXXMethodDecl>(Call->getDecl());
    return !M || !isConstOrHasIdenticalConstOverload(M);
  }();

  if (!DoInvalidate) {
    return State;
  }

  // // Ignore the container calls
  // if (Call && parseCall(*Call).has_value()) {
  //   return State;
  // }

  // if (Call) {
  //   llvm::errs() << "checkRegionChanges of a call: ";
  //   Call->dump();
  //   llvm::errs() << "\n";

  //   llvm::SmallPtrSet<const MemRegion *, 10> Combined;
  //   Combined.insert(Regions.begin(), Regions.end());
  //   Combined.insert(ExplicitRegions.begin(), ExplicitRegions.end());
  //   for (const MemRegion *R : Combined) {
  //     llvm::errs() << "< " << R << "\n";
  //   }

  //   for (SymbolRef Sym : *InvSyms) {
  //     llvm::errs() << "+ " << Sym << "\n";
  //   }
  // }

  auto TrackedRegions = llvm::make_first_range(State->get<LastLookupKeys>());

  llvm::SmallPtrSet<const MemRegion *, 10> TrackedSet;
  TrackedSet.insert(TrackedRegions.begin(), TrackedRegions.end());

  llvm::SmallPtrSet<const MemRegion *, 10> InvalidatedRegions;
  InvalidatedRegions.insert(Regions.begin(), Regions.end());
  InvalidatedRegions.insert(ExplicitRegions.begin(), ExplicitRegions.end());

  // auto ID = State->getID();

  const ASTContext &Ctx = State->getStateManager().getContext();

  for (const MemRegion *R : InvalidatedRegions) {
    // llvm::errs() << R << ": " << loc::MemRegionVal{R}.getType(Ctx) << "\n";
    if (isPointeeContainer(loc::MemRegionVal{R}.getType(Ctx))) {
      // llvm::errs() << "ESCAPING container region: " << R << "in " << ID <<
      // "\n";
      State = State->add<EverEscapedContainers>(R);
    }
  }

  for (const MemRegion *R : set_intersection(TrackedSet, InvalidatedRegions)) {
    // llvm::errs() << "Removed the lookup from State " << ID << "\n";
    State = State->remove<LastLookupKeys>(R);
    State = State->add<EverEscapedContainers>(R);
    // Call->dump();
  }
  return State;
}

void RedundantLookupChecker::checkPreCall(const CallEvent &Call,
                                          CheckerContext &C) const {
  auto ParsedCall = parseCall(Call);
  if (!ParsedCall)
    return;

  const auto &[InstCall, Method, ObjLValue] = *ParsedCall;
  ProgramStateRef State = C.getState();

  SVal LookupKeyValue = InstCall.getArgSVal(0);

  if (const SVal *PrevLookupKey = State->get<LastLookupKeys>(ObjLValue);
      PrevLookupKey && *PrevLookupKey == LookupKeyValue) {
    State = State->remove<LastLookupKeys>(ObjLValue);
    if (const ExplodedNode *N = C.generateNonFatalErrorNode(State)) {
      auto R = std::make_unique<PathSensitiveBugReport>(
          RedundantLookupBug, RedundantLookupBug.getDescription(), N);
      R->markInteresting(ObjLValue);
      C.emitReport(std::move(R));
      return;
    }
  }

  State = State->set<LastLookupKeys>(ObjLValue, LookupKeyValue);
  const auto *Tag = C.getNoteTag([=, ObjLValue = ObjLValue](
                                     PathSensitiveBugReport &R) -> std::string {
    if (&R.getBugType() != &RedundantLookupBug || !R.isInteresting(ObjLValue))
      return "";
    R.markNotInteresting(ObjLValue);
    return "Previous lookup with the same value was here";
  });
  C.addTransition(State, Tag);
}

/// If we encounter an opaque call, let's drop the previously escaped containers
/// as a conservative measure. Remember, once a container's address escapes, any
/// opaque call may mutate it.
void RedundantLookupChecker::checkPostCall(const CallEvent &Call,
                                           CheckerContext &C) const {
  // Ignore calls that didn't trigger conservative eval calls.
  // And ignore instance calls because those are unlikely that would fiddle with
  // global variables.
  if (C.wasInlined || isa<CXXInstanceCall>(Call))
    return;

  ProgramStateRef State = C.getState();
  for (const MemRegion *R : State->get<EverEscapedContainers>())
    State = State->remove<LastLookupKeys>(R);

  if (C.getState() != State)
    C.addTransition(State);
}

void RedundantLookupChecker::printState(raw_ostream &Out, ProgramStateRef State,
                                        const char *NL, const char *Sep) const {
  if (auto Map = State->get<LastLookupKeys>(); !Map.isEmpty()) {
    Out << Sep << "LastLookupKeys :" << NL;
    for (const auto &[R, V] : Map) {
      Out << R << " : " << V << NL;
    }
  }

  if (auto Map = State->get<EverEscapedContainers>(); !Map.isEmpty()) {
    Out << Sep << "EverEscapedContainers :" << NL;
    for (const MemRegion *R : Map) {
      Out << R << NL;
    }
  }
}

void ento::registerRedundantLookupChecker(CheckerManager &Mgr) {
  Mgr.registerChecker<RedundantLookupChecker>();
}

bool ento::shouldRegisterRedundantLookupChecker(const CheckerManager &Mgr) {
  return Mgr.getLangOpts().CPlusPlus;
}
