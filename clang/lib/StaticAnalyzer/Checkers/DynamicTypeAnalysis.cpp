//===--- DynamicTypeAnalysis.cpp ----------------------------- -*- C++ -*--===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DynamicRecursiveASTVisitor.h"
#include "clang/StaticAnalyzer/Checkers/DynamicType.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CallEvent.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramState.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/Compiler.h"

using namespace clang;
using namespace ento;

using MethodVec = llvm::TinyPtrVector<const CXXMethodDecl *>;
using ClassSet = llvm::DenseSet<const CXXRecordDecl *>;
using PotentialOverridersMapping =
    llvm::DenseMap<const CXXMethodDecl *,
                   llvm::SmallSet<const CXXMethodDecl *, 1>>;
using ClassToClassesMapping = llvm::DenseMap<const CXXRecordDecl *, ClassSet>;

namespace {
class RootClassesCollector : private DynamicRecursiveASTVisitor {
public:
  /// Gathers the polymorphic most derived classes of the TU.
  static ClassSet collect(ASTContext &AST) {
    RootClassesCollector Collector(AST);
    return Collector.RootClasses;
  }

private:
  explicit RootClassesCollector(ASTContext &AST);
  void recordPotentialRootClass(const CXXRecordDecl *Class);
  bool VisitCXXRecordDecl(CXXRecordDecl *Class) override;

  ClassSet RootClasses;
  ClassSet HandledClasses;
};
} // namespace

RootClassesCollector::RootClassesCollector(ASTContext &AST) {
  ShouldVisitTemplateInstantiations = true;
  ShouldWalkTypesOfTypeLocs = false;
  ShouldVisitImplicitCode = true;
  ShouldVisitLambdaBody = true;
  TraverseAST(AST);
  HandledClasses.clear();
}

void RootClassesCollector::recordPotentialRootClass(
    const CXXRecordDecl *Class) {
  assert(Class->isCanonicalDecl());
  assert(Class->hasDefinition());

  if (!HandledClasses.insert(Class).second)
    return;

  // If we have base classes, then exclude all the bases from the
  // potential root class set. We also don't need to visit them later.
  auto RemoveConcreteClasses = [this](const CXXRecordDecl *Base) {
    RootClasses.erase(Base);
    HandledClasses.insert(Base);
    return true; // Continue.
  };

  Class->forallBases(RemoveConcreteClasses);
  RootClasses.insert(Class);
}

bool RootClassesCollector::VisitCXXRecordDecl(CXXRecordDecl *Class) {
  Class = Class->getCanonicalDecl();
  if (!Class->hasDefinition())
    return true;

  recordPotentialRootClass(Class);
  return true;
}

static void collectPotentialOverrides(PotentialOverridersMapping &Mapping,
                                      const CXXRecordDecl *Class) {
  for (const CXXMethodDecl *M : Class->getDefinition()->methods()) {
    for (const CXXMethodDecl *OverriddenMethod : M->overridden_methods()) {
      assert(OverriddenMethod->isCanonicalDecl());
      auto Slot = Mapping.try_emplace(OverriddenMethod).first;
      Slot->second.insert(M);
    }
  }
}

static PotentialOverridersMapping
calculateDirectOverriderMapping(const ClassSet &RootClasses) {
  PotentialOverridersMapping PotentialOverriders;

  for (const CXXRecordDecl *RootClass : RootClasses) {
    assert(RootClass->isCanonicalDecl());
    assert(RootClass->hasDefinition());
    if (!RootClass->isPolymorphic())
      continue;

    RootClass->forallBases([&](const CXXRecordDecl *Base) {
      collectPotentialOverrides(PotentialOverriders, Base);
      return true; // Continue
    });
    collectPotentialOverrides(PotentialOverriders, RootClass);
  }

  return PotentialOverriders;
}

namespace {
class DynamicTypeAnalysisImpl : public DynamicTypeAnalysis {
public:
  explicit DynamicTypeAnalysisImpl(ASTContext &Ctx) {
    ClassSet RootClasses = RootClassesCollector::collect(Ctx);
    DirectlyOverriddenByMap = calculateDirectOverriderMapping(RootClasses);
  }

  LLVM_DUMP_METHOD void dump() const;

  /// Methods overridden by an another method.
  /// E.g. If M overrides M', then this has a mapping from M' to M.
  PotentialOverridersMapping DirectlyOverriddenByMap;

  /// This is just a cache for holding the reflexive transitive closure of the
  /// direct mapping.
  llvm::DenseMap<const CXXMethodDecl *, MethodVec> TransitiveOverridersCache;
};
} // namespace

static DynamicTypeAnalysisImpl &getAnalysis(DynamicTypeAnalysis &Impl) {
  return static_cast<DynamicTypeAnalysisImpl &>(Impl);
}
static DynamicTypeAnalysisImpl &getAnalysis(ProgramStateRef State) {
  return getAnalysis(State->getStateManager().getDynamicTypeAnalysis());
}

static MethodVec getOverridersImpl(DynamicTypeAnalysisImpl &Analysis,
                                   const CXXMethodDecl *D) {
  D = D->getCanonicalDecl();
  if (!D->isVirtual()) {
    return {};
  }

  auto [CacheSlot, InsertedIntoCache] =
      Analysis.TransitiveOverridersCache.try_emplace(D);
  if (!InsertedIntoCache) {
    return CacheSlot->second;
  }

  llvm::DenseSet<const CXXMethodDecl *> Visited;
  llvm::DenseSet<const CXXMethodDecl *> TransitiveOverriders;
  llvm::SmallVector<const CXXMethodDecl *, 10> BacklogStack;

  BacklogStack.push_back(D);
  if (!D->isPureVirtual())
    TransitiveOverriders.insert(D);

  while (!BacklogStack.empty()) {
    const CXXMethodDecl *Curr = BacklogStack.pop_back_val();
    assert(Curr->isCanonicalDecl());
    if (!Visited.insert(Curr).second) {
      continue;
    }

    for (const CXXMethodDecl *Overrider :
         Analysis.DirectlyOverriddenByMap.lookup(Curr)) {
      assert(Overrider->isCanonicalDecl());
      BacklogStack.push_back(Overrider);
      TransitiveOverriders.insert(Overrider);
    }
  }

  // Fill the cache.
  llvm::append_range(CacheSlot->second, TransitiveOverriders);
  return CacheSlot->second;
}

void DynamicTypeAnalysisImpl::dump() const {
  llvm::errs() << "DirectlyOverriddenByMap: " << DirectlyOverriddenByMap.size()
               << "\n";
  for (const auto &[Method, OverriddenBy] : DirectlyOverriddenByMap) {
    llvm::errs() << "  " << Method->getQualifiedNameAsString() << ": [ ";
    llvm::interleaveComma(OverriddenBy, llvm::errs(),
                          [](const CXXMethodDecl *M) {
                            llvm::errs() << M->getParent()->getNameAsString();
                          });
    llvm::errs() << " ]\n";
  }
}

/// Implement the public APIs
/// -------------------------

std::unique_ptr<DynamicTypeAnalysis>
ento::createDynamicTypeAnalysis(ASTContext &Ctx) {
  return std::make_unique<DynamicTypeAnalysisImpl>(Ctx);
}

MethodVec ento::getOverriders(const CXXInstanceCall &Call) {
  return getOverridersImpl(getAnalysis(Call.getState()),
                           cast<CXXMethodDecl>(Call.getDecl()));
}

MethodVec ento::getOverriders(ProgramStateRef State,
                              const CXXMethodDecl *Method) {
  return getOverridersImpl(getAnalysis(State), Method);
}

MethodVec ento::getOverriders(DynamicTypeAnalysis &Analysis,
                              const CXXMethodDecl *Method) {
  return getOverridersImpl(getAnalysis(Analysis), Method);
}
