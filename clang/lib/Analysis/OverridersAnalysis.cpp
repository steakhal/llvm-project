//===--- OverridersAnalysis.cpp -------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "clang/Analysis/Analyses/OverridersAnalysis.h"
#include "clang/AST/DynamicRecursiveASTVisitor.h"
#include "clang/Index/USRGeneration.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/YAMLTraits.h"

using namespace clang;

namespace {
/// Gathers the polymorphic most derived classes of the TU.
class OverridersCollectorVisitor final : public DynamicRecursiveASTVisitor {
public:
  OverridersCollectorVisitor() {
    ShouldVisitTemplateInstantiations = true;
    ShouldWalkTypesOfTypeLocs = false;
    ShouldVisitImplicitCode = true;
    ShouldVisitLambdaBody = true;
  }

  DirectOverridersData getDirectOverriders() const;

private:
  void recordPotentialRootClass(const CXXRecordDecl *Class);
  bool VisitCXXRecordDecl(CXXRecordDecl *Class) override;

  // FIXME: Avoid deserializing PCH files in the traversal.

  using ClassSet = llvm::DenseSet<const CXXRecordDecl *>;
  ClassSet RootClasses;
  ClassSet HandledClasses;
};

} // namespace

void OverridersCollectorVisitor::recordPotentialRootClass(
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

bool OverridersCollectorVisitor::VisitCXXRecordDecl(CXXRecordDecl *Class) {
  Class = Class->getCanonicalDecl();
  if (!Class->hasDefinition())
    return true;

  recordPotentialRootClass(Class);
  return true;
}

// bool OverridersCollector::HandleTopLevelDecl(DeclGroupRef DG) {
//   for (Decl *D : DG) {
//     if (auto *R = dyn_cast<CXXRecordDecl>(D)) {
//       TraverseDecl(R);
//     }
//   }
//   return true; // Continue parsing.
// }

namespace llvm::yaml {
template <> struct MappingTraits<DirectOverridersDataEntry> {
  static void mapping(IO &IO, DirectOverridersDataEntry &Info) {
    IO.mapRequired("overridee", Info.Overridee);
    IO.mapRequired("overriders", Info.OverriddenBy);
  }
};
} // namespace llvm::yaml

LLVM_YAML_IS_SEQUENCE_VECTOR(DirectOverridersDataEntry)

// Open the file
// Read the file
// Parse the YAML into the data sequence.
// Transform the data sequence into the internal representation, while
//   discarding the invalid entries.

// Do the work.

// Take the internal representation and create a data sequence.
// Open the file
// Format the YAML stream from the data sequence.
// Flush and close the file.

static llvm::Expected<DirectOverridersData>
readOverridersYAML(llvm::StringRef FilePath) {
  auto FileOrError = llvm::MemoryBuffer::getFile(FilePath, /*IsText=*/true);
  if (!FileOrError) {
    return llvm::createFileError(FilePath, FileOrError.getError());
  }
  DirectOverridersData Data;
  llvm::yaml::Input YAML(FileOrError.get()->getBuffer());
  YAML >> Data;
  return std::move(Data);
}

static llvm::Error writeOverridersYAML(DirectOverridersData &Data,
                                       StringRef FilePath) {
  std::error_code EC;
  llvm::raw_fd_ostream OS(FilePath, EC, llvm::sys::fs::OF_TextWithCRLF);
  if (EC) {
    return llvm::createFileError(FilePath, EC);
  }
  llvm::yaml::Output Outs(OS);

  // FIXME: For some reason 'llvm::yaml::Output::operator<<' only accepts
  // mutable reference Data. We should really take 'Data' as const ref.
  Outs << Data;

  return llvm::Error::success();
}

// ASTConsumer -> Data
// CSA, load the yaml and

using MethodVec = llvm::TinyPtrVector<const CXXMethodDecl *>;
using ClassSet = llvm::DenseSet<const CXXRecordDecl *>;
using PotentialOverridersMapping =
    llvm::DenseMap<const CXXMethodDecl *,
                   llvm::SmallSet<const CXXMethodDecl *, 1>>;

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

DirectOverridersData OverridersCollectorVisitor::getDirectOverriders() const {
  PotentialOverridersMapping Mapping =
      calculateDirectOverriderMapping(RootClasses);
  auto generateAllUSRsForDeclsOrNone =
      [](auto &&Decls) -> std::optional<std::vector<std::string>> {
    std::optional<std::vector<std::string>> USRs;
    USRs = std::vector<std::string>();
    USRs->reserve(llvm::adl_size(Decls));
    for (auto const &D : Decls) {
      auto USR = index::generateUSRForDecl(D);
      if (!USR.has_value())
        return std::nullopt;
      USRs->push_back(USR.value());
    }
    return USRs;
  };

  DirectOverridersData Data;
  Data.reserve(Mapping.size());

  for (auto const &[Overridee, Overriders] : Mapping) {
    auto OverrideeUSR = index::generateUSRForDecl(Overridee);
    auto OverriderUSRs = generateAllUSRsForDeclsOrNone(Overriders);
    if (OverrideeUSR.has_value() && OverriderUSRs.has_value()) {
      Data.emplace_back(*std::move(OverrideeUSR), *std::move(OverriderUSRs));
    }
  }

  return Data;
}

void OverridersCollector::HandleTranslationUnit(ASTContext &Ctx) {
  OverridersCollectorVisitor Collector;
  Collector.TraverseAST(Ctx);
  DirectOverriders = Collector.getDirectOverriders();
}

DirectOverridersData OverridersCollector::getDirectOverriders() const {
  return DirectOverriders; // Expensive copy.
}

OverridersAnalysis::OverridersAnalysis() {
  //
}

void OverridersAnalysis::load(const DirectOverridersData &Data) {
  DirectOverriders.reserve(DirectOverriders.size() + Data.size());
  // TODO: Validate.
  for (const auto &[Overridee, Overriders] : Data) {
    DirectOverriders[Overridee] = Overriders;
  }
}

template <class T> static void getAnalysis(StringRef Desired) {
  auto EqualsToDesired = [Desired](const auto &Entry) {
    return Entry.getName() == Desired;
  };
  auto It = llvm::find_if(AnalysisModuleRegistry::entries(), EqualsToDesired);
  std::unique_ptr<PluginASTAction> P = It->instantiate();
}

MethodVec OverridersAnalysis::getOverriders(const CXXMethodDecl *Method) {
  Method = Method->getCanonicalDecl();
  if (!Method->isVirtual()) {
    return {};
  }

  auto [CacheSlot, InsertedIntoCache] =
      TransitiveOverridersCache.try_emplace(Method);
  if (!InsertedIntoCache) {
    return CacheSlot->second;
  }

  if (CTUCtx) {
    extendDirectOverridersFromCTU(D, Analysis);
  }

  llvm::DenseSet<const CXXMethodDecl *> Visited;
  llvm::DenseSet<const CXXMethodDecl *> TransitiveOverriders;
  llvm::SmallVector<const CXXMethodDecl *, 10> BacklogStack;

  BacklogStack.push_back(Method);
  if (!Method->isPureVirtual())
    TransitiveOverriders.insert(Method);

  while (!BacklogStack.empty()) {
    const CXXMethodDecl *Curr = BacklogStack.pop_back_val();
    assert(Curr->isCanonicalDecl());
    if (!Visited.insert(Curr).second) {
      continue;
    }

    auto MethodUSR = index::generateUSRForDecl(Method);
    if (!MethodUSR.has_value()) {
      continue;
    }

    for (const auto &Overrider : DirectOverriders.lookup(*MethodUSR)) {

      BacklogStack.push_back(Overrider);
      TransitiveOverriders.insert(Overrider);
    }
  }

  // Fill the cache.
  llvm::append_range(CacheSlot->second, TransitiveOverriders);

  // Ignore methods that are in a dependent context because they are effectively
  // templates.
  erase_if(CacheSlot->second, std::mem_fn(&CXXMethodDecl::isDependentContext));
  return CacheSlot->second;
}
