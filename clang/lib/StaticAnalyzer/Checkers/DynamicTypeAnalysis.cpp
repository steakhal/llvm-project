//===--- DynamicTypeAnalysis.cpp ----------------------------- -*- C++ -*--===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/CXXInheritance.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DynamicRecursiveASTVisitor.h"
#include "clang/CrossTU/CrossTranslationUnit.h"
#include "clang/Index/USRGeneration.h"
#include "clang/StaticAnalyzer/Checkers/DynamicType.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/AnalysisManager.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CallEvent.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramState.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/VirtualFileSystem.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/raw_ostream.h"
#include <functional>
#include <memory>
#include <unordered_map>
#include <vector>

using namespace clang;
using namespace ento;

using MethodVec = llvm::TinyPtrVector<const CXXMethodDecl *>;
using ClassSet = llvm::DenseSet<const CXXRecordDecl *>;
using PotentialOverridersMapping =
    llvm::DenseMap<const CXXMethodDecl *,
                   llvm::SmallSet<const CXXMethodDecl *, 1>>;
using ClassToClassesMapping = llvm::DenseMap<const CXXRecordDecl *, ClassSet>;

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

static std::string getUSRForDecl(const Decl *Decl) {
  llvm::SmallString<128> Buff;
  if (!Decl || index::generateUSRForDecl(Decl, Buff))
    return "";
  return std::string(Buff);
}

namespace {
class DynamicTypeAnalysisImpl final : public DynamicTypeAnalysis,
                                      public RootClassesCollector {
public:
  void HandleTranslationUnit(ASTContext &Ctx) override {
    TraverseAST(Ctx);
    HandledClasses.clear(); // We no longer need this - the traversal is done.
    DirectlyOverriddenByMap = calculateDirectOverriderMapping(RootClasses);
  }

  LLVM_DUMP_METHOD void dump() const;

  cross_tu::CrossTranslationUnitContext *CTUContext = nullptr;
  const AnalyzerOptions *Opts = nullptr;

  /// Methods overridden by an another method.
  /// E.g. If M overrides M', then this has a mapping from M' to M.
  PotentialOverridersMapping DirectlyOverriddenByMap;

  /// Same as \p DirectlyOverriddenByMap, but with USRs as keys.
  std::unordered_map<std::string, llvm::SmallSet<std::string, 1>>
      DirectlyOverriddenByMapUSRs;

  /// This is just a cache for holding the reflexive transitive closure of the
  /// direct mapping.
  llvm::DenseMap<const CXXMethodDecl *, MethodVec> TransitiveOverridersCache;
};
} // namespace

static DynamicTypeAnalysisImpl &getAnalysis(DynamicTypeAnalysis &Impl) {
  return static_cast<DynamicTypeAnalysisImpl &>(Impl);
}
static DynamicTypeAnalysisImpl &getAnalysis(ProgramStateRef State) {
  return getAnalysis(State->getAnalysisManager().getDynamicTypeAnalysis());
}

static void emitErrors(llvm::Expected<const FunctionDecl *> &CTUDeclOrError,
                       cross_tu::CrossTranslationUnitContext &CTUCtx) {
  auto Handler = [&](const cross_tu::IndexError &IE) {
    CTUCtx.emitCrossTUDiagnostics(IE);
  };
  handleAllErrors(CTUDeclOrError.takeError(), Handler);
}

static const CXXMethodDecl *
importOrEmitErrors(const CXXMethodDecl *D,
                   cross_tu::CrossTranslationUnitContext &CTUCtx,
                   const AnalyzerOptions &Opts) {
  llvm::Expected<const FunctionDecl *> CTUDeclOrError =
      CTUCtx.getCrossTUDefinition(D, Opts.CTUDir, Opts.CTUIndexName,
                                  Opts.DisplayCTUProgress);
  if (!CTUDeclOrError) {
    emitErrors(CTUDeclOrError, CTUCtx);
    return nullptr;
  }
  return dyn_cast<CXXMethodDecl>(CTUDeclOrError.get());
}

static const CXXMethodDecl *
importOrEmitErrors(StringRef MethodUSR,
                   cross_tu::CrossTranslationUnitContext &CTUCtx,
                   const AnalyzerOptions &Opts) {
  llvm::Expected<const FunctionDecl *> CTUDeclOrError =
      CTUCtx.getCrossTUDefinition(MethodUSR, Opts.CTUDir, Opts.CTUIndexName,
                                  Opts.DisplayCTUProgress);
  if (!CTUDeclOrError) {
    emitErrors(CTUDeclOrError, CTUCtx);
    return nullptr;
  }
  return dyn_cast<CXXMethodDecl>(CTUDeclOrError.get());
}

static void extendDirectOverridersFromCTU(const CXXMethodDecl *D,
                                          DynamicTypeAnalysisImpl &Analysis) {
  auto &CTUCtx = *Analysis.CTUContext;
  auto &Opts = *Analysis.Opts;

  if (!D->hasBody()) {
    importOrEmitErrors(D, CTUCtx, Opts);
  }

  std::string SubjectUSR = getUSRForDecl(D);
  if (auto It = Analysis.DirectlyOverriddenByMapUSRs.find(SubjectUSR);
      It != Analysis.DirectlyOverriddenByMapUSRs.end()) {
    auto &DirectOverriders =
        Analysis.DirectlyOverriddenByMap.try_emplace(D->getCanonicalDecl())
            .first->second;

    for (const auto &DirectOverriderUSR : It->second) {
      if (const CXXMethodDecl *OverriderD =
              importOrEmitErrors(DirectOverriderUSR, CTUCtx, Opts)) {
        llvm::errs() << "  Overridden by "
                     << OverriderD->getQualifiedNameAsString() << "\n";
        DirectOverriders.insert(OverriderD->getCanonicalDecl());
      } else {
        llvm::errs() << "  Failed to import method with USR '"
                     << DirectOverriderUSR << "'\n";
      }
    }
  }
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

  if (Analysis.Opts && Analysis.Opts->IsNaiveCTUEnabled) {
    extendDirectOverridersFromCTU(D, Analysis);
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

  // Ignore methods that are in a dependent context because they are effectively
  // templates.
  erase_if(CacheSlot->second, std::mem_fn(&CXXMethodDecl::isDependentContext));
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

DynamicTypeAnalysis &ento::attachDynamicTypeAnalysis(
    std::vector<std::unique_ptr<ASTConsumer>> &Consumers) {
  auto Data = readOverridersYAML("/home/steak/git/llvm-project/yaa.yaml");
  if (!Data) {
    llvm::logAllUnhandledErrors(Data.takeError(), llvm::errs());
  }

  for (const auto &R : *Data) {
    llvm::errs() << "R.Overridee: " << R.Overridee << ": [";
    llvm::interleaveComma(R.OverriddenBy, llvm::errs());
    llvm::errs() << "]\n";
  }

  llvm::Error Err =
      writeOverridersYAML(*Data, "/home/steak/git/llvm-project/outt.yaml");
  if (Err) {
    llvm::logAllUnhandledErrors(std::move(Err), llvm::errs());
  }

  // NE.push_back(DirectOverridersDataEntry{"newusr", {"other", "other2"}});
  // llvm::yaml::Output Outs(llvm::errs() << "YAML out:\n");
  // Outs << NE;
  std::abort();

  Consumers.push_back(std::make_unique<DynamicTypeAnalysisImpl>());
  return *static_cast<DynamicTypeAnalysisImpl *>(Consumers.back().get());
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

void ento::dumpDynamicTypeAnalysis(DynamicTypeAnalysis &Analysis,
                                   llvm::StringRef OutputFile) {
  assert(!OutputFile.empty());
  llvm::errs() << "Dumping dynamic type analysis to " << OutputFile << "\n";

  std::error_code EC;
  llvm::raw_fd_ostream Out(OutputFile, EC);
  if (EC) {
    llvm::errs() << "Error opening output file '" << OutputFile
                 << "': " << EC.message() << "\n";
    return;
  }

  llvm::DenseMap<const CXXMethodDecl *, std::string> USRCache;
  auto GetCachedUSRForDecl = [&USRCache](const CXXMethodDecl *D) {
    auto [Slot, Inserted] = USRCache.try_emplace(D);
    if (Inserted)
      Slot->second = getUSRForDecl(D);
    return Slot->second;
  };

  for (const auto &[Method, OverriddenBy] :
       getAnalysis(Analysis).DirectlyOverriddenByMap) {
    Out << OverriddenBy.size() << " " << GetCachedUSRForDecl(Method) << "\n";
    for (const CXXMethodDecl *M : OverriddenBy) {
      Out << GetCachedUSRForDecl(M) << "\n";
    }
  }
}

void ento::setCTUContext(DynamicTypeAnalysis &Analysis,
                         cross_tu::CrossTranslationUnitContext &CTUContext) {
  getAnalysis(Analysis).CTUContext = &CTUContext;
}

void ento::setOpts(DynamicTypeAnalysis &Analysis, const AnalyzerOptions &Opts) {
  getAnalysis(Analysis).Opts = &Opts;
}

void ento::loadDynamicTypeAnalysis(DynamicTypeAnalysis &Analysis,
                                   llvm::StringRef InputFile) {
  if (InputFile.empty())
    return;
  llvm::errs() << "Loading dynamic type analysis from " << InputFile << "\n";

  auto BufOrErr = llvm::MemoryBuffer::getFile(InputFile);
  if (!BufOrErr) {
    llvm::errs() << "Error opening input file '" << InputFile
                 << "': " << BufOrErr.getError().message() << "\n";
    return;
  }
  llvm::StringRef Buf = (**BufOrErr).getBuffer();

  auto &DynTyAnalysis = getAnalysis(Analysis);
  auto IsNewLine = [](char c) { return c == '\n'; };
  DynTyAnalysis.DirectlyOverriddenByMapUSRs.clear();

  unsigned NumPotentialOverriders = 0;
  while (!Buf.consumeInteger(/*Radix=*/10, NumPotentialOverriders)) {
    if (!Buf.consume_front(" ")) { // Consume the space before the USR.
      llvm::errs() << "Expected a space characted; abort.\n";
      return;
    }
    StringRef SubjectUSR = Buf.take_until(IsNewLine);
    Buf = Buf.drop_front(SubjectUSR.size() + 1); // +1 for \n.
    auto [Place, Inserted] =
        DynTyAnalysis.DirectlyOverriddenByMapUSRs.try_emplace(SubjectUSR.str());

    if (!Inserted) {
      llvm::errs() << "Subject USR was already present: " << SubjectUSR << "\n";
    }

    auto &OverriddenByUSRs = Place->second;
    for (unsigned i = 0; i < NumPotentialOverriders; ++i) {
      StringRef OverriderUSR = Buf.take_until(IsNewLine);
      Buf = Buf.drop_front(OverriderUSR.size() + 1); // +1 for \n.

      auto [Place, Inserted] = OverriddenByUSRs.insert(OverriderUSR.str());
      if (!Inserted) {
        llvm::errs() << "Overrider USR " << OverriderUSR
                     << " was already present inside SubjectUSR " << SubjectUSR
                     << "\n";
      }
    }
  }
  // llvm::errs() << "Consumed the whole metadata file? " << Buf.empty() <<
  // "\n";

  DynTyAnalysis.dump();
}
