//===-- AnalysisManager.cpp -------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "clang/StaticAnalyzer/Core/PathSensitive/AnalysisManager.h"
#include "clang/Basic/SourceManager.h"

using namespace clang;
using namespace ento;

void AnalysisManager::anchor() { }

AnalysisManager::AnalysisManager(ASTContext &ASTCtx, Preprocessor &PP,
                                 const PathDiagnosticConsumers &PDC,
                                 StoreManagerCreator storemgr,
                                 ConstraintManagerCreator constraintmgr,
                                 CheckerManager *checkerMgr,
                                 AnalyzerOptions &Options,
                                 CodeInjector *injector)
    : AnaCtxMgr(
          ASTCtx, Options.UnoptimizedCFG,
          Options.ShouldIncludeImplicitDtorsInCFG,
          /*addInitializers=*/true,
          Options.ShouldIncludeTemporaryDtorsInCFG,
          Options.ShouldIncludeLifetimeInCFG,
          // Adding LoopExit elements to the CFG is a requirement for loop
          // unrolling.
          Options.ShouldIncludeLoopExitInCFG ||
            Options.ShouldUnrollLoops,
          Options.ShouldIncludeScopesInCFG,
          Options.ShouldSynthesizeBodies,
          Options.ShouldConditionalizeStaticInitializers,
          /*addCXXNewAllocator=*/true,
          Options.ShouldIncludeRichConstructorsInCFG,
          Options.ShouldElideConstructors,
          /*addVirtualBaseBranches=*/true,
          injector),
      Ctx(ASTCtx), PP(PP), LangOpts(ASTCtx.getLangOpts()),
      PathConsumers(PDC), CreateStoreMgr(storemgr),
      CreateConstraintMgr(constraintmgr), CheckerMgr(checkerMgr),
      options(Options) {
  AnaCtxMgr.getCFGBuildOptions().setAllAlwaysAdd();
  AnaCtxMgr.getCFGBuildOptions().OmitImplicitValueInitializers = true;
  AnaCtxMgr.getCFGBuildOptions().AddCXXDefaultInitExprInAggregates =
      Options.ShouldIncludeDefaultInitForAggregates;
}

AnalysisManager::~AnalysisManager() {
  FlushDiagnostics();
  for (PathDiagnosticConsumer *Consumer : PathConsumers) {
    delete Consumer;
  }
}

void AnalysisManager::FlushDiagnostics() {
  PathDiagnosticConsumer::FilesMade filesMade;
  for (PathDiagnosticConsumer *Consumer : PathConsumers) {
    Consumer->FlushDiagnostics(&filesMade);
  }
}

bool AnalysisManager::isInCodeFile(SourceLocation SL, const SourceManager &SM) {
  if (SM.isInMainFile(SL))
    return true;

  // Support the "unified sources" compilation method (eg. WebKit) that
  // involves producing non-header files that include other non-header files.
  // We should be included directly from a UnifiedSource* file
  // and we shouldn't be a header - which is a very safe defensive check.
  SourceLocation IL = SM.getIncludeLoc(SM.getFileID(SL));
  if (!IL.isValid() || !SM.isInMainFile(IL))
    return false;
  // Should rather be "file name starts with", but the current .getFilename
  // includes the full path.
  if (SM.getFilename(IL).contains("UnifiedSource")) {
    // It might be great to reuse FrontendOptions::getInputKindForExtension()
    // but for now it doesn't discriminate between code and header files.
    return llvm::StringSwitch<bool>(SM.getFilename(SL).rsplit('.').second)
        .Cases("c", "m", "mm", "C", "cc", "cp", true)
        .Cases("cpp", "CPP", "c++", "cxx", "cppm", true)
        .Default(false);
  }

  return false;
}
