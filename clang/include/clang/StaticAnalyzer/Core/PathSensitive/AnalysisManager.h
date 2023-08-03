//== AnalysisManager.h - Path sensitive analysis data manager ------*- C++ -*-//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the AnalysisManager class that manages the data and policy
// for path sensitive analysis.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_STATICANALYZER_CORE_PATHSENSITIVE_ANALYSISMANAGER_H
#define LLVM_CLANG_STATICANALYZER_CORE_PATHSENSITIVE_ANALYSISMANAGER_H

#include "clang/Analysis/AnalysisDeclContext.h"
#include "clang/Analysis/PathDiagnostic.h"
#include "clang/StaticAnalyzer/Core/AnalyzerOptions.h"
#include "clang/StaticAnalyzer/Core/BugReporter/BugReporter.h"
#include "clang/StaticAnalyzer/Core/PathDiagnosticConsumers.h"

namespace clang {

class CodeInjector;

namespace ento {
  class CheckerManager;

class AnalysisManager : public BugReporterData {
  virtual void anchor();
  AnalysisDeclContextManager AnaCtxMgr;

  ASTContext &Ctx;
  Preprocessor &PP;
  const LangOptions &LangOpts;
  PathDiagnosticConsumers PathConsumers;

  // Configurable components creators.
  StoreManagerCreator CreateStoreMgr;
  ConstraintManagerCreator CreateConstraintMgr;

  CheckerManager *CheckerMgr;

public:
  AnalyzerOptions &options;

  AnalysisManager(ASTContext &ctx, Preprocessor &PP,
                  const PathDiagnosticConsumers &Consumers,
                  StoreManagerCreator storemgr,
                  ConstraintManagerCreator constraintmgr,
                  CheckerManager *checkerMgr, AnalyzerOptions &Options,
                  CodeInjector *injector = nullptr);

  ~AnalysisManager() override;

  void ClearContexts() {
    AnaCtxMgr.clear();
  }

  AnalysisDeclContextManager& getAnalysisDeclContextManager() {
    return AnaCtxMgr;
  }

  Preprocessor &getPreprocessor() override { return PP; }

  StoreManagerCreator getStoreManagerCreator() {
    return CreateStoreMgr;
  }

  AnalyzerOptions& getAnalyzerOptions() override {
    return options;
  }

  ConstraintManagerCreator getConstraintManagerCreator() {
    return CreateConstraintMgr;
  }

  CheckerManager *getCheckerManager() const { return CheckerMgr; }

  ASTContext &getASTContext() override {
    return Ctx;
  }

  SourceManager &getSourceManager() override {
    return getASTContext().getSourceManager();
  }

  const LangOptions &getLangOpts() const {
    return LangOpts;
  }

  ArrayRef<PathDiagnosticConsumer*> getPathDiagnosticConsumers() override {
    return PathConsumers;
  }

  void FlushDiagnostics();

  bool shouldVisualize() const {
    return options.visualizeExplodedGraphWithGraphViz;
  }

  bool shouldInlineCall() const {
    return options.getIPAMode() != IPAK_None;
  }

  CFG *getCFG(Decl const *D) {
    return AnaCtxMgr.getContext(D)->getCFG();
  }

  template <typename T>
  T *getAnalysis(Decl const *D) {
    return AnaCtxMgr.getContext(D)->getAnalysis<T>();
  }

  ParentMap &getParentMap(Decl const *D) {
    return AnaCtxMgr.getContext(D)->getParentMap();
  }

  AnalysisDeclContext *getAnalysisDeclContext(const Decl *D) {
    return AnaCtxMgr.getContext(D);
  }

  static bool isInCodeFile(SourceLocation SL, const SourceManager &SM);

  bool isInCodeFile(SourceLocation SL) {
    const SourceManager &SM = getASTContext().getSourceManager();
    return isInCodeFile(SL, SM);
  }
};

} // enAnaCtxMgrspace

} // end clang namespace

#endif
