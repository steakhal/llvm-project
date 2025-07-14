//=== DynamicType.h ------------------------------------------------*- C++ -*-//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_STATICANALYZER_CHECKERS_DYNAMIC_TYPE_H
#define LLVM_CLANG_LIB_STATICANALYZER_CHECKERS_DYNAMIC_TYPE_H

#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramState_Fwd.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/TinyPtrVector.h"
#include <memory>

namespace clang {
class AnalyzerOptions;
class ASTConsumer;
class ASTContext;
class CXXMethodDecl;
} // namespace clang

namespace clang::cross_tu {
class CrossTranslationUnitContext;
} // namespace clang::cross_tu

namespace clang::ento {
class CXXInstanceCall;
class DynamicTypeAnalysis;

llvm::TinyPtrVector<const CXXMethodDecl *>
getOverriders(const CXXInstanceCall &Call);
llvm::TinyPtrVector<const CXXMethodDecl *>
getOverriders(ProgramStateRef State, const CXXMethodDecl *Method);
llvm::TinyPtrVector<const CXXMethodDecl *>
getOverriders(DynamicTypeAnalysis &DynTyAnalysis, const CXXMethodDecl *Method);

// Details:
class DynamicTypeAnalysis {
public:
  virtual ~DynamicTypeAnalysis() = default;
};
DynamicTypeAnalysis &
attachDynamicTypeAnalysis(std::vector<std::unique_ptr<ASTConsumer>> &Consumers);

void dumpDynamicTypeAnalysis(DynamicTypeAnalysis &Analysis,
                             llvm::StringRef OutputFile);

// Hack
void loadDynamicTypeAnalysis(DynamicTypeAnalysis &Analysis,
                             llvm::StringRef InputFile);

// Hack
void setCTUContext(DynamicTypeAnalysis &Analysis,
                   cross_tu::CrossTranslationUnitContext &CTUContext);
void setOpts(DynamicTypeAnalysis &Analysis, const AnalyzerOptions &Opts);

} // namespace clang::ento

#endif // LLVM_CLANG_LIB_STATICANALYZER_CHECKERS_DYNAMIC_TYPE_H
