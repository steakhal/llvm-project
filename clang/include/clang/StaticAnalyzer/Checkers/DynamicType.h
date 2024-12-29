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
#include "llvm/ADT/TinyPtrVector.h"
#include <memory>

namespace clang {
class ASTConsumer;
class ASTContext;
class CXXMethodDecl;
} // namespace clang

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

} // namespace clang::ento

#endif // LLVM_CLANG_LIB_STATICANALYZER_CHECKERS_DYNAMIC_TYPE_H
