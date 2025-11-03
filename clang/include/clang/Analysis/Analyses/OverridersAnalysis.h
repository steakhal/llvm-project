//===- OverridersAnalysis.h ------------------------------------- -*- C++ --*-//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "clang/CrossTU/CrossTranslationUnit.h"
#include "clang/Support/Compiler.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/Registry.h"

#include <string>
#include <vector>

namespace clang {

struct DirectOverridersDataEntry {
  std::string Overridee;
  std::vector<std::string> OverriddenBy;
};
using DirectOverridersData = std::vector<DirectOverridersDataEntry>;

class OverridersCollector : public ASTConsumer {
public:
  void HandleTranslationUnit(ASTContext &Ctx) override;

  DirectOverridersData getDirectOverriders() const;

private:
  // Populated by HandleTranslationUnit.
  DirectOverridersData DirectOverriders;
};

using MethodVec = llvm::TinyPtrVector<const CXXMethodDecl *>;

class OverridersAnalysis {
public:
  OverridersAnalysis();

  void load(const DirectOverridersData &Data);

  MethodVec getOverriders(const CXXMethodDecl *Method);

private:
  cross_tu::CrossTranslationUnitContext *CTUCtx; // Optional
  llvm::DenseMap<std::string, std::vector<std::string>> DirectOverriders;

  /// This is just a cache for holding the reflexive transitive closure of the
  /// direct mapping.
  llvm::DenseMap<const CXXMethodDecl *, MethodVec> TransitiveOverridersCache;
};

struct AnalysisModule {};
using AnalysisModuleRegistry = llvm::Registry<AnalysisModule>;
} // namespace clang

namespace llvm {
extern template class CLANG_TEMPLATE_ABI Registry<clang::AnalysisModule>;
} // namespace llvm
