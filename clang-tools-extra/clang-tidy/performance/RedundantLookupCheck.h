//===--- RedundantLookupCheck.h - clang-tidy --------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_PERFORMANCE_REDUNDANTLOOKUPCHECK_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_PERFORMANCE_REDUNDANTLOOKUPCHECK_H

#include "../ClangTidyCheck.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"
#include "llvm/ADT/SmallPtrSet.h"

namespace clang {
class SourceManager;
} // namespace clang

namespace clang::tidy::performance {

/// Detects redundant container lookups.
///
/// For the user-facing documentation see:
/// http://clang.llvm.org/extra/clang-tidy/checks/performance/redundant-lookup.html
class RedundantLookupCheck : public ClangTidyCheck {
public:
  RedundantLookupCheck(StringRef Name, ClangTidyContext *Context);
  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override;
  void onEndOfTranslationUnit() override;
  void storeOptions(ClangTidyOptions::OptionMap &Opts) override;
  bool isLanguageVersionSupported(const LangOptions &LangOpts) const override {
    return LangOpts.CPlusPlus;
  }

  struct LookupContext {
    const FunctionDecl *Context;
    unsigned Hash;
  };

private:
  llvm::DenseMap<LookupContext, llvm::SmallPtrSet<const CallExpr *, 2>>
      RegisteredLookups;
  const StringRef ContainerNameRegex;
  const std::vector<StringRef> LookupMethodNames;
  ASTContext *Ctx = nullptr;
};

} // namespace clang::tidy::performance

namespace llvm {
template <>
struct DenseMapInfo<
    clang::tidy::performance::RedundantLookupCheck::LookupContext> {
  using LookupContext =
      clang::tidy::performance::RedundantLookupCheck::LookupContext;
  static inline LookupContext getEmptyKey() { return {nullptr, ~0U}; }
  static inline LookupContext getTombstoneKey() { return {nullptr, ~0U - 1}; }
  static unsigned getHashValue(const LookupContext &Val) {
    return Val.Hash * 37U;
  }
  static bool isEqual(const LookupContext &LHS, const LookupContext &RHS) {
    return LHS.Hash == RHS.Hash;
  }
};
} // namespace llvm

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_PERFORMANCE_REDUNDANTLOOKUPCHECK_H
