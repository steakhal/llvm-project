//===--- ObsolescentFunctionsCheck.h - clang-tidy ---------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_CERT_OBSOLESCENTFUNCTIONSCHECK_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_CERT_OBSOLESCENTFUNCTIONSCHECK_H

#include "../ClangTidyCheck.h"

namespace clang {
namespace tidy {
namespace cert {

/// Checks for deprecated and obsolescent function listed in
/// CERT C Coding Standard Recommendation MSC24 - C. For the listed functions,
/// an alternative, safe replacement is suggested if available.
/// The checker heavily relies on the availability of annexK(Bounds - checking
/// interfaces) from C11. For the user-facing documentation see:
/// 
/// http://clang.llvm.org/extra/clang-tidy/checks/cert-msc24-c.html
class ObsolescentFunctionsCheck : public ClangTidyCheck {
public:
  ObsolescentFunctionsCheck(StringRef Name, ClangTidyContext *Context)
      : ClangTidyCheck(Name, Context) {}
  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override;
  void registerPPCallbacks(const SourceManager &SM, Preprocessor *Pp,
                           Preprocessor *ModuleExpanderPP) override;

private:
  SourceLocation
  getSourceLocation(const ast_matchers::MatchFinder::MatchResult &Result);
  bool useSafeFunctionsFromAnnexK();
  std::string getReplacementFunctionName(StringRef FunctionName);
  Preprocessor *PP = nullptr;
};

} // namespace cert
} // namespace tidy
} // namespace clang

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_CERT_OBSOLESCENTFUNCTIONSCHECK_H
