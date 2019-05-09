//===--- StrictAliasingCheck.h - clang-tidy----------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_BUGPRONE_STRICTALIASINGCHECK_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_BUGPRONE_STRICTALIASINGCHECK_H

#include "../ClangTidy.h"

namespace clang {
namespace tidy {
namespace bugprone {

/// C++ and C forbids bitcasts (reinterpret casts) to unrelated types.
/// Tries to find some of the bugprone cases using only the static type
/// information.
///
/// It has the following (case-sensitive) checker options:
///   - WarnOnlyIfDereferenced={1,0}, default is 1
///
/// WarnOnlyIfDereferenced:
///   Emit warning even if the pointer is not dereferenced right after the cast
///   operation
///
/// For the user-facing documentation see:
/// http://clang.llvm.org/extra/clang-tidy/checks/bugprone-strict-aliasing.html
class StrictAliasingCheck : public ClangTidyCheck {
public:
  StrictAliasingCheck(StringRef Name, ClangTidyContext *Context);

  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void storeOptions(ClangTidyOptions::OptionMap &Opts) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override;

private:
  const bool WarnOnlyIfDereferenced;
};

} // namespace bugprone
} // namespace tidy
} // namespace clang

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_BUGPRONE_STRICTALIASINGCHECK_H
