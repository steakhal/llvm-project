//===--- IncorrectPointerCastCheck.h - clang-tidy----------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_MISC_INCORRECTPOINTERCASTCHECK_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_MISC_INCORRECTPOINTERCASTCHECK_H

#include "../ClangTidy.h"

namespace clang {
namespace tidy {
namespace misc {

/// Warns for cases when pointer is cast and the pointed to type is incompatible
/// with allocated memory area type. This may lead to access memory based on
/// invalid memory layout.
///
/// For the user-facing documentation see:
/// http://clang.llvm.org/extra/clang-tidy/checks/misc-incorrect-pointer-cast.html
class IncorrectPointerCastCheck : public ClangTidyCheck {
public:
  IncorrectPointerCastCheck(StringRef Name, ClangTidyContext *Context)
      : ClangTidyCheck(Name, Context),
        WarnForDifferentSignedness(
            Options.get("WarnForDifferentSignedness", false)),
        IgnoreReinterpretCast(Options.get("IgnoreReinterpretCast", false)) {}
  void storeOptions(ClangTidyOptions::OptionMap &Opts) override;
  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override;

private:
  /// This option can be configured to warn when the pointed to type signedness
  /// is different from the allocated type. The default is false because this
  /// option might be noisy on some code bases.
  const bool WarnForDifferentSignedness;
  /// This option can be configured to do not warn when reinterpter cast is
  /// used. The default is false.
  const bool IgnoreReinterpretCast;
};

} // namespace misc
} // namespace tidy
} // namespace clang

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_MISC_INCORRECTPOINTERCASTCHECK_H
