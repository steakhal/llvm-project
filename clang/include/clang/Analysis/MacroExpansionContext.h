//===- MacroExpansionContext.h - Macro expansion information ----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_ANALYSIS_MACROEXPANSIONCONTEXT_H
#define LLVM_CLANG_ANALYSIS_MACROEXPANSIONCONTEXT_H

#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Lex/Preprocessor.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"

namespace clang {

/// MacroExpansionContext tracks the macro expansions processed by the
/// Preprocessor. It means that it can track source locations from a single
/// translation unit. For every macro expansion it can tell you what text will
/// be substituted.
///
/// It was designed to deal with:
///  - regular macros
///  - macro functions
///  - variadic macros
///  - transitive macro expansions
///  - macro redefinition
///  - unbalanced parenthesis
///
/// \code{.c}
///   void bar();
///   #define retArg(x) x
///   #define retArgUnclosed retArg(bar()
///   #define BB CC
///   #define applyInt BB(int)
///   #define CC(x) retArgUnclosed
///
///   void unbalancedMacros() {
///     applyInt  );
///   //^~~~~~~~~~^ is the substituted range
///   // Substituted text is "applyInt  )"
///   // Expanded text is "bar()"
///   }
///
///   #define expandArgUnclosedCommaExpr(x) (x, bar(), 1
///   #define f expandArgUnclosedCommaExpr
///
///   void unbalancedMacros2() {
///     int x =  f(f(1))  ));  // Look at the parenthesis!
///   //         ^~~~~~^ is the substituted range
///   // Substituted text is "f(f(1))"
///   // Expanded text is "((1,bar(),1,bar(),1"
///   }
/// \endcode
/// \remark Currently we don't respect the whitespaces between expanded tokens,
///         so the output for this example might differ from the -E compiler
///         invocation.
/// \remark All whitespaces are consumed while constructing the expansion.
///         After all identifier a single space inserted to produce a valid C
///         code even if identifier follows an other identifiers such as
///         variable declarations.
/// \remark MacroExpansionContext object must outlive the Preprocessor
///         parameter.
class MacroExpansionContext {
public:
  /// Register the necessary callbacks to the Preprocessor to record the
  /// expansion events and the generated tokens. Must ensure that this object
  /// outlives the given Preprocessor.
  MacroExpansionContext(Preprocessor &PP, const LangOptions &LangOpts);
  using MacroExpansionText = SmallString<40>;

  /// \param MacroExpansionLoc Must be the expansion location of a macro.
  /// \return The textual representation of the token sequence which were
  ///         substituted in place of the macro.
  ///         If no macro was expanded at that location, returns an empty
  ///         string.
  MacroExpansionText
  getExpandedMacroForLocation(SourceLocation MacroExpansionLoc) const;

  /// \param MacroExpansionLoc Must be the expansion location of a macro.
  /// \return The text from the original source code which were substituted by
  ///         the macro expansion chain from the given location.
  ///         If no macro was expanded at that location, returns an empty
  ///         string.
  StringRef
  getSubstitutedTextForLocation(SourceLocation MacroExpansionLoc) const;

  LLVM_DUMP_METHOD void dumpExpansionRangesToStream(raw_ostream &OS) const;
  LLVM_DUMP_METHOD void dumpExpandedTextsToStream(raw_ostream &OS) const;
  LLVM_DUMP_METHOD void dumpExpansionRanges() const;
  LLVM_DUMP_METHOD void dumpExpandedTexts() const;

private:
  using ExpansionMap = llvm::DenseMap<SourceLocation, MacroExpansionText>;
  using ExpansionRangeMap = llvm::DenseMap<SourceLocation, SourceLocation>;

  /// Associates the textual representation of the expanded tokens at the given
  /// macro expansion location.
  ExpansionMap ExpandedTokens;

  /// Tracks which source location was the last affected by any macro
  /// substitution starting from a given macro expansion location.
  ExpansionRangeMap ExpansionRanges;

  const Preprocessor &PP;
  const SourceManager &SM;
  const LangOptions &LangOpts;

  /// This callback is called by the preprocessor.
  /// It stores the textual representation of the expanded token sequence for a
  /// macro expansion location.
  void onTokenLexed(const Token &Tok);
};
} // end namespace clang

#endif // LLVM_CLANG_ANALYSIS_MACROEXPANSIONCONTEXT_H
