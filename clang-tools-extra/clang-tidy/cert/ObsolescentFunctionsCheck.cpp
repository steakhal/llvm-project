//===--- ObsolescentFunctionsCheck.cpp - clang-tidy -----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "ObsolescentFunctionsCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Lex/PPCallbacks.h"
#include "clang/Lex/Preprocessor.h"
#include <cassert>

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace cert {

namespace {
constexpr llvm::StringLiteral FunctionNamesExprName = "FunctionNamesExpr";
constexpr llvm::StringLiteral FunctionExprName = "FunctionExpr";
constexpr llvm::StringLiteral FunctionPtrDeclName = "FunctionPtrDecl";

bool doesReplacementFromAnnexKExist(StringRef FunctionName) {
  static constexpr std::array<StringRef, 56>
      FunctionNamesWithAnnexKReplacement = {
          "asctime",   "ctime",     "fopen",     "freopen",   "bsearch",
          "fprintf",   "fscanf",    "fwprintf",  "fwscanf",   "getenv",
          "gmtime",    "localtime", "mbsrtowcs", "mbstowcs",  "memcpy",
          "memmove",   "printf",    "qsort",     "snprintf",  "sprintf",
          "sscanf",    "strcat",    "strcpy",    "strerror",  "strncat",
          "strncpy",   "strtok",    "swprintf",  "swscanf",   "vfprintf",
          "vfscanf",   "vfwprintf", "vfwscanf",  "vprintf",   "vscanf",
          "vsnprintf", "vsprintf",  "vsscanf",   "vswprintf", "vswscanf",
          "vwprintf",  "vwscanf",   "wcrtomb",   "wcscat",    "wcscpy",
          "wcsncat",   "wcsncpy",   "wcsrtombs", "wcstok",    "wcstombs",
          "wctomb",    "wmemcpy",   "wmemmove",  "wprintf",   "wscanf"};

  return std::find(FunctionNamesWithAnnexKReplacement.cbegin(),
                   FunctionNamesWithAnnexKReplacement.cend(),
                   FunctionName) != FunctionNamesWithAnnexKReplacement.cend();
}

std::string getAnnexKReplacementFunctionName(StringRef FunctionName) {
  return std::string(FunctionName) + "_s";
}

StringRef getRationale(StringRef FunctionName) {
  return llvm::StringSwitch<StringRef>(FunctionName)
      .Cases("asctime", "ctime", "is non-reentrant")
      .Cases("fopen", "freopen", "has no exclusive access to file")
      .Cases("rewind", "setbuf", "has no error detection")
      .Default("is obsolescent");
}

} // namespace

void ObsolescentFunctionsCheck::registerMatchers(MatchFinder *Finder) {
  auto FunctionNamesMatcher = hasAnyName(
      "::rewind", "::setbuf", "::gets", "::asctime", "::ctime", "::fopen",
      "::freopen", "::bsearch", "::fprintf", "::fscanf", "::fwprintf",
      "::fwscanf", "::getenv", "::gmtime", "::localtime", "::mbsrtowcs",
      "::mbstowcs", "::memcpy", "::memmove", "::printf", "::qsort", "::setbuf",
      "::snprintf", "::sprintf", "::sscanf", "::strcat", "::strcpy",
      "::strerror", "::strncat", "::strncpy", "::strtok", "::swprintf",
      "::swscanf", "::vfprintf", "::vfscanf", "::vfwprintf", "::vfwscanf",
      "::vprintf", "::vscanf", "::vsnprintf", "::vsprintf", "::vsscanf",
      "::vswprintf", "::vswscanf", "::vwprintf", "::vwscanf", "::wcrtomb",
      "::wcscat", "::wcscpy", "::wcsncat", "::wcsncpy", "::wcsrtombs",
      "::wcstok", "::wcstombs", "::wctomb", "::wmemcpy", "::wmemmove",
      "::wprintf", "::wscanf");

  Finder->addMatcher(callExpr(callee(functionDecl(FunctionNamesMatcher)
                                         .bind(FunctionNamesExprName)))
                         .bind(FunctionExprName),
                     this);
  Finder->addMatcher(varDecl(hasInitializer(ignoringImpCasts(declRefExpr(
                                 to(functionDecl(FunctionNamesMatcher)
                                        .bind(FunctionNamesExprName))))))
                         .bind(FunctionPtrDeclName),
                     this);
}

void ObsolescentFunctionsCheck::check(const MatchFinder::MatchResult &Result) {
  const FunctionDecl *FuncDecl =
      Result.Nodes.getNodeAs<FunctionDecl>(FunctionNamesExprName);
  if (FuncDecl == nullptr)
    return;

  StringRef FunctionName = FuncDecl->getName();
  if (FunctionName == "gets") {
    diag(getSourceLocation(Result),
         "function 'gets' is deprecated as of C99, removed from C11.");
    return;
  }

  std::string ReplacementFunctionName =
      getReplacementFunctionName(FunctionName);
  if (ReplacementFunctionName.empty())
    return;

  diag(getSourceLocation(Result),
       "function '%0' %1, '%2' should be used instead.")
      << FunctionName << getRationale(FunctionName) << ReplacementFunctionName;
}

void ObsolescentFunctionsCheck::registerPPCallbacks(
    const SourceManager &SM, Preprocessor *Pp, Preprocessor *ModuleExpanderPP) {
  PP = Pp;
}

SourceLocation ObsolescentFunctionsCheck::getSourceLocation(
    const ast_matchers::MatchFinder::MatchResult &Result) {
  SourceLocation Loc;
  if (const auto *Call = Result.Nodes.getNodeAs<CallExpr>(FunctionExprName))
    Loc = Call->getExprLoc();
  else if (const auto *Var =
               Result.Nodes.getNodeAs<VarDecl>(FunctionPtrDeclName))
    Loc = Var->getEndLoc();

  return Loc;
}

bool ObsolescentFunctionsCheck::useSafeFunctionsFromAnnexK() {
  assert(PP && "No Preprocessor registered.");

  if (!PP->isMacroDefined("__STDC_LIB_EXT1__"))
    return false;

  const auto *MI =
      PP->getMacroInfo(PP->getIdentifierInfo("__STDC_WANT_LIB_EXT1__"));
  if (MI == nullptr) {
    return false;
  }

  if (MI->tokens().empty())
    return false;

  const auto &T = MI->tokens().back();
  if (!T.isLiteral() || !T.getLiteralData())
    return false;

  StringRef ValueStr = StringRef(T.getLiteralData(), T.getLength());
  llvm::APInt IntValue;
  if (ValueStr.getAsInteger(10, IntValue))
    return false;

  return IntValue == 1;
}

std::string
ObsolescentFunctionsCheck::getReplacementFunctionName(StringRef FunctionName) {
  if (useSafeFunctionsFromAnnexK() &&
      doesReplacementFromAnnexKExist(FunctionName))
    return getAnnexKReplacementFunctionName(FunctionName);

  return llvm::StringSwitch<std::string>(FunctionName)
      .Case("rewind", "fseek")
      .Case("setbuf", "setvbuf")
      .Default(std::string());
}

} // namespace cert
} // namespace tidy
} // namespace clang
