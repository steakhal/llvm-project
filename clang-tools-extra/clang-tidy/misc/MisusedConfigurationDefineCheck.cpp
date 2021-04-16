//===--- MisusedConfigurationDefineCheck.cpp - clang-tidy -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MisusedConfigurationDefineCheck.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Lex/Lexer.h"
#include "clang/Lex/PPCallbacks.h"
#include "clang/Lex/Preprocessor.h"

namespace clang {
namespace tidy {
namespace misc {

class MisusedConfigurationDefineCallbacks : public PPCallbacks {
  ClangTidyCheck &Check;
  Preprocessor &PP;
  SourceManager &SM;
  llvm::StringMap<SourceLocation> FirstAccessPlace;

public:
  explicit MisusedConfigurationDefineCallbacks(ClangTidyCheck &Check,
                                               Preprocessor &PP)
      : Check(Check), PP(PP), SM(PP.getSourceManager()) {}

  void Ifdef(SourceLocation Loc, const Token &MacroNameTok,
             const MacroDefinition &MacroDefinition) override {
    handleAccess(Loc, MacroNameTok);
  }

  void Ifndef(SourceLocation Loc, const Token &MacroNameTok,
              const MacroDefinition &MacroDefinition) override {
    handleAccess(Loc, MacroNameTok);
  }

  void MacroUndefined(const Token &MacroNameTok, const MacroDefinition &MD,
                      const MacroDirective *Undef) override {
    handleAccess(MacroNameTok.getLocation(), MacroNameTok);
  }

  void MacroDefined(const Token &MacroNameTok,
                    const MacroDirective *MD) override {
    SourceLocation Loc = MacroNameTok.getLocation();
    if (SM.isInSystemHeader(Loc))
      return;

    std::string MacroName = PP.getSpelling(MacroNameTok);
    SourceLocation FirstReadLoc = FirstAccessPlace.lookup(MacroName);
    if (FirstReadLoc.isInvalid())
      return;
    PresumedLoc PFirstReadLoc = SM.getPresumedLoc(FirstReadLoc);
    PresumedLoc PLoc = SM.getPresumedLoc(Loc);
    assert(PFirstReadLoc.isValid());
    assert(PLoc.isValid());

    if (PFirstReadLoc.getFileID() != PLoc.getFileID()) {
      Check.diag(Loc, "the macro being defined here was preveusly used in "
                      "macro directive which might be unexpected");
      Check.diag(FirstReadLoc, "first accessed here", DiagnosticIDs::Note);
    }
  }

private:
  void handleAccess(SourceLocation Loc, const Token &MacroNameTok) {
    if (SM.isInSystemHeader(Loc))
      return;

    std::string MacroName = PP.getSpelling(MacroNameTok);
    FirstAccessPlace.try_emplace(std::move(MacroName), Loc);
  }
};

void MisusedConfigurationDefineCheck::registerPPCallbacks(
    const SourceManager &SM, Preprocessor *PP, Preprocessor *ModuleExpanderPP) {
  PP->addPPCallbacks(
      ::std::make_unique<MisusedConfigurationDefineCallbacks>(*this, *PP));
}

} // namespace misc
} // namespace tidy
} // namespace clang
