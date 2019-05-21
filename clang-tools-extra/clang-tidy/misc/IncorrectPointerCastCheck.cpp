//===--- IncorrectPointerCastCheck.cpp - clang-tidy------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "IncorrectPointerCastCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecordLayout.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace misc {

void IncorrectPointerCastCheck::storeOptions(
    ClangTidyOptions::OptionMap &Opts) {
  Options.store(Opts, "WarnForDifferentSignedness", WarnForDifferentSignedness);
  Options.store(Opts, "IgnoreReinterpretCast", IgnoreReinterpretCast);
}

void IncorrectPointerCastCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(cStyleCastExpr(hasCastKind(CK_BitCast),
                                    unless(isInTemplateInstantiation()))
                         .bind("cast"),
                     this);
  if (!IgnoreReinterpretCast) {
    Finder->addMatcher(
        cxxReinterpretCastExpr(hasCastKind(CK_BitCast),
                               unless(isInTemplateInstantiation()))
            .bind("cast"),
        this);
  }
}

void IncorrectPointerCastCheck::check(const MatchFinder::MatchResult &Result) {
  const ASTContext &Context = *Result.Context;
  const auto *castExpr = Result.Nodes.getNodeAs<CastExpr>("cast");

  const QualType SrcType = castExpr->getSubExpr()->getType();
  const QualType DestType = castExpr->getType();

  if (!SrcType->isPointerType() || !DestType->isPointerType())
    return;

  if (SrcType->isDependentType() || DestType->isDependentType())
    return;

  const QualType SrcPointedType = SrcType->getPointeeType();
  const QualType DestPointedType = DestType->getPointeeType();

  if (SrcPointedType->isIncompleteType() || DestPointedType->isIncompleteType())
    return;

  if (Context.getIntWidth(SrcPointedType) <
      Context.getIntWidth(DestPointedType)) {
    diag(castExpr->getBeginLoc(),
         "cast from %0 to %1 may lead to access memory based on invalid memory "
         "layout; pointed to type is wider than the allocated type")
        << SrcPointedType << DestPointedType;
  } else if (Result.Context->getTypeAlign(SrcPointedType) <
             Result.Context->getTypeAlign(DestPointedType)) {
    diag(castExpr->getBeginLoc(),
         "cast from %0 to %1 may lead to access memory based on invalid "
         "memory layout; pointed to type is strictly aligned than the "
         "allocated type")
        << SrcPointedType << DestPointedType;
  } else if (SrcPointedType->isStructureType() &&
             DestPointedType->isStructureType()) {
    const auto *SrcTypeCXXRecordDecl = SrcPointedType->getAsCXXRecordDecl();
    const auto *DestTypeCXXRecordDecl = DestPointedType->getAsCXXRecordDecl();
    bool FieldsAreSame = true;

    for (RecordDecl::field_iterator
             SrcIterator = SrcTypeCXXRecordDecl->field_begin(),
             SrcEnd = SrcTypeCXXRecordDecl->field_end(),
             DestIterator = DestTypeCXXRecordDecl->field_begin(),
             DestEnd = DestTypeCXXRecordDecl->field_end();
         DestIterator != DestEnd && FieldsAreSame;
         ++SrcIterator, ++DestIterator) {
      const FieldDecl &SrcField = **SrcIterator;
      const FieldDecl &DestField = **DestIterator;
      if (SrcField.getType() != DestField.getType() || SrcIterator == SrcEnd) {
        FieldsAreSame = false;
      }
    }

    if (!FieldsAreSame) {
      diag(castExpr->getBeginLoc(),
           "cast from %0 to %1 may lead to access memory based on invalid "
           "memory layout; struct members are incompatible")
          << SrcPointedType << DestPointedType;
    }
  } else if (WarnForDifferentSignedness &&
             ((SrcPointedType->isSignedIntegerType() &&
               DestPointedType->isUnsignedIntegerType()) ||
              (SrcPointedType->isUnsignedIntegerType() &&
               DestPointedType->isSignedIntegerType()))) {
    diag(castExpr->getBeginLoc(),
         "cast from %0 to %1 may lead to access memory based on invalid "
         "memory layout; different signedness types")
        << SrcPointedType << DestPointedType;
  }
}

} // namespace misc
} // namespace tidy
} // namespace clang
