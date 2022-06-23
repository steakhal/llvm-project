//===--- DontModifyStdNamespaceCheck.cpp - clang-tidy----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "DontModifyStdNamespaceCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

using namespace clang;
using namespace clang::ast_matchers;

static bool refersToStdType(QualType Ty) {
  if (const auto *TagTy = Ty->getAs<TagType>())
    if (const Decl *D = TagTy->getDecl())
      if (D->isInStdNamespace())
        return true;
  return false;
}

static bool isStdOrIntegralType(const TemplateArgument &Arg) {
  if (Arg.getKind() != TemplateArgument::Type)
    return false;

  QualType Ty = Arg.getAsType();
  while (Ty->isPointerType() || Ty->isReferenceType())
    Ty = Ty->getPointeeType();

  return Ty->isBuiltinType() || refersToStdType(Ty);
}

static bool refersToOnlyStdOrBuiltinTypesInPack(const TemplateArgument &Pack) {
  if (Pack.getKind() != TemplateArgument::Pack)
    return false;

  for (const TemplateArgument &Arg : Pack.getPackAsArray())
    if (!isStdOrIntegralType(Arg))
      return false;
  return true;
}

static bool refersToOnlyStdOrBuiltinTypes(const TemplateArgument &Arg) {
  return isStdOrIntegralType(Arg) || refersToOnlyStdOrBuiltinTypesInPack(Arg);
}

// FIXME: It should also look fox POSIX types.
static bool refersToOnlyStdOrBuiltinTypes(const TemplateArgumentList &ArgList) {
  for (unsigned I = 0; I < ArgList.size(); ++I)
    if (!refersToOnlyStdOrBuiltinTypes(ArgList.get(I)))
      return false;
  return true;
}

namespace clang {
namespace tidy {
namespace cert {
void DontModifyStdNamespaceCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(decl().bind("decl"), this);
}
} // namespace cert
} // namespace tidy
} // namespace clang

static bool isExpandedInSystemHeader(SourceLocation Loc,
                                     const SourceManager &SM) {
  SourceLocation ExpLoc = SM.getExpansionLoc(Loc);
  if (ExpLoc.isInvalid() || SM.isInSystemHeader(ExpLoc))
    return true;
  return false;
}

void clang::tidy::cert::DontModifyStdNamespaceCheck::check(
    const MatchFinder::MatchResult &Result) {
  const auto &SM = *Result.SourceManager;
  const auto *D = Result.Nodes.getNodeAs<Decl>("decl");

  // By default, use the lexical context where the Decl is declared.
  const NamespaceDecl *NS =
      dyn_cast_or_null<NamespaceDecl>(D->getLexicalDeclContext());

  bool IsTrickyMethodDecl = false; // FIXME: Rename this to something better.
  // Method decls might not be wrapped by a NamespaceDecl when specialize.
  // Use the namespace of the class in such cases.
  if (NS == nullptr) {
    if (const auto *MD = dyn_cast<CXXMethodDecl>(D)) {
      if (const CXXRecordDecl *RD = MD->getParent()) {
        NS = dyn_cast_or_null<NamespaceDecl>(RD->getLexicalDeclContext());
        IsTrickyMethodDecl = true;
      }
    }
  }

  // Early return if we could not recover the namespace or if that is not a
  // top-level namespace.
  if (!NS || !NS->getParent()->isTranslationUnit())
    return;

  // We also ignore if the declaration is in system header.
  if (isExpandedInSystemHeader(D->getLocation(), SM))
    return;

  // Only look for "std" or "posix" namespaces.
  if (NS->getName() != "std" && NS->getName() != "posix")
    return;

  // If it's a class specialization and it refers to at least one
  // 'program-defined' type, ignore it.
  if (const auto *Class = dyn_cast<ClassTemplateSpecializationDecl>(D))
    if (!refersToOnlyStdOrBuiltinTypes(Class->getTemplateArgs()))
      return;

  // Template functions might also be specialized, so check their types
  // similarly.
  if (const auto *FD = dyn_cast<FunctionDecl>(D))
    if (FD->getTemplateSpecializationKind() == TSK_ExplicitSpecialization)
      if (!refersToOnlyStdOrBuiltinTypes(
              *FD->getTemplateSpecializationInfo()->TemplateArguments))
        return;

  // Ignore non-template method decls, so we don't report multiple warnings into
  // a single class.
  // FIXME: This is too aggressive, we miss a couple cases due to this, check
  // the FIXMEs in the tests.
  if (const auto *MD = dyn_cast<CXXMethodDecl>(D))
    if (IsTrickyMethodDecl && !MD->isFunctionTemplateSpecialization())
      return;

  diag(D->getLocation(),
       "modification of %0 namespace can result in undefined behavior")
      << NS;

  // Only report the 'opening' of the namespace if we have one.
  // E.g. CXXMethodDecls might not be wrapped by a NamespaceDecl, in which case
  // 'NS' will refer to the namespace in which the class is declared.
  if (!isExpandedInSystemHeader(NS->getLocation(), SM))
    diag(NS->getLocation(), "%0 namespace opened here", DiagnosticIDs::Note)
        << NS;
}
