//===--- USRMapper.cpp ----------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "clang/Analysis/Analyses/USRMapper.h"
#include "clang/AST/DynamicRecursiveASTVisitor.h"
#include "clang/Index/USRGeneration.h"
#include "llvm/ADT/StringExtras.h"

using namespace clang;
using llvm::DenseMap;

namespace {
class Mapper final : public ConstDynamicRecursiveASTVisitor {
public:
  Mapper(DenseMap<const Decl *, USRString> &DeclToUSR,
         DenseMap<USRString, const Decl *> &USRToDecl)
      : DeclToUSR(DeclToUSR), USRToDecl(USRToDecl) {
    ShouldVisitTemplateInstantiations = true;
    ShouldWalkTypesOfTypeLocs = false;
    ShouldVisitImplicitCode = true;
    ShouldVisitLambdaBody = true;
  }

  bool VisitDecl(const Decl *D) override {
    assert(D);

    D = D->getCanonicalDecl();
    auto [Place, Inserted] = DeclToUSR.try_emplace(D);

    if (Inserted) {
      Sratchspace.clear();
      if (!index::generateUSRForDecl(D, Sratchspace) && !Sratchspace.empty()) {
        Place->second = llvm::toStringRef(Sratchspace).str();
        [[maybe_unused]] bool InsertedToTheOther =
            USRToDecl.try_emplace(Place->second, D).second;
        assert(InsertedToTheOther && "The two maps should be in sync");
      }
    }
    return true; // Continue
  }

private:
  llvm::SmallVector<char, 100> Sratchspace;
  DenseMap<const Decl *, USRString> &DeclToUSR;
  DenseMap<USRString, const Decl *> &USRToDecl;
};
} // namespace

USRMapper::USRMapper() = default;

void USRMapper::traverseDecl(const Decl *D) {
  Mapper{DeclToUSR, USRToDecl}.TraverseDecl(D);
}

const Decl *USRMapper::getDeclForUSR(llvm::StringRef USR) const {
  auto It = USRToDecl.find_as(USR);
  return It == USRToDecl.end() ? nullptr : It->second;
}
std::optional<USRString> USRMapper::getUSRForDecl(const Decl *D) const {
  assert(D);
  D = D->getCanonicalDecl();
  auto It = DeclToUSR.find_as(D);
  return It == DeclToUSR.end() ? std::optional<USRString>{} : It->second;
}
