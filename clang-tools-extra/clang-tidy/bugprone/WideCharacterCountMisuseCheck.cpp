//===--- WideCharacterCountMisuseCheck.cpp - clang-tidy -------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "WideCharacterCountMisuseCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/EvaluatedExprVisitor.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"

using namespace clang;
using namespace clang::ast_matchers;

namespace {

class SizeofInferrer : public ConstEvaluatedExprVisitor<SizeofInferrer> {
  using Base = ConstEvaluatedExprVisitor<SizeofInferrer>;

  bool RepresentsBytes = false;
  std::vector<std::pair<std::string, SourceRange>> PropagationSteps;

public:
  void VisitCallExpr(const CallExpr *E) {
    const FunctionDecl *FD = E->getCalleeDecl()->getAsFunction();
    if (!FD)
      return;

    const auto *II = FD->getDeclName().getAsIdentifierInfo();
    if (!II)
      return;

    RepresentsBytes = llvm::StringSwitch<bool>(II->getName())
                          .Case("wcslen", false)
                          .Default(true);
    PropagationSteps.push_back(std::make_pair(
        (Twine{"Return value of `"} + II->getName() + "' represents " +
         (RepresentsBytes ? "bytes" : "character") + " count")
            .str(),
        E->getSourceRange()));
  }

  void VisitBinaryOperator(const BinaryOperator *E) {
    // For now, only these are handled.
    if (!E->isAdditiveOp() && !E->isMultiplicativeOp())
      return;

    SizeofInferrer LhsVisitor{Context};
    SizeofInferrer RhsVisitor{Context};
    LhsVisitor.Visit(E->getLHS());
    RhsVisitor.Visit(E->getRHS());
    bool IsLhsBytes = LhsVisitor.RepresentsBytes;
    bool IsRhsBytes = RhsVisitor.RepresentsBytes;
    RepresentsBytes = IsLhsBytes != IsRhsBytes;

    llvm::append_range(PropagationSteps,
                       std::move(RhsVisitor.PropagationSteps));
    llvm::append_range(PropagationSteps,
                       std::move(LhsVisitor.PropagationSteps));
    PropagationSteps.push_back(std::make_pair(
        (Twine{"Operator "} + E->getOpcodeStr() + " represents " +
         (RepresentsBytes ? "bytes" : "character") + " count")
            .str(),
        E->getSourceRange()));
  }
  void VisitDeclRefExpr(const DeclRefExpr *E) {
    if (const auto *Var = dyn_cast_or_null<VarDecl>(E->getDecl())) {
      SizeofInferrer Visitor{Context};
      Visitor.Visit(Var->getInit());
      RepresentsBytes = Visitor.RepresentsBytes;
      llvm::append_range(PropagationSteps, std::move(Visitor.PropagationSteps));
      PropagationSteps.push_back(
          std::make_pair((Twine{"Variable represents "} +
                          (RepresentsBytes ? "bytes" : "characters") + " count")
                             .str(),
                         E->getSourceRange()));
    }
  }

  void VisitUnaryExprOrTypeTraitExpr(const UnaryExprOrTypeTraitExpr *E) {
    RepresentsBytes = E->getKind() == clang::UETT_SizeOf;
    if (RepresentsBytes)
      PropagationSteps.push_back(std::make_pair(
          "Sizeof expressions represent bytes count", E->getSourceRange()));
    else
      PropagationSteps.push_back(std::make_pair(
          "Expression represents character count", E->getSourceRange()));
  }

  explicit SizeofInferrer(const ASTContext &Context) : Base(Context) {}
  static bool representsBytes(const Expr *E, const ASTContext &Context) {
    SizeofInferrer Visitor{Context};
    Visitor.Visit(E);
    return Visitor.RepresentsBytes;
  }
  bool representsBytes() const { return RepresentsBytes; }
  const auto &getPropogationSteps() const & { return PropagationSteps; }
};
} // namespace

namespace clang {
namespace tidy {
namespace bugprone {

void WideCharacterCountMisuseCheck::registerMatchers(MatchFinder *Finder) {
  auto ThirdArgWLen = callExpr().bind("third-arg");
  auto SecondArgWLen =
      callExpr(callee(functionDecl(hasAnyName("fgetws", "wcsftime"))))
          .bind("second-arg");
  Finder->addMatcher(ThirdArgWLen, this);
  Finder->addMatcher(SecondArgWLen, this);
}

void WideCharacterCountMisuseCheck::check(const MatchFinder::MatchResult &Result) {
  const ASTContext &Ctx = *Result.Context;
  const Expr *Arg;
  const CallExpr *CE;

  if (const auto *Call = Result.Nodes.getNodeAs<CallExpr>("third-arg")) {
    if (Call->getNumArgs() < 3)
      return;
    CE = Call;
    Arg = Call->getArg(2);
  }
  if (const auto *Call = Result.Nodes.getNodeAs<CallExpr>("second-arg")) {
    if (Call->getNumArgs() < 2)
      return;
    CE = Call;
    Arg = Call->getArg(1);
  }
  assert(CE);
  assert(Arg);

  SizeofInferrer Visitor{Ctx};
  Visitor.Visit(Arg);

  if (!Visitor.representsBytes())
    return;

  diag(CE->getExprLoc(),
       "function `%0' expects wide-character count instead of bytes count")
      << CE->getCalleeDecl()->getAsFunction()->getName()
      << CE->getSourceRange();

  for (const auto &Msg : llvm::reverse(Visitor.getPropogationSteps())) {
    diag(Msg.second.getBegin(), Msg.first, DiagnosticIDs::Note) << Msg.second;
  }
}

// lattice:
// top: helyben hagy
// count, byte
// pseudo-bot-signal: suspicius warn (pl count / top: a skalár helyett nem lehet
// hogy sizeof-ot akartál használni?) bot-ignore: silence

// wchar.h

// wide char lengths
// wchar_t *wcsncpy( wchar_t *dest, const wchar_t *src, std::size_t count );
// wchar_t *wcsncat( wchar_t *dest, const wchar_t *src, std::size_t count );
// std::size_t wcsxfrm( wchar_t* dest, const wchar_t* src, std::size_t count );
// int wcsncmp( const wchar_t* lhs, const wchar_t* rhs, std::size_t count );
// wchar_t* wmemcpy( wchar_t* dest, const wchar_t* src, std::size_t count );
// wchar_t* wmemmove( wchar_t* dest, const wchar_t* src, std::size_t count );
// int wmemcmp( const wchar_t* lhs, const wchar_t* rhs, std::size_t count );
// const wchar_t* wmemchr( const wchar_t* ptr, wchar_t ch, std::size_t count );
// wchar_t* wmemset( wchar_t* dest, wchar_t ch, std::size_t count );
// std::size_t mbsrtowcs( wchar_t* dst, const char** src, std::size_t len,
// std::mbstate_t* ps );
//
// wchar_t* fgetws( wchar_t* str, int count, std::FILE* stream );
// std::size_t wcsftime( wchar_t* str, std::size_t count, const wchar_t* format,
// const std::tm* time );

// byte lengths
// std::size_t mbrlen( const char* s, std::size_t n, std::mbstate_t* ps);
// std::size_t mbrtowc( wchar_t* pwc, const char* s, std::size_t n,
// std::mbstate_t* ps ); std::size_t wcsrtombs( char* dst, const wchar_t** src,
// std::size_t len, std::mbstate_t* ps );

} // namespace bugprone
} // namespace tidy
} // namespace clang
