//===- unittests/AST/SemanticallyOrSyntacticallyEquivalentASTNodes.cpp ----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Unit tests for checking the `isSemanticallyEquivalentTo()` and
// `isSyntacticallyEquivalentTo()` functions.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/Decl.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Mangle.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Basic/LLVM.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Testing/Support/Annotations.h"
#include "gtest/gtest.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;

template <typename NodeT, unsigned Nth> static const NodeT *
selectNth(StringRef BoundTo, const SmallVectorImpl<BoundNodes> &Results) {
  unsigned Counter = 0;
  for (const BoundNodes &N : Results)
    if (const NodeT *Node = N.getNodeAs<NodeT>(BoundTo))
      if (++Counter == Nth)
        return Node;
  return nullptr;
}

#if 0
TEST(EquivalentASTNodes, SimpleCases) {
  StringRef Code = R"(
    extern int z;
    int force_declref1 = z; // uses the first decl (1st)
    int z; // redeclaration
    int force_declref2 = z; // uses redecl! (2nd)

    int x, y;
    int force_declref3 = x; // 3rd
    int force_declref4 = y; // 4th

    namespace foo {
      int w;
      int force_declref5 = w;        // 5th
      int force_declref6 = ::foo::w; // 6th
    }
  )";
  auto AST =
      tooling::buildASTFromCodeWithArgs(Code, {"-target", "x86_64-unknown-unknown"});
  ASTContext &Ctx = AST->getASTContext();

  const auto *FirstZ =
      selectNth<DeclRefExpr, 1>("z", match(declRefExpr(to(varDecl(hasName("z")))).bind("z"), Ctx));
    const auto *LastZ =
      selectNth<DeclRefExpr, 2>("z", match(declRefExpr(to(varDecl(hasName("z")))).bind("z"), Ctx));

  ASSERT_NE(FirstZ, LastZ);
  ASSERT_NE(FirstZ->getDecl(), LastZ->getDecl());
  ASSERT_EQ(FirstZ->getDecl(), LastZ->getDecl()->getCanonicalDecl());
  ASSERT_EQ(FirstZ->getDecl()->getCanonicalDecl(), LastZ->getDecl()->getCanonicalDecl());
  // Sees through redecls.
  EXPECT_TRUE(FirstZ->isSemanticallyEquivalentTo(LastZ));
  EXPECT_TRUE(LastZ->isSemanticallyEquivalentTo(FirstZ));
  EXPECT_TRUE(FirstZ->isSyntacticallyEquivalentTo(LastZ));
  EXPECT_TRUE(LastZ->isSyntacticallyEquivalentTo(FirstZ));

  const auto *X =
      selectFirst<DeclRefExpr>("x", match(declRefExpr(to(varDecl(hasName("x")))).bind("x"), Ctx));
  const auto *Y =
      selectFirst<DeclRefExpr>("y", match(declRefExpr(to(varDecl(hasName("y")))).bind("y"), Ctx));
  EXPECT_TRUE(X->isSemanticallyEquivalentTo(X));  // Reflexive.
  EXPECT_TRUE(X->isSyntacticallyEquivalentTo(X)); // Reflexive.

  EXPECT_FALSE(X->isSemanticallyEquivalentTo(Y));
  EXPECT_FALSE(Y->isSemanticallyEquivalentTo(X));
  EXPECT_FALSE(X->isSyntacticallyEquivalentTo(Y));
  EXPECT_FALSE(Y->isSyntacticallyEquivalentTo(X));

  const auto *W =
      selectNth<DeclRefExpr, 1>("w", match(declRefExpr(to(varDecl(hasName("w")))).bind("w"), Ctx));
  const auto *QualifiedW =
      selectNth<DeclRefExpr, 2>("w", match(declRefExpr(to(varDecl(hasName("w")))).bind("w"), Ctx));
  ASSERT_NE(W, QualifiedW);
  ASSERT_EQ(W->getDecl(), QualifiedW->getDecl());
  EXPECT_TRUE(W->isSemanticallyEquivalentTo(QualifiedW));
  EXPECT_TRUE(QualifiedW->isSemanticallyEquivalentTo(W));
  EXPECT_TRUE(W->isSyntacticallyEquivalentTo(QualifiedW));
  EXPECT_TRUE(QualifiedW->isSyntacticallyEquivalentTo(W));
}

TEST(EquivalentASTNodes, FullyQualifiednames) {
  StringRef Code = R"(
    int w; // global
    namespace foo {
      int w;
      int force_declref1 = ::w;      // 1st
      int force_declref2 = ::foo::w; // 2nd
    }
    namespace bar = foo;
    int force_declref3 = ::bar::w; // 3rd
    int force_declref4 = ::foo::w; // 4th
  )";
  auto AST =
      tooling::buildASTFromCodeWithArgs(Code, {"-target", "x86_64-unknown-unknown"});
  ASTContext &Ctx = AST->getASTContext();

  const auto *GloablW =
      selectFirst<DeclRefExpr>("w", match(declRefExpr(to(varDecl(hasName("w")))).bind("w"), Ctx));
  const auto *OtherW =
      selectNth<DeclRefExpr, 2>("w", match(declRefExpr(to(varDecl(hasName("w")))).bind("w"), Ctx));
  ASSERT_NE(GloablW, OtherW);
  ASSERT_NE(GloablW->getDecl(), OtherW->getDecl());
  EXPECT_FALSE(GloablW->isSemanticallyEquivalentTo(OtherW));
  EXPECT_FALSE(OtherW->isSemanticallyEquivalentTo(GloablW));
  EXPECT_FALSE(GloablW->isSyntacticallyEquivalentTo(OtherW));
  EXPECT_FALSE(OtherW->isSyntacticallyEquivalentTo(GloablW));

  const auto *BarW =
      selectNth<DeclRefExpr, 3>("w", match(declRefExpr(to(varDecl(hasName("w")))).bind("w"), Ctx));
  const auto *FooW =
      selectNth<DeclRefExpr, 4>("w", match(declRefExpr(to(varDecl(hasName("w")))).bind("w"), Ctx));  
  ASSERT_NE(BarW, FooW);
  ASSERT_EQ(BarW->getDecl(), FooW->getDecl());
  ASSERT_NE(BarW->getQualifier(), FooW->getQualifier());
  EXPECT_TRUE(BarW->isSemanticallyEquivalentTo(FooW));
  EXPECT_TRUE(FooW->isSemanticallyEquivalentTo(BarW));
  EXPECT_TRUE(BarW->isSyntacticallyEquivalentTo(FooW));
  EXPECT_TRUE(FooW->isSyntacticallyEquivalentTo(BarW));
}

TEST(EquivalentASTNodes, NamespaceAliases) {
  StringRef Code = R"(
    int w; // global
    namespace foo {
      int w;
      int force_declref1 = ::w;      // 1st
      int force_declref2 = ::foo::w; // 2nd
    }
    namespace bar = foo;
    namespace baz = foo;
    int force_declref3 = ::bar::w; // 3rd
    int force_declref4 = ::baz::w; // 4th
  )";
  auto AST =
      tooling::buildASTFromCodeWithArgs(Code, {"-target", "x86_64-unknown-unknown"});
  ASTContext &Ctx = AST->getASTContext();

  const auto *GlobalW =
      selectNth<DeclRefExpr, 1>("w", match(declRefExpr(to(varDecl(hasName("w")))).bind("w"), Ctx));
  const auto *OtherW =
      selectNth<DeclRefExpr, 2>("w", match(declRefExpr(to(varDecl(hasName("w")))).bind("w"), Ctx));
  ASSERT_NE(GlobalW, OtherW);
  ASSERT_NE(GlobalW->getDecl(), OtherW->getDecl());
  EXPECT_FALSE(GlobalW->isSemanticallyEquivalentTo(OtherW));
  EXPECT_FALSE(OtherW->isSemanticallyEquivalentTo(GlobalW));
  EXPECT_FALSE(GlobalW->isSyntacticallyEquivalentTo(OtherW));
  EXPECT_FALSE(OtherW->isSyntacticallyEquivalentTo(GlobalW));

  const auto *BarW =
      selectNth<DeclRefExpr, 3>("w", match(declRefExpr(to(varDecl(hasName("w")))).bind("w"), Ctx));
  const auto *FooW =
      selectNth<DeclRefExpr, 4>("w", match(declRefExpr(to(varDecl(hasName("w")))).bind("w"), Ctx));  
  ASSERT_NE(BarW, FooW);
  ASSERT_EQ(BarW->getDecl(), FooW->getDecl());
  ASSERT_NE(BarW->getQualifier(), FooW->getQualifier());
  EXPECT_TRUE(BarW->isSemanticallyEquivalentTo(FooW));
  EXPECT_TRUE(FooW->isSemanticallyEquivalentTo(BarW));
  EXPECT_TRUE(BarW->isSyntacticallyEquivalentTo(FooW));
  EXPECT_TRUE(FooW->isSyntacticallyEquivalentTo(BarW));
}


TEST(EquivalentASTNodes, NestedNamespaceAliases) {
  StringRef Code = R"(
    namespace foo {
      struct bar {
        static int w;
      };
      using bar_alias = bar;
    }
    namespace foo_alias = foo;
    using nested_alias = foo::bar;

    int force_declref1 = foo::bar::w;             // 1st
    int force_declref2 = foo_alias::bar_alias::w; // 2nd
    int force_declref3 = nested_alias::w;         // 3rd
  )";
  auto AST =
      tooling::buildASTFromCodeWithArgs(Code, {"-target", "x86_64-unknown-unknown"});
  ASTContext &Ctx = AST->getASTContext();

  const auto *FooBarW =
      selectNth<DeclRefExpr, 1>("w", match(declRefExpr(to(varDecl(hasName("w")))).bind("w"), Ctx));
  const auto *FooAliasBarAliasW =
      selectNth<DeclRefExpr, 2>("w", match(declRefExpr(to(varDecl(hasName("w")))).bind("w"), Ctx));
  ASSERT_NE(FooBarW, FooAliasBarAliasW);
  EXPECT_TRUE(FooBarW->isSemanticallyEquivalentTo(FooAliasBarAliasW));
  EXPECT_TRUE(FooAliasBarAliasW->isSemanticallyEquivalentTo(FooBarW));
  EXPECT_FALSE(FooBarW->isSyntacticallyEquivalentTo(FooAliasBarAliasW));
  EXPECT_FALSE(FooAliasBarAliasW->isSyntacticallyEquivalentTo(FooBarW));

  const auto *NestedAliasW =
      selectNth<DeclRefExpr, 3>("w", match(declRefExpr(to(varDecl(hasName("w")))).bind("w"), Ctx));
  ASSERT_NE(FooBarW, NestedAliasW);
  EXPECT_TRUE(FooBarW->isSemanticallyEquivalentTo(NestedAliasW));
  EXPECT_TRUE(NestedAliasW->isSemanticallyEquivalentTo(FooBarW));
  EXPECT_FALSE(FooBarW->isSyntacticallyEquivalentTo(NestedAliasW));
  EXPECT_FALSE(NestedAliasW->isSyntacticallyEquivalentTo(FooBarW));
}


TEST(EquivalentASTNodes, Templates) {
  StringRef Code = R"(
    template <class T> struct A {
      struct B {
        using type = int;
        static typename A<T>::B::type w;
      };
      using C = B;
    };
    using i32 = int;
    template <class T> int A<T>::B::w;  // definition
    int force_declref1 = A<int>::C::w;  // 1st
    int force_declref2 = A<i32>::B::w;  // 2nd
    int force_declref3 = A<char>::C::w; // 3rd
  )";
  auto AST =
      tooling::buildASTFromCodeWithArgs(Code, {"-target", "x86_64-unknown-unknown"});
  ASTContext &Ctx = AST->getASTContext();

  const auto *AintCW =
      selectNth<DeclRefExpr, 1>("w", match(declRefExpr(to(varDecl(hasName("w")))).bind("w"), Ctx));
  const auto *Ai32BW =
      selectNth<DeclRefExpr, 2>("w", match(declRefExpr(to(varDecl(hasName("w")))).bind("w"), Ctx));
  ASSERT_NE(AintCW, Ai32BW);
  ASSERT_EQ(AintCW->getDecl()->getCanonicalDecl(), Ai32BW->getDecl()->getCanonicalDecl());
  EXPECT_TRUE(AintCW->isSemanticallyEquivalentTo(Ai32BW));
  EXPECT_TRUE(Ai32BW->isSemanticallyEquivalentTo(AintCW));
  EXPECT_FALSE(AintCW->isSyntacticallyEquivalentTo(Ai32BW));
  EXPECT_FALSE(Ai32BW->isSyntacticallyEquivalentTo(AintCW));

  const auto *AcharBW =
      selectNth<DeclRefExpr, 3>("w", match(declRefExpr(to(varDecl(hasName("w")))).bind("w"), Ctx));
  ASSERT_NE(Ai32BW, AcharBW);
  EXPECT_FALSE(Ai32BW->isSemanticallyEquivalentTo(AcharBW));
  EXPECT_FALSE(AcharBW->isSemanticallyEquivalentTo(Ai32BW));
  EXPECT_FALSE(Ai32BW->isSyntacticallyEquivalentTo(AcharBW));
  EXPECT_FALSE(AcharBW->isSyntacticallyEquivalentTo(Ai32BW));
}


namespace {
  const internal::VariadicDynCastAllOfMatcher<Stmt, DependentScopeDeclRefExpr> dependentScopeDeclRefExpr;
} // namespace

TEST(EquivalentASTNodes, DependentScopeDeclRefExprs) {
  StringRef Code = R"(
    template <class T> struct X {
      static const int value = 1;
    };
    template <class T> using Y = X<T>;

    template <class T, class U> void f() {
      X<T>::value; // 1st
      Y<T>::value; // 2nd
      X<U>::value; // 3rd
    }
    template <class A, class B, class D> void g() {
      using C = A;
      A::w;         // 4th
      B::w;         // 5th
      C::w;         // 6th
      D::unrelated; // 7th
    }
  )";
  auto AST =
      tooling::buildASTFromCodeWithArgs(Code, {"-target", "x86_64-unknown-unknown"});
  ASTContext &Ctx = AST->getASTContext();

  const auto *XT =
      selectNth<DependentScopeDeclRefExpr, 1>("value", match(dependentScopeDeclRefExpr().bind("value"), Ctx));
  const auto *YT =
      selectNth<DependentScopeDeclRefExpr, 2>("value", match(dependentScopeDeclRefExpr().bind("value"), Ctx));
  ASSERT_NE(XT, YT);
  EXPECT_TRUE(XT->isSemanticallyEquivalentTo(YT));
  EXPECT_TRUE(YT->isSemanticallyEquivalentTo(XT));
  EXPECT_FALSE(XT->isSyntacticallyEquivalentTo(YT));
  EXPECT_FALSE(YT->isSyntacticallyEquivalentTo(XT));

  const auto *XU =
      selectNth<DependentScopeDeclRefExpr, 2>("value", match(dependentScopeDeclRefExpr().bind("value"), Ctx));
  ASSERT_NE(XT, XU);
  EXPECT_TRUE(XT->isSemanticallyEquivalentTo(XU)); // FIXME: Should be false.
  EXPECT_TRUE(XU->isSemanticallyEquivalentTo(XT)); // FIXME: Should be false. (same as above)
  EXPECT_FALSE(XT->isSyntacticallyEquivalentTo(XU));
  EXPECT_FALSE(XU->isSyntacticallyEquivalentTo(XT));

  const auto *AW =
      selectNth<DependentScopeDeclRefExpr, 1>("w", match(dependentScopeDeclRefExpr().bind("w"), Ctx));
  const auto *BW =
      selectNth<DependentScopeDeclRefExpr, 2>("w", match(dependentScopeDeclRefExpr().bind("w"), Ctx));
  ASSERT_NE(AW, BW);
  EXPECT_TRUE(AW->isSemanticallyEquivalentTo(BW));
  EXPECT_TRUE(BW->isSemanticallyEquivalentTo(AW));
  EXPECT_FALSE(AW->isSyntacticallyEquivalentTo(BW));
  EXPECT_FALSE(BW->isSyntacticallyEquivalentTo(AW));

  const auto *CW =
      selectNth<DependentScopeDeclRefExpr, 3>("w", match(dependentScopeDeclRefExpr().bind("w"), Ctx));
  ASSERT_NE(AW, CW);
  EXPECT_FALSE(AW->isSemanticallyEquivalentTo(CW));
  EXPECT_FALSE(CW->isSemanticallyEquivalentTo(AW));
  EXPECT_FALSE(AW->isSyntacticallyEquivalentTo(CW));
  EXPECT_FALSE(CW->isSyntacticallyEquivalentTo(AW));

  const auto *DUnrelated =
      selectNth<DependentScopeDeclRefExpr, 4>("unrelated", match(dependentScopeDeclRefExpr().bind("unrelated"), Ctx));
  ASSERT_NE(AW, DUnrelated);
  EXPECT_FALSE(AW->isSemanticallyEquivalentTo(DUnrelated));
  EXPECT_FALSE(DUnrelated->isSemanticallyEquivalentTo(AW));
  EXPECT_FALSE(AW->isSyntacticallyEquivalentTo(DUnrelated));
  EXPECT_FALSE(DUnrelated->isSyntacticallyEquivalentTo(AW));
}

TEST(EquivalentASTNodes, IsSameExample) {
  StringRef Code = R"(
    namespace std {
      struct false_type {
        static constexpr bool value = false;
      };
      struct true_type {
        static constexpr bool value = true;
      };
      template <class T, class U> struct is_same : false_type {};
      template <class T> struct is_same<T, T> : true_type {};
    } // namespace std

    using i32 = int;
    int force_declref1 = std::is_same<char, int>::value;   // 1st
    int force_declref2 = std::is_same<char, float>::value; // 2nd
    int force_declref3 = std::is_same<char, i32>::value;   // 3rd
  )";
  auto AST =
      tooling::buildASTFromCodeWithArgs(Code, {"-target", "x86_64-unknown-unknown"});
  ASTContext &Ctx = AST->getASTContext();

  const auto *CharInt =
      selectNth<DeclRefExpr, 1>("value", match(declRefExpr().bind("value"), Ctx));
  const auto *FloatInt =
      selectNth<DeclRefExpr, 2>("value", match(declRefExpr().bind("value"), Ctx));
  ASSERT_NE(CharInt, FloatInt);
  EXPECT_FALSE(CharInt->isSemanticallyEquivalentTo(FloatInt));
  EXPECT_FALSE(FloatInt->isSemanticallyEquivalentTo(CharInt));
  EXPECT_FALSE(CharInt->isSyntacticallyEquivalentTo(FloatInt));
  EXPECT_FALSE(FloatInt->isSyntacticallyEquivalentTo(CharInt));

  const auto *CharI32 =
      selectNth<DeclRefExpr, 3>("value", match(declRefExpr().bind("value"), Ctx));
  ASSERT_NE(CharInt, CharI32);
  EXPECT_TRUE(CharInt->isSemanticallyEquivalentTo(CharI32));
  EXPECT_TRUE(CharI32->isSemanticallyEquivalentTo(CharInt));
  EXPECT_FALSE(CharInt->isSyntacticallyEquivalentTo(CharI32));
  EXPECT_FALSE(CharI32->isSyntacticallyEquivalentTo(CharInt));
}

#endif

TEST(EquivalentASTNodes, CompareQualifiers) {
  StringRef Code = R"(
    struct false_type {
      static constexpr bool value = false;
    };
    namespace A {
      using box = false_type;
    };
    using namespace A;
    int force_declref1 = A::box::value; // 1st
    int force_declref2 =  ::box::value; // 2nd
  )";
  auto AST =
      tooling::buildASTFromCodeWithArgs(Code, {"-target", "x86_64-unknown-unknown"});
  ASTContext &Ctx = AST->getASTContext();

  const auto *CharInt =
      selectNth<DeclRefExpr, 1>("value", match(declRefExpr().bind("value"), Ctx));
  const auto *FloatInt =
      selectNth<DeclRefExpr, 2>("value", match(declRefExpr().bind("value"), Ctx));
  ASSERT_NE(CharInt, FloatInt);
  EXPECT_TRUE(CharInt->isSemanticallyEquivalentTo(FloatInt));
  EXPECT_TRUE(FloatInt->isSemanticallyEquivalentTo(CharInt));
  EXPECT_TRUE(CharInt->isSyntacticallyEquivalentTo(FloatInt));
  EXPECT_TRUE(FloatInt->isSyntacticallyEquivalentTo(CharInt));

  // Checking the qualifier sequence directly, demonstrating the 'RecurseToPrefix' option.
  EXPECT_TRUE(CharInt->getQualifier()->isSemanticallyEquivalentTo(FloatInt->getQualifier(), /*RecurseToPrefix=*/true));
  EXPECT_TRUE(CharInt->getQualifier()->isSemanticallyEquivalentTo(FloatInt->getQualifier(), /*RecurseToPrefix=*/false));

  EXPECT_TRUE(CharInt->getQualifier()->isSyntacticallyEquivalentTo(FloatInt->getQualifier(), /*RecurseToPrefix=*/true));
  EXPECT_TRUE(CharInt->getQualifier()->isSyntacticallyEquivalentTo(FloatInt->getQualifier(), /*RecurseToPrefix=*/false));
}
