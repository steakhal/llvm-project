//===------- unittests/Analysis/Scalable/UnsafeBufferUsageTest.cpp --------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "clang/Analysis/Scalable/Analyses/UnsafeBufferUsage/UnsafeBufferUsage.h"
#include "clang/AST/DynamicRecursiveASTVisitor.h"
#include "clang/Analysis/Scalable/ASTEntityMapping.h"
#include "clang/Analysis/Scalable/Analyses/UnsafeBufferUsage/UnsafeBufferUsageBuilder.h"
#include "clang/Analysis/Scalable/Analyses/UnsafeBufferUsage/UnsafeBufferUsageExtractor.h"
#include "clang/Analysis/Scalable/Model/EntityId.h"
#include "clang/Analysis/Scalable/Model/EntityName.h"
#include "clang/Analysis/Scalable/TUSummary/TUSummary.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/ArrayRef.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

using namespace clang;
using namespace ssaf;
using testing::UnorderedElementsAre;

namespace {

template <typename SomeDecl = NamedDecl>
const SomeDecl *findDeclByName(StringRef Name, ASTContext &Ctx) {
  class NamedDeclFinder : public DynamicRecursiveASTVisitor {
  public:
    StringRef SearchingName;
    const NamedDecl *FoundDecl = nullptr;

    NamedDeclFinder(StringRef SearchingName) : SearchingName(SearchingName) {}

    bool VisitDecl(Decl *D) override {
      if (const auto *ND = dyn_cast<NamedDecl>(D)) {
        if (ND->getNameAsString() == SearchingName) {
          FoundDecl = ND;
          return false;
        }
      }
      return true;
    }
  };

  NamedDeclFinder Finder(Name);

  Finder.TraverseDecl(Ctx.getTranslationUnitDecl());
  return dyn_cast_or_null<SomeDecl>(Finder.FoundDecl);
}

const FunctionDecl *findFnByName(StringRef Name, ASTContext &Ctx) {
  return findDeclByName<FunctionDecl>(Name, Ctx);
}

class UnsafeBufferUsageTest : public testing::Test {
protected:
  TUSummary TUSummary;
  UnsafeBufferUsageTUSummaryBuilder Builder;
  UnsafeBufferUsageTUSummaryExtractor Extractor;
  std::unique_ptr<ASTUnit> AST;

  UnsafeBufferUsageTest()
      : TUSummary(
            BuildNamespace(BuildNamespaceKind::CompilationUnit, "Mock.cpp")),
        Builder(TUSummary),
        Extractor(UnsafeBufferUsageTUSummaryExtractor(Builder)) {}

  std::unique_ptr<UnsafeBufferUsageEntitySummary> setUpTest(StringRef Code) {
    AST = tooling::buildASTFromCode(Code);

    const auto *ContributorDefn =
        findDeclByName("test_subject", AST->getASTContext());
    std::optional<EntityName> EN = getEntityName(ContributorDefn);

    if (!ContributorDefn || !EN)
      return nullptr;
    return Extractor.extractEntitySummary(
        Builder.addEntity(*EN), ContributorDefn, AST->getASTContext());
  }

public:
  std::optional<EntityId> getEntityId(StringRef Name) {
    if (const auto *D = findDeclByName(Name, AST->getASTContext()))
      if (auto EntityName = getEntityName(D))
        return Builder.addEntity(*EntityName);
    return std::nullopt;
  }

  std::optional<EntityId> getEntityIdForReturn(StringRef FunName) {
    if (const auto *D = findFnByName(FunName, AST->getASTContext()))
      if (auto EntityName = getEntityNameForReturn(D))
        return Builder.addEntity(*EntityName);
    return std::nullopt;
  }

  auto UnorderedPointerLevelsAre(
      llvm::ArrayRef<std::pair<StringRef, unsigned>> Pairs = {});
  auto UnorderedReturnPointerLevelsAre(
      llvm::ArrayRef<std::pair<StringRef, unsigned>> Pairs = {});
};

constexpr inline auto buildEntityPointerLevel =
    UnsafeBufferUsageTUSummaryBuilder::buildEntityPointerLevel;
constexpr inline auto buildUnsafeBufferUsageEntitySummary =
    UnsafeBufferUsageTUSummaryBuilder::buildUnsafeBufferUsageEntitySummary;

auto UnorderedPointerLevelsAreImpl(
    UnsafeBufferUsageTest *Fixture,
    llvm::ArrayRef<std::pair<StringRef, unsigned>> Pairs) {
  std::vector<EntityPointerLevel> EPLs;
  for (auto [Name, Level] : Pairs) {
    auto Id = Fixture->getEntityId(Name);
    if (!Id)
      ADD_FAILURE() << "Entity not found: " << Name.str();
    else
      EPLs.push_back(buildEntityPointerLevel(*Id, Level));
  }
  return ::testing::UnorderedElementsAreArray(EPLs);
}

auto UnorderedReturnPointerLevelsAreImpl(
    UnsafeBufferUsageTest *Fixture,
    llvm::ArrayRef<std::pair<StringRef, unsigned>> Pairs) {
  std::vector<EntityPointerLevel> EPLs;
  for (auto [Name, Level] : Pairs) {
    auto Id = Fixture->getEntityIdForReturn(Name);
    if (!Id)
      ADD_FAILURE() << "Entity not found: " << Name.str();
    else
      EPLs.push_back(buildEntityPointerLevel(*Id, Level));
  }
  return ::testing::UnorderedElementsAreArray(EPLs);
}

auto UnsafeBufferUsageTest::UnorderedPointerLevelsAre(
    llvm::ArrayRef<std::pair<StringRef, unsigned>> Pairs) {
  return UnorderedPointerLevelsAreImpl(this, Pairs);
}
auto UnsafeBufferUsageTest::UnorderedReturnPointerLevelsAre(
    llvm::ArrayRef<std::pair<StringRef, unsigned>> Pairs) {
  return UnorderedReturnPointerLevelsAreImpl(this, Pairs);
}

//////////////////////////////////////////////////////////////
//                   Data Structure Tests                   //
//////////////////////////////////////////////////////////////

TEST_F(UnsafeBufferUsageTest, EntityPointerLevelComparison) {
  EntityId E1 = Builder.addEntity({"c:@F@foo", "", {}});
  EntityId E2 = Builder.addEntity({"c:@F@bar", "", {}});

  auto P1 = buildEntityPointerLevel(E1, 2);
  auto P2 = buildEntityPointerLevel(E1, 2);
  auto P3 = buildEntityPointerLevel(E1, 1);
  auto P4 = buildEntityPointerLevel(E2, 2);

  EXPECT_EQ(P1, P2);
  EXPECT_NE(P1, P3);
  EXPECT_NE(P1, P4);
  EXPECT_NE(P3, P4);
  EXPECT_TRUE(P3 < P2);
  EXPECT_TRUE(P3 < P4);
  EXPECT_FALSE(P1 < P2);
  EXPECT_FALSE(P2 < P1);
}

static EntityPointerLevelSet
getSubsetOf(const UnsafeBufferUsageEntitySummary &Summary, EntityId Id) {
  auto Subset = Summary.getSubsetOf(Id);
  return {Subset.begin(), Subset.end()};
}

TEST_F(UnsafeBufferUsageTest, UnsafeBufferUsageEntitySummaryTest) {
  EntityId E1 = Builder.addEntity({"c:@F@foo", "", {}});
  EntityId E2 = Builder.addEntity({"c:@F@bar", "", {}});
  EntityId E3 = Builder.addEntity({"c:@F@baz", "", {}});

  auto P1 = buildEntityPointerLevel(E1, 1);
  auto P2 = buildEntityPointerLevel(E1, 2);
  auto P3 = buildEntityPointerLevel(E2, 1);
  auto P4 = buildEntityPointerLevel(E2, 2);
  auto P5 = buildEntityPointerLevel(E3, 1);

  auto ES = buildUnsafeBufferUsageEntitySummary({P1, P2, P3, P4, P5});
  ASSERT_TRUE(ES);
  EXPECT_THAT(*ES, UnorderedElementsAre(P1, P2, P3, P4, P5));
  EXPECT_THAT(getSubsetOf(*ES, E1), UnorderedElementsAre(P1, P2));
  EXPECT_THAT(getSubsetOf(*ES, E2), UnorderedElementsAre(P3, P4));
  EXPECT_THAT(getSubsetOf(*ES, E3), UnorderedElementsAre(P5));
}

//////////////////////////////////////////////////////////////
//                   Extractor Tests                        //
//////////////////////////////////////////////////////////////

TEST_F(UnsafeBufferUsageTest, SimpleFunctionWithUnsafePointer) {
  auto Sum = setUpTest(R"cpp(
    void test_subject(int *p) {
      p[5];
    }
  )cpp");

  ASSERT_NE(Sum, nullptr);
  EXPECT_THAT(*Sum, UnorderedPointerLevelsAre({{"p", 1u}}));
}

TEST_F(UnsafeBufferUsageTest, PointerArithmetic) {
  auto Sum = setUpTest(R"cpp(
    void test_subject(int *p, int *q) {
      *(p + 5);
      *(q - 3);
    }
  )cpp");

  ASSERT_NE(Sum, nullptr);
  EXPECT_THAT(*Sum, UnorderedPointerLevelsAre({{"p", 1u}, {"q", 1u}}));
}

TEST_F(UnsafeBufferUsageTest, PointerIncrementDecrement) {
  auto Sum = setUpTest(R"cpp(
    void test_subject(int *p, int *q, int *r, int *s) {
      (++p)[5];
      (q++)[5];
      (--r)[5];
      (s--)[5];
    }
  )cpp");

  ASSERT_NE(Sum, nullptr);

  EXPECT_THAT(*Sum, UnorderedPointerLevelsAre({
                        {"p", 1u},
                        {"q", 1u},
                        {"r", 1u},
                        {"s", 1u},
                    }));
}

TEST_F(UnsafeBufferUsageTest, PointerAssignment) {
  auto Sum = setUpTest(R"cpp(
    void test_subject(int *p, int *q) {
      (p = q + 5)[5];
    }
  )cpp");

  ASSERT_NE(Sum, nullptr);
  EXPECT_THAT(*Sum, UnorderedPointerLevelsAre({{"p", 1u}, {"q", 1u}}));
}

TEST_F(UnsafeBufferUsageTest, CompoundAssignment) {
  auto Sum = setUpTest(R"cpp(
    void test_subject(int *p, int *q) {
      (p += 5)[5];
      (q -= 3)[5];
    }
  )cpp");

  ASSERT_NE(Sum, nullptr);
  EXPECT_THAT(*Sum, UnorderedPointerLevelsAre({{"p", 1u}, {"q", 1u}}));
}

TEST_F(UnsafeBufferUsageTest, MultiLevelPointer) {
  auto Sum = setUpTest(R"cpp(
    void test_subject(int **p, int **q, int **r) {
      (*p)[5];
      *(*q);
      *(q[5]);
      r[5][5];
    }
  )cpp");

  ASSERT_NE(Sum, nullptr);
  EXPECT_THAT(*Sum, UnorderedPointerLevelsAre({
                        {"p", 2u},
                        {"q", 1u},
                        {"r", 1u},
                        {"r", 2u},
                    }));
}

TEST_F(UnsafeBufferUsageTest, ConditionalOperator) {
  auto Sum = setUpTest(R"cpp(
    void test_subject(int **p, int **q, int cond) {
      (cond ? *p : *q)[5];
      cond ? p[5] : q[5];
    }
  )cpp");

  ASSERT_NE(Sum, nullptr);
  EXPECT_THAT(*Sum, UnorderedPointerLevelsAre({
                        {"p", 1u},
                        {"p", 2u},
                        {"q", 1u},
                        {"q", 2u},
                    }));
}

TEST_F(UnsafeBufferUsageTest, CastExpression) {
  auto Sum = setUpTest(R"cpp(
    void test_subject(void *p, int q) {
      ((int*)p)[5];
      ((int*)q)[5];
    }
  )cpp");

  ASSERT_NE(Sum, nullptr);
  EXPECT_THAT(*Sum, UnorderedPointerLevelsAre({{"p", 1u}}));
}

TEST_F(UnsafeBufferUsageTest, CommaOperator) {
  auto Sum = setUpTest(R"cpp(
    void test_subject(int *p, int x) {
      (x++, p)[5];
    }
  )cpp");

  ASSERT_NE(Sum, nullptr);
  EXPECT_THAT(*Sum, UnorderedPointerLevelsAre({{"p", 1u}}));
}

TEST_F(UnsafeBufferUsageTest, ParenthesizedExpression) {
  auto Sum = setUpTest(R"cpp(
    void test_subject(int *p) {
      (((p)))[5];
    }
  )cpp");

  ASSERT_NE(Sum, nullptr);
  EXPECT_THAT(*Sum, UnorderedPointerLevelsAre({{"p", 1u}}));
}

TEST_F(UnsafeBufferUsageTest, ArrayParameter) {
  auto Sum = setUpTest(R"cpp(
    void test_subject(int arr[], int arr2[][10]) {
      int n = 5;
      arr[100];
      arr2[5][n];
    }
  )cpp");

  ASSERT_NE(Sum, nullptr);
  EXPECT_THAT(*Sum, UnorderedPointerLevelsAre(
                        {{"arr", 1u}, {"arr2", 1u}, {"arr2", 2u}}));
}

TEST_F(UnsafeBufferUsageTest, FunctionCall) {
  auto Sum = setUpTest(R"cpp(
    int ** (*fp)();
    int ** test_subject() {
      fp = &test_subject;
      test_subject()[5];
      (*fp())[5];
    }
  )cpp");

  ASSERT_NE(Sum, nullptr);
  // No (foo, 2) because indirect calls are ignored.
  EXPECT_THAT(*Sum, UnorderedReturnPointerLevelsAre({{"test_subject", 1u}}));
}

TEST_F(UnsafeBufferUsageTest, StructMemberAccess) {
  auto Sum = setUpTest(R"cpp(
    struct S {
      int *ptr;
      int (*ptr_to_arr)[10];
    };
    void test_subject(struct S obj) {
      int n = 5;
      obj.ptr[5];
      (*obj.ptr_to_arr)[n];
    }
  )cpp");

  ASSERT_NE(Sum, nullptr);
  EXPECT_THAT(*Sum,
              UnorderedPointerLevelsAre({{"ptr", 1u}, {"ptr_to_arr", 2u}}));
}

TEST_F(UnsafeBufferUsageTest, StringLiteralSubscript) {
  auto Sum = setUpTest(R"cpp(
    void test_subject() {
      "hello"[5];
    }
  )cpp");

  ASSERT_NE(Sum, nullptr);
  // String literals should not generate pointer kind variables
  EXPECT_THAT(*Sum, UnorderedPointerLevelsAre());
}

TEST_F(UnsafeBufferUsageTest, OpaqueValueExpr) {
  auto Sum = setUpTest(R"cpp(
    void test_subject(int *p, int *q) {
       (p ?: q)[5];
    }
  )cpp");

  ASSERT_NE(Sum, nullptr);
  EXPECT_THAT(*Sum, UnorderedPointerLevelsAre({{"p", 1u}, {"q", 1u}}));
}

TEST_F(UnsafeBufferUsageTest, AddressOfOperator) {
  auto Sum = setUpTest(R"cpp(
    void test_subject(int x) {
      (&x)[5];
    }
  )cpp");

  ASSERT_NE(Sum, nullptr);
  // Address-of should not generate pointer kind variables for 'x'
  EXPECT_THAT(*Sum, UnorderedPointerLevelsAre());
}

TEST_F(UnsafeBufferUsageTest, AddressOfThenDereference) {
  auto Sum = setUpTest(R"cpp(
    void test_subject(int *p, int *q) {
      (*(&p))[5];
      (&(*q))[5];
    }
  )cpp");

  ASSERT_NE(Sum, nullptr);
  EXPECT_THAT(*Sum, UnorderedPointerLevelsAre({{"p", 1u}, {"q", 1u}}));
}
} // namespace
