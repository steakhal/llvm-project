//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/StaticAnalyzer/Checkers/DynamicType.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "gtest/gtest.h"

#include <iterator>
#include <memory>

using namespace clang;
using namespace ento;

static std::string qualifiedNameOf(const CXXMethodDecl *Method) {
  return Method->getQualifiedNameAsString();
}

static llvm::SmallVector<std::string>
qualifiedNamesOf(llvm::TinyPtrVector<const CXXMethodDecl *> Methods) {
  llvm::SmallVector<std::string> Result;
  Result.reserve(Methods.size());
  llvm::transform(Methods, std::back_inserter(Result), qualifiedNameOf);
  llvm::sort(Result);
  return Result;
}

template <typename... StringLiterals>
static llvm::SmallVector<std::string> expected(const StringLiterals &...Items) {
  llvm::SmallVector<std::string> Result;
  Result.reserve(sizeof...(StringLiterals));
  (static_cast<void>(Result.emplace_back(Items)), ...);
  llvm::sort(Result);
  return Result;
}

class DynamicTypeAnalysisTest : public testing::Test {
  std::unique_ptr<ASTUnit> ASTUnitP;
  std::unique_ptr<DynamicTypeAnalysis> Analysis;

public:
  const CXXMethodDecl *method(StringRef Name) const {
    assert(ASTUnitP);

    using namespace ast_matchers;
    auto Matches = ast_matchers::match(cxxMethodDecl(hasName(Name)).bind("fn"),
                                       ASTUnitP->getASTContext());
    if (Matches.size() == 1)
      return Matches[0].getNodeAs<CXXMethodDecl>("fn");
    return nullptr;
  }

  llvm::SmallVector<std::string> getOverridersOf(StringRef Name) {
    return qualifiedNamesOf(getOverriders(*Analysis, method(Name)));
  }

  testing::AssertionResult buildAST(StringRef Code) {
    ASTUnitP = tooling::buildASTFromCode(Code);
    if (!ASTUnitP)
      return testing::AssertionFailure() << "AST construction failed";

    ASTContext &Context = ASTUnitP->getASTContext();
    if (Context.getDiagnostics().hasErrorOccurred())
      return testing::AssertionFailure() << "Compilation error";

    Analysis = createDynamicTypeAnalysis(Context);
    if (!Analysis)
      return testing::AssertionFailure() << "Constructing analysis failed";
    return testing::AssertionSuccess();
  }
};

TEST_F(DynamicTypeAnalysisTest, NonPolymorphic) {
  ASSERT_TRUE(buildAST(R"cpp(
struct NonPolymorphic {
  void m();
};)cpp"));
  EXPECT_EQ(getOverridersOf("NonPolymorphic::m"), expected());
}

TEST_F(DynamicTypeAnalysisTest, TwoPolymorphicRoots) {
  ASSERT_TRUE(buildAST(R"cpp(
struct Root1 {
  virtual void m();
};
struct Root2 {
  virtual void m();
};)cpp"));
  EXPECT_EQ(getOverridersOf("Root1::m"), expected("Root1::m"));
  EXPECT_EQ(getOverridersOf("Root2::m"), expected("Root2::m"));
}

TEST_F(DynamicTypeAnalysisTest,
       TwoPolymorphicRootsWithSharedNonPolymorphicBase) {
  ASSERT_TRUE(buildAST(R"cpp(
struct Base { void non_virtual() {} };
struct Root1 : Base {
  virtual void m();
};
struct Root2 : Base {
  virtual void m();
};)cpp"));
  EXPECT_EQ(getOverridersOf("Base::non_virtual"), expected());
  EXPECT_EQ(getOverridersOf("Root1::m"), expected("Root1::m"));
  EXPECT_EQ(getOverridersOf("Root2::m"), expected("Root2::m"));
}

TEST_F(DynamicTypeAnalysisTest, TwoPolymorphicRootsWithSharedPolymorphicBase) {
  ASSERT_TRUE(buildAST(R"cpp(
struct Base { virtual ~Base() = default; };
struct Root1 : Base {
  ~Root1() override;
  virtual void m();
};
struct Root2 : Base {
  virtual void m();
};)cpp"));
  EXPECT_EQ(getOverridersOf("Base::~Base"),
            expected("Base::~Base", "Root1::~Root1", "Root2::~Root2"));
  EXPECT_EQ(getOverridersOf("Root1::m"), expected("Root1::m"));
  EXPECT_EQ(getOverridersOf("Root2::m"), expected("Root2::m"));
}

TEST_F(DynamicTypeAnalysisTest,
       SomePolymorphicRootsWithSharedRelevantPolymorphicBase) {
  ASSERT_TRUE(buildAST(R"cpp(
struct PolyBase {
  ~PolyBase() = default;
  virtual void m() {};
};
struct Root1 : PolyBase {
  void m() override;
};
struct Root2 : PolyBase {
  void m() override;
};
struct Root3 {
  virtual ~Root3();
};)cpp"));
  EXPECT_EQ(getOverridersOf("PolyBase::m"),
            expected("PolyBase::m", "Root1::m", "Root2::m"));
  EXPECT_EQ(getOverridersOf("Root1::m"), expected("Root1::m"));
  EXPECT_EQ(getOverridersOf("Root2::m"), expected("Root2::m"));
  EXPECT_EQ(getOverridersOf("Root3::~Root3"), expected("Root3::~Root3"));
}
