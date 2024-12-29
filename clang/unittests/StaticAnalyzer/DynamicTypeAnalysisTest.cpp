//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "Reusables.h"
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

namespace {
struct TestCase {
  std::string OverridersOf;
  std::vector<std::string> ExpectedOverriders;
};
} // namespace

static std::string qualifiedNameOf(const CXXMethodDecl *Method) {
  return Method->getQualifiedNameAsString();
}

static std::vector<std::string>
qualifiedNamesOf(llvm::TinyPtrVector<const CXXMethodDecl *> Methods) {
  std::vector<std::string> Result;
  Result.reserve(Methods.size());
  llvm::transform(Methods, std::back_inserter(Result), qualifiedNameOf);
  llvm::sort(Result);
  return Result;
}

template <typename... StringLiterals>
static std::vector<std::string> expected(const StringLiterals &...Items) {
  std::vector<std::string> Result;
  Result.reserve(sizeof...(StringLiterals));
  (static_cast<void>(Result.emplace_back(Items)), ...);
  llvm::sort(Result);
  return Result;
}

namespace {
class OverridersConsumer final : public ExprEngineConsumer {
public:
  OverridersConsumer(CompilerInstance &C, DynamicTypeAnalysis &DyTyAnalysis,
                     llvm::ArrayRef<TestCase> Expectations)
      : ExprEngineConsumer(C, DyTyAnalysis), Expectations(Expectations) {}

private:
  void HandleTranslationUnit(ASTContext &Ctx) override;

  const CXXMethodDecl *method(StringRef Name) const {
    using namespace ast_matchers;
    auto Matches = ast_matchers::match(cxxMethodDecl(hasName(Name)).bind("fn"),
                                       Eng.getContext());
    if (Matches.size() == 1)
      return Matches[0].getNodeAs<CXXMethodDecl>("fn");
    return nullptr;
  }

  DynamicTypeAnalysis &getAnalysis() const {
    return Eng.getAnalysisManager().getDynamicTypeAnalysis();
  }

  std::vector<std::string> getOverridersOf(StringRef Name) const {
    return qualifiedNamesOf(getOverriders(getAnalysis(), method(Name)));
  }

  llvm::ArrayRef<TestCase> Expectations;
};
} // namespace

void OverridersConsumer::HandleTranslationUnit(ASTContext &) {
  ASSERT_FALSE(Expectations.empty())
      << "There should be at least one test case";

  for (const TestCase &Expectation : Expectations) {
    const CXXMethodDecl *Method = method(Expectation.OverridersOf);
    if (!Method) {
      ADD_FAILURE() << "Method not found: " << Expectation.OverridersOf;
      continue;
    }
    auto Overriders = qualifiedNamesOf(getOverriders(getAnalysis(), Method));
    EXPECT_EQ(Overriders, Expectation.ExpectedOverriders)
        << "Method: " << Expectation.OverridersOf;
  }
}

static bool runTest(StringRef Code, ArrayRef<TestCase> Expectations) {
  using MyConsumerAction =
      GenericTestActionWithArgs<OverridersConsumer, ArrayRef<TestCase>>;
  return tooling::runToolOnCode(
      std::make_unique<MyConsumerAction>(Expectations), Code);
}

TEST(DynamicTypeAnalysisTest, NonPolymorphic) {
  llvm::StringLiteral Code = R"cpp(
struct NonPolymorphic {
  void m();
};)cpp";
  std::vector<TestCase> Expectations = {
      {"NonPolymorphic::m", expected()},
  };
  EXPECT_TRUE(runTest(Code, Expectations));
}

TEST(DynamicTypeAnalysisTest, TwoPolymorphicRoots) {
  llvm::StringLiteral Code = R"cpp(
struct Root1 {
  virtual void m();
};
struct Root2 {
  virtual void m();
};)cpp";
  std::vector<TestCase> Expectations = {
      {"Root1::m", expected("Root1::m")},
      {"Root2::m", expected("Root2::m")},
  };
  EXPECT_TRUE(runTest(Code, Expectations));
}

TEST(DynamicTypeAnalysisTest, TwoPolymorphicRootsWithSharedNonPolymorphicBase) {
  llvm::StringLiteral Code = R"cpp(
struct Base { void non_virtual() {} };
struct Root1 : Base {
  virtual void m();
};
struct Root2 : Base {
  virtual void m();
};)cpp";
  std::vector<TestCase> Expectations = {
      {"Base::non_virtual", expected()},
      {"Root1::m", expected("Root1::m")},
      {"Root2::m", expected("Root2::m")},
  };
  EXPECT_TRUE(runTest(Code, Expectations));
}

TEST(DynamicTypeAnalysisTest, TwoPolymorphicRootsWithSharedPolymorphicBase) {
  llvm::StringLiteral Code = R"cpp(
struct Base { virtual ~Base() = default; };
struct Root1 : Base {
  ~Root1() override;
  virtual void m();
};
struct Root2 : Base {
  virtual void m();
};)cpp";
  std::vector<TestCase> Expectations = {
      {"Base::~Base",
       expected("Base::~Base", "Root1::~Root1", "Root2::~Root2")},
      {"Root1::m", expected("Root1::m")},
      {"Root2::m", expected("Root2::m")},
  };
  EXPECT_TRUE(runTest(Code, Expectations));
}

TEST(DynamicTypeAnalysisTest,
     SomePolymorphicRootsWithSharedRelevantPolymorphicBase) {
  llvm::StringLiteral Code = R"cpp(
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
};)cpp";
  std::vector<TestCase> Expectations = {
      {"PolyBase::m", expected("PolyBase::m", "Root1::m", "Root2::m")},
      {"Root1::m", expected("Root1::m")},
      {"Root2::m", expected("Root2::m")},
      {"Root3::~Root3", expected("Root3::~Root3")},
  };
  EXPECT_TRUE(runTest(Code, Expectations));
}
