//===--- RedundantLookupCheck.cpp - clang-tidy ----------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "RedundantLookupCheck.h"
#include "../utils/ExprSequence.h"
#include "../utils/OptionsUtils.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Basic/SourceManager.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SetOperations.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/DOTGraphTraits.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/Regex.h"

using namespace clang;
using namespace llvm;
using namespace clang::ast_matchers;

struct LookupNode {
  using NodeRef = const LookupNode *;
  SmallVector<const LookupNode *, 0> Children;
  const CallExpr *LookupCall;
  unsigned Line;
  unsigned Col;

  LookupNode(const SourceManager &SM, const CallExpr *LookupCall)
      : LookupCall(LookupCall),
        Line(SM.getPresumedLineNumber(LookupCall->getBeginLoc())),
        Col(SM.getPresumedColumnNumber(LookupCall->getBeginLoc())) {}
};
namespace llvm {

template <> struct GraphTraits<const LookupNode *> {
  using NodeRef = const LookupNode *;
  using ChildIteratorType = SmallVector<const LookupNode *, 0>::const_iterator;
  using nodes_iterator = llvm::df_iterator<const LookupNode *>;

  static NodeRef getEntryNode(const LookupNode *S) { return S; }

  static ChildIteratorType child_begin(NodeRef N) {
    if (N)
      return N->Children.begin();
    return ChildIteratorType();
  }

  static ChildIteratorType child_end(NodeRef N) {
    if (N)
      return N->Children.end();
    return ChildIteratorType();
  }

  static nodes_iterator nodes_begin(const LookupNode *S) { return df_begin(S); }

  static nodes_iterator nodes_end(const LookupNode *S) { return df_end(S); }
};

template <>
struct DOTGraphTraits<const LookupNode *> : public DefaultDOTGraphTraits {
  DOTGraphTraits(bool isSimple = false) : DefaultDOTGraphTraits(isSimple) {}

  static std::string getNodeLabel(const LookupNode *Node,
                                  const LookupNode *Graph) {
    std::string OutStr;
    llvm::raw_string_ostream Out(OutStr);
    Out << "Line " << Node->Line << ", Column " << Node->Col;
    return OutStr;
  }
};

} // namespace llvm

namespace clang::tidy::performance {

static constexpr auto DefaultContainerNameRegex = "set|map";

static const llvm::StringRef DefaultLookupMethodNames =
    llvm::StringLiteral( //
        "at;"
        "contains;"
        "count;"
        "find_as;"
        "find;"
        // These are tricky, as they take the "key" at different places.
        // They sometimes bundle up the key and the value together in a pair.
        //   "emplace;"
        //   "insert_or_assign;"
        //   "insert;"
        //   "try_emplace;"
        )
        .drop_back(); // Drops the last semicolon.

RedundantLookupCheck::RedundantLookupCheck(StringRef Name,
                                           ClangTidyContext *Context)
    : ClangTidyCheck(Name, Context),
      ContainerNameRegex(
          Options.get("ContainerNameRegex", DefaultContainerNameRegex)),
      LookupMethodNames(utils::options::parseStringList(
          Options.get("LookupMethodNames", DefaultLookupMethodNames))) {}

void RedundantLookupCheck::storeOptions(ClangTidyOptions::OptionMap &Opts) {
  Options.store(Opts, "ContainerNameRegex", ContainerNameRegex);
  Options.store(Opts, "LookupMethodNames",
                utils::options::serializeStringList(LookupMethodNames));
}

namespace {
/// Checks if any of the ends of the source range is in a macro expansion.
AST_MATCHER(Expr, hasMacroSourceRange) {
  SourceRange R = Node.getSourceRange();
  return R.getBegin().isMacroID() || R.getEnd().isMacroID();
}
} // namespace

static constexpr const char *ObjKey = "obj";
static constexpr const char *LookupKey = "key";
static constexpr const char *LookupCallKey = "lookup";
static constexpr const char *EnclosingFnKey = "fn";

void RedundantLookupCheck::registerMatchers(MatchFinder *Finder) {
  auto MatchesContainerNameRegex =
      matchesName(ContainerNameRegex, llvm::Regex::IgnoreCase);

  // Match that the expression is a record type with a name that contains "map"
  // or "set".
  auto RecordCalledMapOrSet =
      expr(ignoringImpCasts(hasType(hasUnqualifiedDesugaredType(recordType(
               hasDeclaration(namedDecl(MatchesContainerNameRegex)))))))
          .bind(ObjKey);

  auto SubscriptCall =
      cxxOperatorCallExpr(hasOverloadedOperatorName("[]"), argumentCountIs(2),
                          hasArgument(0, RecordCalledMapOrSet),
                          hasArgument(1, expr().bind(LookupKey)));

  auto LookupMethodCalls =
      cxxMemberCallExpr(on(RecordCalledMapOrSet), argumentCountIs(1),
                        hasArgument(0, expr().bind(LookupKey)),
                        callee(cxxMethodDecl(hasAnyName(LookupMethodNames))));

  // Match any lookup or subscript calls that are not in a macro expansion.
  auto AnyLookup = callExpr(unless(hasMacroSourceRange()),
                            anyOf(SubscriptCall, LookupMethodCalls))
                       .bind(LookupCallKey);

  // We need to collect all lookups in a function to be able to report them in
  // batches.
  Finder->addMatcher(
      functionDecl(hasBody(compoundStmt(forEachDescendant(AnyLookup))))
          .bind(EnclosingFnKey),
      this);
}

/// Hash the container object expr along with the key used for lookup and the
/// enclosing function together.
static unsigned hashLookupEvent(const ASTContext &Ctx,
                                const FunctionDecl *EnclosingFn,
                                const Expr *LookupKey,
                                const Expr *ContainerObject) {
  llvm::FoldingSetNodeID ID;
  ID.AddPointer(EnclosingFn);

  LookupKey->Profile(ID, Ctx, /*Canonical=*/true,
                     /*ProfileLambdaExpr=*/true);
  ContainerObject->Profile(ID, Ctx, /*Canonical=*/true,
                           /*ProfileLambdaExpr=*/true);
  return ID.ComputeHash();
}

void RedundantLookupCheck::check(const MatchFinder::MatchResult &Result) {
  Ctx = Result.Context;

  const auto *EnclosingFn =
      Result.Nodes.getNodeAs<FunctionDecl>(EnclosingFnKey);
  const auto *LookupCall = Result.Nodes.getNodeAs<CallExpr>(LookupCallKey);
  const auto *Key = Result.Nodes.getNodeAs<Expr>(LookupKey);
  const auto *ContainerObject = Result.Nodes.getNodeAs<Expr>(ObjKey);

  const unsigned LookupHash =
      hashLookupEvent(*Result.Context, EnclosingFn, ContainerObject, Key);
  RegisteredLookups.try_emplace({EnclosingFn, LookupHash})
      .first->second.insert(LookupCall);
}

void RedundantLookupCheck::onEndOfTranslationUnit() {
  if (!Ctx)
    return;
  auto ByBeginLoc = [&SM = Ctx->getSourceManager()](const CallExpr *Lookup1,
                                                    const CallExpr *Lookup2) {
    return SM.isBeforeInTranslationUnit(Lookup1->getBeginLoc(),
                                        Lookup2->getBeginLoc());
  };

  // Process the found lookups of each function.
  for (const auto &[GroupContext, LookupGroup] : RegisteredLookups) {
    if (LookupGroup.size() < 2)
      continue;

    const FunctionDecl *EnclosingFn = GroupContext.Context;
    // EnclosingFn->dumpColor();

    CFG::BuildOptions Options;
    Options.AddImplicitDtors = true;
    Options.AddTemporaryDtors = true;

    std::unique_ptr<CFG> TheCFG =
        CFG::buildCFG(EnclosingFn, EnclosingFn->getBody(), Ctx, Options);
    if (!TheCFG)
      continue;

    // auto &Policy = Ctx->getPrintingPolicy();
    utils::ExprSequence Sequence(TheCFG.get(), EnclosingFn->getBody(), Ctx);

    llvm::SmallPtrSet<const CallExpr *, 10> SeenDestinnations;
    llvm::SmallPtrSet<const CallExpr *, 10> UniqueNodes;
    llvm::SmallVector<std::pair<const CallExpr *, const CallExpr *>> GraphEdges;
    for (const CallExpr *A : LookupGroup) {
      for (const CallExpr *B : LookupGroup) {
        if (A == B)
          continue;
        if (Sequence.inSequence(A, B)) {
          UniqueNodes.insert(A);
          UniqueNodes.insert(B);
          SeenDestinnations.insert(B);
          GraphEdges.emplace_back(A, B);
          // llvm::errs() << "  ";
          // A->printPretty(llvm::errs(), nullptr, Policy);
          // llvm::errs() << " aka. " << A << "  SEQUENCED BEFORE  ";
          // B->printPretty(llvm::errs(), nullptr, Policy);
          // llvm::errs() << " aka. " << B << "\n";
        }
      }
    }

    auto &SM = Ctx->getSourceManager();

    llvm::SmallVector<LookupNode> GraphNodes;
    GraphNodes.reserve(UniqueNodes.size());
    llvm::DenseMap<const CallExpr *, LookupNode *> Translation;
    for (const auto &[From, To] : GraphEdges) {
      LookupNode *MappedFrom;

      if (Translation.contains(From)) {
        MappedFrom = Translation[From];
      } else {
        GraphNodes.push_back(LookupNode(SM, From));
        MappedFrom = &GraphNodes.back();
        Translation.try_emplace(From, MappedFrom);
      }

      LookupNode *MappedTo;
      if (Translation.contains(To)) {
        MappedTo = Translation[To];
      } else {
        GraphNodes.push_back(LookupNode(SM, To));
        MappedTo = &GraphNodes.back();
        Translation.try_emplace(To, MappedTo);
      }

      assert(Sequence.inSequence(From, To));
      MappedFrom->Children.push_back(MappedTo);
    }

    auto Roots = llvm::set_difference(UniqueNodes, SeenDestinnations);
    // llvm::errs() << "Number of root nodes: " << Roots.size() << "\n";

    for (const CallExpr *Root : Roots) {
      const auto *TranslatedRoot = Translation[Root];
      // llvm::ViewGraph(TranslatedRoot, "lookups");

      auto Begin =
          llvm::GraphTraits<const LookupNode *>::nodes_begin(TranslatedRoot);
      auto End =
          llvm::GraphTraits<const LookupNode *>::nodes_end(TranslatedRoot);
      llvm::SmallVector<const CallExpr *, 0> FinalGroup;
      for (const LookupNode *Node : llvm::make_range(Begin, End)) {
        FinalGroup.push_back(Node->LookupCall);
      }

      if (FinalGroup.size() < 2)
        continue;

      llvm::sort(FinalGroup, ByBeginLoc);
      const CallExpr *LastLookupCall = FinalGroup.back();
      diag(LastLookupCall->getBeginLoc(),
           "possibly redundant container lookups")
          << LastLookupCall->getSourceRange();

      for (const CallExpr *LookupCall : llvm::drop_end(FinalGroup)) {
        diag(LookupCall->getBeginLoc(), "previous lookup here",
             DiagnosticIDs::Note)
            << LookupCall->getSourceRange();
      }
    }

    // llvm::SmallVector<const CallExpr *> SortedGroup;
    // SortedGroup.reserve(LookupGroup.size());
    // llvm::append_range(SortedGroup, LookupGroup);
    // llvm::sort(SortedGroup, ByBeginLoc);

    // const CallExpr *FirstLookupCall = SortedGroup.front();
    // diag(FirstLookupCall->getBeginLoc(), "possibly redundant container
    // lookups")
    //     << FirstLookupCall->getSourceRange();

    // for (const CallExpr *LookupCall : llvm::drop_begin(SortedGroup)) {
    //   diag(LookupCall->getBeginLoc(), "next lookup here",
    //   DiagnosticIDs::Note)
    //       << LookupCall->getSourceRange();
    // }
  }

  RegisteredLookups.clear();
}

} // namespace clang::tidy::performance
