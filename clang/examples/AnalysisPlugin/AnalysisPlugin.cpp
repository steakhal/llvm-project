//===- AnalysisPlugin.cpp -------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/Expr.h"
#include "clang/CrossTU/CrossTranslationUnit.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/Frontend/MultiplexConsumer.h"
#include "clang/Serialization/ASTWriter.h"
#include "clang/StaticAnalyzer/Checkers/DynamicType.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/raw_ostream.h"

#include <memory>
#include <optional>

using namespace clang;

namespace {
class ConsumerWithPath : public ASTConsumer {
public:
  explicit ConsumerWithPath(llvm::StringRef Path) : Path(Path) {}

  std::string Path;
};

// Copy-pasted from clang/tools/clang-extdef-mapping/ClangExtDefMapGen.cpp
// FIXME: We should probably factor out the ASTConsumer and use it here.
class USRDumpConsumer final : public ConsumerWithPath {
public:
  using ConsumerWithPath::ConsumerWithPath;
  void Initialize(ASTContext &Context) override {
    Ctx = &Context;
    SM = &Ctx->getSourceManager();
  }

  void HandleTranslationUnit(ASTContext &Context) override {
    std::error_code EC;
    auto OS = std::make_unique<llvm::raw_fd_ostream>(Path.data(), EC);
    if (EC) {
      llvm::errs() << "ERROR: Could not create the USRs output file: "
                   << EC.message() << "\n";
      return;
    }

    handleDecl(Context.getTranslationUnitDecl());
    *OS << cross_tu::createCrossTUIndexString(Index);
    OS->flush();
  }

private:
  void handleDecl(const Decl *D);
  void addIfInMain(const DeclaratorDecl *DD, SourceLocation defStart);

  ASTContext *Ctx;
  SourceManager *SM;
  llvm::StringMap<std::string> Index;
  std::string CurrentFileName;
};

class OverridersAnalysisConsumer final : public ConsumerWithPath {
public:
  OverridersAnalysisConsumer(StringRef Path, ento::DynamicTypeAnalysis &Ana)
      : ConsumerWithPath(Path), Ana(Ana) {}

  void HandleTranslationUnit(ASTContext &) override {
    dumpDynamicTypeAnalysis(Ana, Path.data());
  }

  ento::DynamicTypeAnalysis &Ana;
};

static std::string
getOutputPathForPlugin(const CompilerInstance &CI,
                       llvm::StringRef PluginOutputFileExtension) {
  llvm::SmallString<500> AlternativeOutput =
      StringRef(CI.getFrontendOpts().OutputFile);
  llvm::sys::path::replace_extension(AlternativeOutput,
                                     PluginOutputFileExtension);
  return AlternativeOutput.str().str();
}

template <class Derived> class ActionTemplate : public PluginASTAction {
public:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 llvm::StringRef) override {
    return std::make_unique<typename Derived::Consumer>(
        getOutputPathForPlugin(CI, Derived::PluginOutputFileExtension));
  }

  bool ParseArgs(const CompilerInstance &CI,
                 const std::vector<std::string> &arg) override {
    if (CI.getFrontendOpts().OutputFile.empty()) {
      llvm::errs() << "Plugin '" << Derived::PluginName
                   << "' requires the -o flag.\n";
      return false;
    }
    return true;
  }

  PluginASTAction::ActionType getActionType() final {
    return CmdlineBeforeMainAction;
  }
};

class MyASTDumpAction final : public ActionTemplate<MyASTDumpAction> {
public:
  using Consumer = struct DummyConsumer : ASTConsumer {
    DummyConsumer(...) {}
  };
  static constexpr llvm::StringLiteral PluginOutputFileExtension = ".ast";
  static constexpr llvm::StringLiteral PluginName = "dump-the-ast";
  static constexpr llvm::StringLiteral PluginDescription = "TODO Description";

  std::unique_ptr<ASTConsumer>
  CreateASTConsumer(CompilerInstance &CI, llvm::StringRef InFile) override {
    class PCHConsumerCreator : public GeneratePCHAction {
    public:
      using GeneratePCHAction::CreateASTConsumer;
    };

    SaveAndRestore OverrideOutput(
        CI.getFrontendOpts().OutputFile,
        getOutputPathForPlugin(CI, PluginOutputFileExtension));
    auto Consumer =
        std::make_unique<PCHConsumerCreator>()->CreateASTConsumer(CI, InFile);
    return Consumer;
  }
};

class USRDumpAction final : public ActionTemplate<USRDumpAction> {
public:
  using Consumer = USRDumpConsumer;
  static constexpr llvm::StringLiteral PluginOutputFileExtension = ".usrs";
  static constexpr llvm::StringLiteral PluginName = "dump-the-usrs";
  static constexpr llvm::StringLiteral PluginDescription = "TODO Description";
};

class DumpTheOverridersAction final
    : public ActionTemplate<DumpTheOverridersAction> {
public:
  using Consumer = struct DummyConsumer : ASTConsumer {
    DummyConsumer(...) {}
  };
  static constexpr llvm::StringLiteral PluginOutputFileExtension =
      ".overriders";
  static constexpr llvm::StringLiteral PluginName = "dump-the-overriders";
  static constexpr llvm::StringLiteral PluginDescription = "TODO Description";

  std::unique_ptr<ASTConsumer>
  CreateASTConsumer(CompilerInstance &CI, llvm::StringRef InFile) override {
    std::vector<std::unique_ptr<ASTConsumer>> Consumers;
    ento::DynamicTypeAnalysis &Ana = ento::attachDynamicTypeAnalysis(Consumers);
    Consumers.emplace_back(std::make_unique<OverridersAnalysisConsumer>(
        getOutputPathForPlugin(CI, PluginOutputFileExtension), Ana));
    return std::make_unique<MultiplexConsumer>(std::move(Consumers));
  }
};

} // namespace

static FrontendPluginRegistry::Add<MyASTDumpAction>
    DumpTheAST(MyASTDumpAction::PluginName, MyASTDumpAction::PluginDescription);
static FrontendPluginRegistry::Add<USRDumpAction>
    DumpTheUSRs(USRDumpAction::PluginName, USRDumpAction::PluginDescription);
static FrontendPluginRegistry::Add<DumpTheOverridersAction>
    DumpTheOverriders(DumpTheOverridersAction::PluginName,
                      DumpTheOverridersAction::PluginDescription);

void USRDumpConsumer::handleDecl(const Decl *D) {
  if (!D)
    return;

  if (const auto *FD = dyn_cast<FunctionDecl>(D)) {
    if (FD->isThisDeclarationADefinition())
      if (const Stmt *Body = FD->getBody())
        addIfInMain(FD, Body->getBeginLoc());
  } else if (const auto *VD = dyn_cast<VarDecl>(D)) {
    if (cross_tu::shouldImport(VD, *Ctx) && VD->hasInit())
      if (const Expr *Init = VD->getInit())
        addIfInMain(VD, Init->getBeginLoc());
  }

  if (const auto *DC = dyn_cast<DeclContext>(D))
    for (const Decl *D : DC->decls())
      handleDecl(D);
}

void USRDumpConsumer::addIfInMain(const DeclaratorDecl *DD,
                                  SourceLocation defStart) {
  std::optional<std::string> LookupName =
      cross_tu::CrossTranslationUnitContext::getLookupName(DD);
  if (!LookupName)
    return;
  assert(!LookupName->empty() && "Lookup name should be non-empty.");

  if (CurrentFileName.empty()) {
    CurrentFileName = std::string(
        SM->getFileEntryForID(SM->getMainFileID())->tryGetRealPathName());
    if (CurrentFileName.empty())
      CurrentFileName = "invalid_file";
  }

  switch (DD->getLinkageInternal()) {
  case Linkage::External:
  case Linkage::VisibleNone:
  case Linkage::UniqueExternal:
    if (SM->isInMainFile(defStart))
      Index[*LookupName] = CurrentFileName;
    break;
  case Linkage::Invalid:
    llvm_unreachable("Linkage has not been computed!");
  default:
    break;
  }
}
