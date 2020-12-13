//===- unittests/Analysis/MacroExpansionContextTest.cpp - -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "clang/Analysis/MacroExpansionContext.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/DiagnosticOptions.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TargetOptions.h"
#include "clang/Lex/HeaderSearch.h"
#include "clang/Lex/HeaderSearchOptions.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/PreprocessorOptions.h"
#include "clang/Parse/Parser.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

// static bool HACK_EnableDebugInUnitTest = (::llvm::DebugFlag = true);

namespace clang {
namespace analysis {
namespace {

class MacroExpansionContextTest : public ::testing::Test {
protected:
  MacroExpansionContextTest()
      : InMemoryFileSystem(new llvm::vfs::InMemoryFileSystem),
        FileMgr(FileSystemOptions(), InMemoryFileSystem),
        DiagID(new DiagnosticIDs()), DiagOpts(new DiagnosticOptions()),
        Diags(DiagID, DiagOpts.get(), new IgnoringDiagConsumer()),
        SourceMgr(Diags, FileMgr), TargetOpts(new TargetOptions()) {
    TargetOpts->Triple = "x86_64-pc-linux-unknown";
    Target = TargetInfo::CreateTargetInfo(Diags, TargetOpts);
    LangOpts.CPlusPlus20 = 1; // For __VA_OPT__
  }

  IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem> InMemoryFileSystem;
  FileManager FileMgr;
  IntrusiveRefCntPtr<DiagnosticIDs> DiagID;
  IntrusiveRefCntPtr<DiagnosticOptions> DiagOpts;
  DiagnosticsEngine Diags;
  SourceManager SourceMgr;
  LangOptions LangOpts;
  std::shared_ptr<TargetOptions> TargetOpts;
  IntrusiveRefCntPtr<TargetInfo> Target;

  std::unique_ptr<MacroExpansionContext>
  getMacroExpansionContextFor(StringRef SourceText) {
    std::unique_ptr<llvm::MemoryBuffer> Buf =
        llvm::MemoryBuffer::getMemBuffer(SourceText);
    SourceMgr.setMainFileID(SourceMgr.createFileID(std::move(Buf)));
    TrivialModuleLoader ModLoader;
    HeaderSearch HeaderInfo(std::make_shared<HeaderSearchOptions>(), SourceMgr,
                            Diags, LangOpts, Target.get());
    Preprocessor PP(std::make_shared<PreprocessorOptions>(), Diags, LangOpts,
                    SourceMgr, HeaderInfo, ModLoader,
                    /*IILookup =*/nullptr,
                    /*OwnsHeaderSearch =*/false);

    PP.Initialize(*Target);
    auto Ctx = std::make_unique<MacroExpansionContext>(PP, LangOpts);

    // Lex source text.
    PP.EnterMainSourceFile();

    while (true) {
      Token Tok;
      PP.Lex(Tok);
      if (Tok.is(tok::eof))
        break;
    }

    // Callbacks have been executed at this point.
    return Ctx;
  }

  /// Returns the expansion location to main file at the given row and column.
  SourceLocation at(unsigned row, unsigned col) const {
    SourceLocation Loc =
        SourceMgr.translateLineCol(SourceMgr.getMainFileID(), row, col);
    return SourceMgr.getExpansionLoc(Loc);
  }
};

TEST_F(MacroExpansionContextTest, EmptyExpansions) {
  const auto Ctx = getMacroExpansionContextFor(R"code(
  #define EMPTY
  A b cd EMPTY ef EMPTY gh
EMPTY zz
      )code");
  // After preprocessing:
  //  A b cd ef gh
  //      zz

  EXPECT_EQ("", Ctx->getExpandedMacroForLocation(at(3, 10)));
  EXPECT_EQ("EMPTY", Ctx->getSubstitutedTextForLocation(at(3, 10)));

  EXPECT_EQ("", Ctx->getExpandedMacroForLocation(at(3, 19)));
  EXPECT_EQ("EMPTY", Ctx->getSubstitutedTextForLocation(at(3, 19)));

  EXPECT_EQ("", Ctx->getExpandedMacroForLocation(at(4, 1)));
  EXPECT_EQ("EMPTY", Ctx->getSubstitutedTextForLocation(at(4, 1)));
}

TEST_F(MacroExpansionContextTest, TransitiveExpansions) {
  const auto Ctx = getMacroExpansionContextFor(R"code(
  #define EMPTY
  #define WOOF EMPTY ) EMPTY   1
  A b cd WOOF ef EMPTY gh
      )code");
  // After preprocessing:
  //  A b cd ) 1 ef gh

  EXPECT_EQ(")1", Ctx->getExpandedMacroForLocation(at(4, 10)));
  EXPECT_EQ("WOOF", Ctx->getSubstitutedTextForLocation(at(4, 10)));

  EXPECT_EQ("", Ctx->getExpandedMacroForLocation(at(4, 18)));
  EXPECT_EQ("EMPTY", Ctx->getSubstitutedTextForLocation(at(4, 18)));
}

TEST_F(MacroExpansionContextTest, MacroFunctions) {
  const auto Ctx = getMacroExpansionContextFor(R"code(
  #define EMPTY
  #define WOOF(x) x(EMPTY ) )  ) EMPTY   1
  A b cd WOOF($$ ef) EMPTY gh
  WOOF(WOOF)
  WOOF(WOOF(bar barr))),,),')
      )code");
  // After preprocessing:
  //  A b cd $$ ef( ) ) ) 1 gh
  //  WOOF( ) ) ) 1
  //  bar barr( ) ) ) 1( ) ) ) 1),,),')

  EXPECT_EQ("$$ ef ()))1", Ctx->getExpandedMacroForLocation(at(4, 10)));
  EXPECT_EQ("WOOF($$ ef)", Ctx->getSubstitutedTextForLocation(at(4, 10)));

  EXPECT_EQ("", Ctx->getExpandedMacroForLocation(at(4, 22)));
  EXPECT_EQ("EMPTY", Ctx->getSubstitutedTextForLocation(at(4, 22)));

  EXPECT_EQ("WOOF ()))1", Ctx->getExpandedMacroForLocation(at(5, 3)));
  EXPECT_EQ("WOOF(WOOF)", Ctx->getSubstitutedTextForLocation(at(5, 3)));

  EXPECT_EQ("bar barr ()))1()))1", Ctx->getExpandedMacroForLocation(at(6, 3)));
  EXPECT_EQ("WOOF(WOOF(bar barr))",
            Ctx->getSubstitutedTextForLocation(at(6, 3)));
}

TEST_F(MacroExpansionContextTest, VariadicMacros) {
  // From the GCC website.
  const auto Ctx = getMacroExpansionContextFor(R"code(
  #define eprintf(format, ...) fprintf (stderr, format, __VA_ARGS__)
  eprintf("success!\n", );
  eprintf("success!\n");

  #define eprintf2(format, ...) \
    fprintf (stderr, format __VA_OPT__(,) __VA_ARGS__)
  eprintf2("success!\n", );
  eprintf2("success!\n");
      )code");
  // After preprocessing:
  //  fprintf (stderr, "success!\n", );
  //  fprintf (stderr, "success!\n", );
  //  fprintf (stderr, "success!\n" );
  //  fprintf (stderr, "success!\n" );

  EXPECT_EQ(R"(fprintf (stderr ,"success!\n",))",
            Ctx->getExpandedMacroForLocation(at(3, 3)));
  EXPECT_EQ(R"(eprintf("success!\n", ))",
            Ctx->getSubstitutedTextForLocation(at(3, 3)));

  EXPECT_EQ(R"(fprintf (stderr ,"success!\n",))",
            Ctx->getExpandedMacroForLocation(at(4, 3)));
  EXPECT_EQ(R"(eprintf("success!\n"))",
            Ctx->getSubstitutedTextForLocation(at(4, 3)));

  EXPECT_EQ(R"(fprintf (stderr ,"success!\n"))",
            Ctx->getExpandedMacroForLocation(at(8, 3)));
  EXPECT_EQ(R"(eprintf2("success!\n", ))",
            Ctx->getSubstitutedTextForLocation(at(8, 3)));

  EXPECT_EQ(R"(fprintf (stderr ,"success!\n"))",
            Ctx->getExpandedMacroForLocation(at(9, 3)));
  EXPECT_EQ(R"(eprintf2("success!\n"))",
            Ctx->getSubstitutedTextForLocation(at(9, 3)));
}

TEST_F(MacroExpansionContextTest, ConcatenationMacros) {
  // From the GCC website.
  const auto Ctx = getMacroExpansionContextFor(R"code(
  #define COMMAND(NAME)  { #NAME, NAME ## _command }
  struct command commands[] = {
    COMMAND(quit),
    COMMAND(help),
  };)code");
  // After preprocessing:
  //  struct command commands[] = {
  //    { "quit", quit_command },
  //    { "help", help_command },
  //  };

  EXPECT_EQ(R"({"quit",quit_command })",
            Ctx->getExpandedMacroForLocation(at(4, 5)));
  EXPECT_EQ("COMMAND(quit)", Ctx->getSubstitutedTextForLocation(at(4, 5)));

  EXPECT_EQ(R"({"help",help_command })",
            Ctx->getExpandedMacroForLocation(at(5, 5)));
  EXPECT_EQ("COMMAND(help)", Ctx->getSubstitutedTextForLocation(at(5, 5)));
}

TEST_F(MacroExpansionContextTest, StringizingMacros) {
  // From the GCC website.
  const auto Ctx = getMacroExpansionContextFor(R"code(
  #define WARN_IF(EXP) \
  do { if (EXP) \
          fprintf (stderr, "Warning: " #EXP "\n"); } \
  while (0)
  WARN_IF (x == 0);

  #define xstr(s) str(s)
  #define str(s) #s
  #define foo 4
  str (foo)
  xstr (foo)
      )code");
  // After preprocessing:
  //  do { if (x == 0) fprintf (stderr, "Warning: " "x == 0" "\n"); } while (0);
  //  "foo"
  //  "4"

  EXPECT_EQ(
      R"(do {if (x ==0)fprintf (stderr ,"Warning: ""x == 0""\n");}while (0))",
      Ctx->getExpandedMacroForLocation(at(6, 3)));
  EXPECT_EQ("WARN_IF (x == 0)", Ctx->getSubstitutedTextForLocation(at(6, 3)));

  EXPECT_EQ(R"("foo")", Ctx->getExpandedMacroForLocation(at(11, 3)));
  EXPECT_EQ("str (foo)", Ctx->getSubstitutedTextForLocation(at(11, 3)));

  EXPECT_EQ(R"("4")", Ctx->getExpandedMacroForLocation(at(12, 3)));
  EXPECT_EQ("xstr (foo)", Ctx->getSubstitutedTextForLocation(at(12, 3)));
}

TEST_F(MacroExpansionContextTest, StringizingVariadicMacros) {
  const auto Ctx = getMacroExpansionContextFor(R"code(
  #define xstr(...) str(__VA_ARGS__)
  #define str(...) #__VA_ARGS__
  #define RParen2x ) )
  #define EMPTY
  #define f(x, ...) __VA_ARGS__ ! x * x
  #define g(...) zz EMPTY f(__VA_ARGS__ ! x) f() * y
  #define h(x, G) G(x) G(x ## x RParen2x
  #define q(G) h(apple, G(apple)) RParen2x

  q(g)
  q(xstr)
  g(RParen2x)
  f( RParen2x )s
      )code");
  // clang-format off
  // After preprocessing:
  //  zz ! apple ! x * apple ! x ! * * y(apple) zz ! apple ! x * apple ! x ! * * y(appleapple ) ) ) )
  //  "apple"(apple) "apple"(appleapple ) ) ) )
  //  zz ! * ) ! x) ! * * y
  //  ! ) ) * ) )
  // clang-format on

  EXPECT_EQ("zz !apple !x *apple !x !**y (apple )zz !apple !x *apple !x !**y "
            "(appleapple ))))",
            Ctx->getExpandedMacroForLocation(at(11, 3)));
  EXPECT_EQ("q(g)", Ctx->getSubstitutedTextForLocation(at(11, 3)));

  EXPECT_EQ(R"res("apple"(apple )"apple"(appleapple )))))res",
            Ctx->getExpandedMacroForLocation(at(12, 3)));
  EXPECT_EQ("q(xstr)", Ctx->getSubstitutedTextForLocation(at(12, 3)));

  EXPECT_EQ("zz !*)!x )!**y ", Ctx->getExpandedMacroForLocation(at(13, 3)));
  EXPECT_EQ("g(RParen2x)", Ctx->getSubstitutedTextForLocation(at(13, 3)));

  EXPECT_EQ("!))*))", Ctx->getExpandedMacroForLocation(at(14, 3)));
  EXPECT_EQ("f( RParen2x )", Ctx->getSubstitutedTextForLocation(at(14, 3)));
}

TEST_F(MacroExpansionContextTest, RedefUndef) {
  const auto Ctx = getMacroExpansionContextFor(R"code(
  #define Hi(x) Welcome x
  Hi(Adam)
  #define Hi Willkommen
  Hi Hans
  #undef Hi
  Hi(Hi)
      )code");
  // After preprocessing:
  //  Welcome Adam
  //  Willkommen Hans
  //  Hi(Hi)

  // FIXME: Extra space follows every identifier.
  EXPECT_EQ("Welcome Adam ", Ctx->getExpandedMacroForLocation(at(3, 3)));
  EXPECT_EQ("Hi(Adam)", Ctx->getSubstitutedTextForLocation(at(3, 3)));

  EXPECT_EQ("Willkommen ", Ctx->getExpandedMacroForLocation(at(5, 3)));
  EXPECT_EQ("Hi", Ctx->getSubstitutedTextForLocation(at(5, 3)));

  // There was no macro expansion at 7:3, empty returned in that case.
  EXPECT_EQ("", Ctx->getExpandedMacroForLocation(at(7, 3)));
  EXPECT_EQ("", Ctx->getSubstitutedTextForLocation(at(7, 3)));
}

TEST_F(MacroExpansionContextTest, UnbalacedParenthesis) {
  const auto Ctx = getMacroExpansionContextFor(R"code(
  #define retArg(x) x
  #define retArgUnclosed retArg(fun()
  #define BB CC
  #define applyInt BB(int)
  #define CC(x) retArgUnclosed

  applyInt );

  #define expandArgUnclosedCommaExpr(x) (x, fun(), 1
  #define f expandArgUnclosedCommaExpr

  int x =  f(f(1))  ));
      )code");
  // After preprocessing:
  //  fun();
  //  int x = ((1, fun(), 1, fun(), 1 ));

  EXPECT_EQ("fun ()", Ctx->getExpandedMacroForLocation(at(8, 3)));
  EXPECT_EQ("applyInt )", Ctx->getSubstitutedTextForLocation(at(8, 3)));

  EXPECT_EQ("((1,fun (),1,fun (),1",
            Ctx->getExpandedMacroForLocation(at(13, 12)));
  EXPECT_EQ("f(f(1))", Ctx->getSubstitutedTextForLocation(at(13, 12)));
}

} // namespace
} // namespace analysis
} // namespace clang
