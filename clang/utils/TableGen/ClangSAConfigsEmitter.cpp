//=- ClangSACheckersEmitter.cpp - Generate Clang SA checkers tables -*- C++ -*-
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This tablegen backend emits Clang Static Analyzer config tables.
//
//===----------------------------------------------------------------------===//

#include "StaticAnalyzer/ConfigValues.h"
#include "StaticAnalyzer/DefPrinter.h"
#include "TableGenBackends.h"
#include "llvm/TableGen/Record.h"
#include "llvm/TableGen/TableGenBackend.h"

using namespace llvm;
using namespace clang;
using namespace ento;

static void printAnalyzerOptionCategories(const ParserContext &Ctx,
                                          raw_ostream &OS) {
  std::vector<const ConfigCategory *> SortedCategories;
  SortedCategories.reserve(Ctx.ConfigCategories.size());
  auto ByDisplayOrder = [](const auto *Lhs, const auto *Rhs) {
    return Lhs->DisplayOrder < Rhs->DisplayOrder;
  };

  for (const auto &Entry : Ctx.ConfigCategories)
    SortedCategories.push_back(&Entry.getValue());
  llvm::sort(SortedCategories, ByDisplayOrder);

  for (const auto *Cat : SortedCategories)
    OS << Cat->DisplayName << "\n";
}

void clang::EmitClangSAConfigs(RecordKeeper &Records, raw_ostream &OS) {
  ParserContext Ctx = ento::parseClangSATablegenFile(Records);
  printAnalyzerOptions(Ctx, OS);
  printAnalyzerOptionCategories(Ctx, OS);
}

// Invoke with:
// ./build/release/bin/clang-tblgen -gen-clang-sa-configs Foo.td
