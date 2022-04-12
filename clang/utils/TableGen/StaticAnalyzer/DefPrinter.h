//===- DefPrinter.h                                                     ---===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_UTILS_TABLEGEN_STATICANALYZER_DEFPRINTER_H
#define LLVM_CLANG_UTILS_TABLEGEN_STATICANALYZER_DEFPRINTER_H

namespace llvm {
class raw_ostream;
} // namespace llvm

namespace clang {
namespace ento {
struct ParserContext;
void printAnalyzerOptions(const ParserContext &Ctx, llvm::raw_ostream &OS);
} // namespace ento
} // namespace clang

#endif // LLVM_CLANG_UTILS_TABLEGEN_STATICANALYZER_DEFPRINTER_H
