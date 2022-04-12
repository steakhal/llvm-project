//==- ClangSABackend.h - Declarations for Clang Static Analyzer backends --===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_UTILS_TABLEGEN_STATICANALYZER_CLANGSABACKENDS_H
#define LLVM_CLANG_UTILS_TABLEGEN_STATICANALYZER_CLANGSABACKENDS_H

namespace llvm {
class raw_ostream;
class RecordKeeper;
} // namespace llvm

namespace clang {
void EmitClangSACheckers(llvm::RecordKeeper &Records, llvm::raw_ostream &OS);
void EmitClangSAConfigs(llvm::RecordKeeper &Records, llvm::raw_ostream &OS);
} // namespace clang

#endif // LLVM_CLANG_UTILS_TABLEGEN_STATICANALYZER_CLANGSABACKENDS_H
