//===- SemanticChecks.h - Semantic checks of ConfigValues -------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

namespace llvm {
class RecordKeeper;
} // namespace llvm

namespace clang {
namespace ento {
struct ParserContext;
void performSemanticChecks(llvm::RecordKeeper &Records,
                           const ParserContext &Ctx);

} // namespace ento
} // namespace clang
