//=== CStringLength.h Stores the length of a cstring. ------------*- C++ -*--=//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Defines an interface for interacting and manipulating the associated cstring
// length of a given memory region.
// You can assign a cstring length to any memory region, representing the first
// zero terminator in that region.
// Eg: "abc\00def" -> 4
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_STATICANALYZER_CHECKERS_CSTRINGLENGTH_H
#define LLVM_CLANG_LIB_STATICANALYZER_CHECKERS_CSTRINGLENGTH_H

#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramState.h"

namespace clang {
namespace ento {
class CheckerContext;

namespace cstring {

/// Assigns a cstring length to a memory region.
LLVM_NODISCARD ProgramStateRef setCStringLength(ProgramStateRef State,
                                                const MemRegion *MR,
                                                SVal StrLength);

/// Removes the assigned cstring length from the memory region.
/// It is useful for invalidation.
LLVM_NODISCARD ProgramStateRef removeCStringLength(ProgramStateRef State,
                                                   const MemRegion *MR);

// FIXME: Eventually rework the interface of this function.
//  Especially the magic 'Hypothetical' parameter.
LLVM_NODISCARD SVal getCStringLength(CheckerContext &Ctx,
                                     ProgramStateRef &State, const Expr *Ex,
                                     SVal Buf, bool Hypothetical = false);

LLVM_DUMP_METHOD void dumpCStringLengths(ProgramStateRef State,
                                         raw_ostream &Out = llvm::errs(),
                                         const char *NL = "\n",
                                         const char *Sep = " : ");
} // namespace cstring
} // namespace ento
} // namespace clang

#endif
