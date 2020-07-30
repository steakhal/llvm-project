//=== CStringLength.h Query and store the length of a cstring. ---*- C++ -*--=//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Defines an interface for interacting and manipulating the associated cstring
// length of a given memory region.
// You can assign a cstring length to any memory region.
// The represented value is what strlen would return on the given memory region.
// Eg: 3 for both "ABC" and "abc\00def".
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_STATICANALYZER_CHECKERS_CSTRINGLENGTH_H
#define LLVM_CLANG_LIB_STATICANALYZER_CHECKERS_CSTRINGLENGTH_H

#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramState.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/SVals.h"

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
LLVM_NODISCARD SVal getCStringLength(CheckerContext &Ctx,
                                     ProgramStateRef &State, const Expr *Ex,
                                     SVal Buf);

// TODO: Don't use.
LLVM_NODISCARD NonLoc createCStringLength(ProgramStateRef &State,
                                          CheckerContext &Ctx, const Expr *Ex,
                                          const MemRegion *MR);

LLVM_DUMP_METHOD void dumpCStringLengths(ProgramStateRef State,
                                         raw_ostream &Out = llvm::errs(),
                                         const char *NL = "\n",
                                         const char *Sep = " : ");
} // namespace cstring
} // namespace ento
} // namespace clang

#endif
