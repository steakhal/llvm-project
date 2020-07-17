//=== TODO. --------*- C++ -*-//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// TODO.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_STATICANALYZER_CHECKERS_CSTRINGLENGTH_H
#define LLVM_CLANG_LIB_STATICANALYZER_CHECKERS_CSTRINGLENGTH_H

#include "clang/StaticAnalyzer/Core/BugReporter/BugReporterVisitors.h"
#include "clang/StaticAnalyzer/Core/CheckerManager.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramState.h"

namespace clang {
namespace ento {
namespace cstring {

/// TODO.
LLVM_NODISCARD ProgramStateRef setCStringLength(ProgramStateRef State,
                                                const MemRegion *MR,
                                                SVal StrLength);

/// TODO.
LLVM_NODISCARD ProgramStateRef removeCStringLength(ProgramStateRef State,
                                                   const MemRegion *MR);

/// TODO: do we even need this API? What should it do?
/// What is the hypothetical parameter? wut
LLVM_NODISCARD SVal getCStringLengthForRegion(CheckerContext &C,
                                              ProgramStateRef &state,
                                              const Expr *Ex,
                                              const MemRegion *MR,
                                              bool hypothetical);

/// TODO: same comments here.
/// TODO: shouldn't we have a simpler API? eg:
/// 'getCStringLength(ProgramStateRef, const MemRegion *) -> SVal'?
LLVM_NODISCARD SVal getCStringLength(CheckerContext &C, ProgramStateRef &state,
                                     const Expr *Ex, SVal Buf,
                                     bool hypothetical = false);

LLVM_DUMP_METHOD void dumpCStringLengths(ProgramStateRef State,
                                         raw_ostream &Out = llvm::errs(),
                                         const char *NL = "\n",
                                         const char *Sep = " : ");

/// \deprecated { PLS dont use it, put it somewhere else or dont know. It is
/// only used by  }
LLVM_ATTRIBUTE_DEPRECATED(const StringLiteral *getCStringLiteral(SVal val),
                          "TODO: Use something else?? instead.");
} // namespace cstring
} // namespace ento
} // namespace clang

#endif
