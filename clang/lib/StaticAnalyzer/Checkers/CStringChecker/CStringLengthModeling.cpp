//=== CStringLengthModeling.cpp  Implementation of CStringLength API C++ -*--=//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Implements the CStringLength API and the CStringChecker bookkeeping parts.
// Updates the associated cstring lengths of memory regions:
//  - Infers the cstring length of string literals.
//  - Removes cstring length associations of dead symbols.
//  - Handles region invalidation.
//
//===----------------------------------------------------------------------===//

#include "CStringChecker.h"
#include "CStringLength.h"

#include "clang/StaticAnalyzer/Core/BugReporter/BugReporter.h"
#include "clang/StaticAnalyzer/Core/CheckerManager.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramStateTrait.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace ento;
using namespace cstring;

/// Associates an strlen to a memory region.
REGISTER_MAP_WITH_PROGRAMSTATE(CStringLengthMap, const MemRegion *, SVal)

//===----------------------------------------------------------------------===//
// Implementation of the public CStringLength API.
//===----------------------------------------------------------------------===//

ProgramStateRef cstring::setCStringLength(ProgramStateRef State,
                                          const MemRegion *MR, SVal StrLength) {
  assert(!StrLength.isUndef() && "Attempt to set an undefined string length");

  MR = MR->StripCasts();

  switch (MR->getKind()) {
  case MemRegion::StringRegionKind:
    // FIXME: This can happen if we strcpy() into a string region. This is
    // undefined [C99 6.4.5p6], but we should still warn about it.
    return State;

  case MemRegion::SymbolicRegionKind:
  case MemRegion::AllocaRegionKind:
  case MemRegion::NonParamVarRegionKind:
  case MemRegion::ParamVarRegionKind:
  case MemRegion::FieldRegionKind:
  case MemRegion::ObjCIvarRegionKind:
    // These are the types we can currently track string lengths for.
    break;

  case MemRegion::ElementRegionKind:
    // FIXME: Handle element regions by upper-bounding the parent region's
    // string length.
    return State;

  default:
    // Other regions (mostly non-data) can't have a reliable C string length.
    // For now, just ignore the change.
    // FIXME: These are rare but not impossible. We should output some kind of
    // warning for things like strcpy((char[]){'a', 0}, "b");
    return State;
  }

  if (StrLength.isUnknown())
    return removeCStringLength(State, MR);
  return State->set<CStringLengthMap>(MR, StrLength);
}

ProgramStateRef cstring::removeCStringLength(ProgramStateRef State,
                                             const MemRegion *MR) {
  return State->remove<CStringLengthMap>(MR);
}

NonLoc cstring::createCStringLength(ProgramStateRef &State, CheckerContext &Ctx,
                                    const Expr *Ex, const MemRegion *MR) {
  assert(Ex);
  assert(MR);

  SValBuilder &SVB = Ctx.getSValBuilder();
  QualType SizeTy = SVB.getContext().getSizeType();
  NonLoc CStrLen =
      SVB.getMetadataSymbolVal(CStringChecker::getTag(), MR, Ex, SizeTy,
                               Ctx.getLocationContext(), Ctx.blockCount())
          .castAs<NonLoc>();

  // Implicitly constrain the range to SIZE_MAX/4
  BasicValueFactory &BVF = SVB.getBasicValueFactory();
  const llvm::APSInt &MaxValue = BVF.getMaxValue(SizeTy);
  const llvm::APSInt Four = APSIntType(MaxValue).getValue(4);
  const llvm::APSInt *MaxLength = BVF.evalAPSInt(BO_Div, MaxValue, Four);
  const NonLoc MaxLengthSVal = SVB.makeIntVal(*MaxLength);
  SVal Constrained =
      SVB.evalBinOpNN(State, BO_LE, CStrLen, MaxLengthSVal, SizeTy);
  State = State->assume(Constrained.castAs<DefinedOrUnknownSVal>(), true);
  State = State->set<CStringLengthMap>(MR, CStrLen);
  return CStrLen;
}

Optional<SVal> cstring::getCStringLength(CheckerContext &Ctx,
                                         const ProgramStateRef &State,
                                         SVal Buf) {
  if (Buf.isUnknownOrUndef())
    return Buf;

  if (Buf.getAs<loc::GotoLabel>())
    return UndefinedVal();

  // If it's not a region, give up.
  const MemRegion *MR = Buf.getAsRegion();
  if (!MR)
    return UnknownVal();

  // If we have a region, strip casts from it and see if we can figure out
  // its length. For anything we can't figure out, just return UnknownVal.
  MR = MR->StripCasts();

  switch (MR->getKind()) {
  case MemRegion::StringRegionKind: {
    // Modifying the contents of string regions is undefined [C99 6.4.5p6],
    // so we can assume that the byte length is the correct C string length.
    SValBuilder &SVB = Ctx.getSValBuilder();
    QualType SizeTy = SVB.getContext().getSizeType();
    const StringLiteral *StrLiteral =
        cast<StringRegion>(MR)->getStringLiteral();
    return SVB.makeIntVal(StrLiteral->getByteLength(), SizeTy);
  }
  case MemRegion::SymbolicRegionKind:
  case MemRegion::AllocaRegionKind:
  case MemRegion::NonParamVarRegionKind:
  case MemRegion::ParamVarRegionKind:
  case MemRegion::FieldRegionKind:
  case MemRegion::ObjCIvarRegionKind:
    if (const SVal *RecordedLength = State->get<CStringLengthMap>(MR))
      return *RecordedLength;
    return llvm::None;
  case MemRegion::CompoundLiteralRegionKind:
    // FIXME: Can we track this? Is it necessary?
    return UnknownVal();
  case MemRegion::ElementRegionKind:
    // FIXME: How can we handle this? It's not good enough to subtract the
    // offset from the base string length; consider "123\x00567" and &a[5].
    return UnknownVal();
  default:
    // Other regions (mostly non-data) can't have a reliable C string length.
    return UndefinedVal();
  }
}

void cstring::dumpCStringLengths(const ProgramStateRef State, raw_ostream &Out,
                                 const char *NL, const char *Sep) {
  const CStringLengthMapTy Items = State->get<CStringLengthMap>();
  if (!Items.isEmpty())
    Out << "CString lengths:" << NL;
  for (const auto &Item : Items) {
    Item.first->dumpToStream(Out);
    Out << Sep;
    Item.second.dumpToStream(Out);
    Out << NL;
  }
}

//===----------------------------------------------------------------------===//
// Implementation of the tracking and bookkeeping part of the CStringChecker.
// Updates the CStringLengthMap.
// - Infers the cstring length of string literals.
// - Removes cstring length associations of dead symbols.
// - Handles region invalidation.
//===----------------------------------------------------------------------===//

void *CStringChecker::getTag() {
  static int Tag;
  return &Tag;
}

void CStringChecker::checkPreStmt(const DeclStmt *DS, CheckerContext &C) const {
  // Record string length for char a[] = "abc";
  ProgramStateRef state = C.getState();

  for (const auto *I : DS->decls()) {
    const VarDecl *D = dyn_cast<VarDecl>(I);
    if (!D)
      continue;

    // FIXME: Handle array fields of structs.
    if (!D->getType()->isArrayType())
      continue;

    const Expr *Init = D->getInit();
    if (!Init)
      continue;
    if (!isa<StringLiteral>(Init))
      continue;

    Loc VarLoc = state->getLValue(D, C.getLocationContext());
    const MemRegion *MR = VarLoc.getAsRegion();
    if (!MR)
      continue;

    SVal StrVal = C.getSVal(Init);
    assert(StrVal.isValid() && "Initializer string is unknown or undefined");
    DefinedOrUnknownSVal strLength =
        getCStringLength(C, state, StrVal)->castAs<DefinedOrUnknownSVal>();

    state = state->set<CStringLengthMap>(MR, strLength);
  }

  C.addTransition(state);
}

void CStringChecker::checkLiveSymbols(ProgramStateRef State,
                                      SymbolReaper &SR) const {
  // Mark all symbols in our string length map as valid.
  for (const auto &Item : State->get<CStringLengthMap>()) {
    SVal Len = Item.second;
    const auto LenSymbolRange =
        llvm::make_range(Len.symbol_begin(), Len.symbol_end());
    for (SymbolRef Symbol : LenSymbolRange)
      SR.markInUse(Symbol);
  }
}

void CStringChecker::checkDeadSymbols(SymbolReaper &SR,
                                      CheckerContext &C) const {
  ProgramStateRef State = C.getState();
  CStringLengthMapTy Entries = State->get<CStringLengthMap>();
  if (Entries.isEmpty())
    return;

  CStringLengthMapTy::Factory &F = State->get_context<CStringLengthMap>();
  for (CStringLengthMapTy::iterator I = Entries.begin(), E = Entries.end();
       I != E; ++I) {
    SVal Len = I.getData();
    if (SymbolRef Sym = Len.getAsSymbol()) {
      if (SR.isDead(Sym))
        Entries = F.remove(Entries, I.getKey());
    }
  }

  State = State->set<CStringLengthMap>(Entries);
  C.addTransition(State);
}

ProgramStateRef CStringChecker::checkRegionChanges(
    ProgramStateRef state, const InvalidatedSymbols *,
    ArrayRef<const MemRegion *> ExplicitRegions,
    ArrayRef<const MemRegion *> Regions, const LocationContext *,
    const CallEvent *) const {
  CStringLengthMapTy Entries = state->get<CStringLengthMap>();
  if (Entries.isEmpty())
    return state;

  llvm::SmallPtrSet<const MemRegion *, 8> Invalidated;
  llvm::SmallPtrSet<const MemRegion *, 32> SuperRegions;

  // First build sets for the changed regions and their super-regions.
  for (ArrayRef<const MemRegion *>::iterator I = Regions.begin(),
                                             E = Regions.end();
       I != E; ++I) {
    const MemRegion *MR = *I;
    Invalidated.insert(MR);

    SuperRegions.insert(MR);
    while (const SubRegion *SR = dyn_cast<SubRegion>(MR)) {
      MR = SR->getSuperRegion();
      SuperRegions.insert(MR);
    }
  }

  CStringLengthMapTy::Factory &F = state->get_context<CStringLengthMap>();

  // Then loop over the entries in the current state.
  for (CStringLengthMapTy::iterator I = Entries.begin(), E = Entries.end();
       I != E; ++I) {
    const MemRegion *MR = I.getKey();

    // Is this entry for a super-region of a changed region?
    if (SuperRegions.count(MR)) {
      Entries = F.remove(Entries, MR);
      continue;
    }

    // Is this entry for a sub-region of a changed region?
    const MemRegion *Super = MR;
    while (const SubRegion *SR = dyn_cast<SubRegion>(Super)) {
      Super = SR->getSuperRegion();
      if (Invalidated.count(Super)) {
        Entries = F.remove(Entries, MR);
        break;
      }
    }
  }

  return state->set<CStringLengthMap>(Entries);
}

// TODO: Is it useful?
void CStringChecker::printState(raw_ostream &Out, ProgramStateRef State,
                                const char *NL, const char *Sep) const {
  dumpCStringLengths(State, Out, NL, Sep);
}
