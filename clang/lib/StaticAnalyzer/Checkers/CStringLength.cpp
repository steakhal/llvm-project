//=== TODO. ------*- C++ -*-//
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

#include "CStringLength.h"
#include "clang/StaticAnalyzer/Checkers/BuiltinCheckerRegistration.h"
#include "clang/StaticAnalyzer/Core/BugReporter/BugReporter.h"
#include "clang/StaticAnalyzer/Core/CheckerManager.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramStateTrait.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace ento;

// TODO describe map.
REGISTER_MAP_WITH_PROGRAMSTATE(CStringLengthMap, const MemRegion *, SVal)

namespace clang {
namespace ento {
namespace cstring {

/// TODO describe behavior.
ProgramStateRef setCStringLength(ProgramStateRef State, const MemRegion *MR,
                                 SVal StrLength) {
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
    return State->remove<CStringLengthMap>(MR);

  return State->set<CStringLengthMap>(MR, StrLength);
}

/// TODO.
ProgramStateRef removeCStringLength(ProgramStateRef State,
                                    const MemRegion *MR) {
  return State->remove<CStringLengthMap>(MR);
}

// TODO: understand this and migrate if useful as-is. Modify if necessary.
SVal getCStringLengthForRegion(CheckerContext &C, ProgramStateRef &state,
                               const Expr *Ex, const MemRegion *MR,
                               bool hypothetical) {
  if (!hypothetical) {
    // If there's a recorded length, go ahead and return it.
    const SVal *Recorded = state->get<CStringLengthMap>(MR);
    if (Recorded)
      return *Recorded;
  }

  // Otherwise, get a new symbol and update the state.
  SValBuilder &svalBuilder = C.getSValBuilder();
  QualType sizeTy = svalBuilder.getContext().getSizeType();
  SVal strLength =
      svalBuilder.getMetadataSymbolVal(CStringChecker::getTag(), MR, Ex, sizeTy,
                                       C.getLocationContext(), C.blockCount());

  if (!hypothetical) {
    if (Optional<NonLoc> strLn = strLength.getAs<NonLoc>()) {
      // In case of unbounded calls strlen etc bound the range to SIZE_MAX/4
      BasicValueFactory &BVF = svalBuilder.getBasicValueFactory();
      const llvm::APSInt &maxValInt = BVF.getMaxValue(sizeTy);
      llvm::APSInt fourInt = APSIntType(maxValInt).getValue(4);
      const llvm::APSInt *maxLengthInt =
          BVF.evalAPSInt(BO_Div, maxValInt, fourInt);
      NonLoc maxLength = svalBuilder.makeIntVal(*maxLengthInt);
      SVal evalLength =
          svalBuilder.evalBinOpNN(state, BO_LE, *strLn, maxLength, sizeTy);
      state = state->assume(evalLength.castAs<DefinedOrUnknownSVal>(), true);
    }
    state = state->set<CStringLengthMap>(MR, strLength);
  }

  return strLength;
}

// TODO: factor reporting out from this.
SVal getCStringLength(CheckerContext &C, ProgramStateRef &state, const Expr *Ex,
                      SVal Buf, bool hypothetical /*=false*/) {
  const MemRegion *MR = Buf.getAsRegion();
  if (!MR) {
    // If we can't get a region, see if it's something we /know/ isn't a
    // C string. In the context of locations, the only time we can issue such
    // a warning is for labels.
    if (Optional<loc::GotoLabel> Label = Buf.getAs<loc::GotoLabel>()) {
      if (Filter.CheckCStringNotNullTerm) {
        SmallString<120> buf;
        llvm::raw_svector_ostream os(buf);
        assert(CurrentFunctionDescription);
        os << "Argument to " << CurrentFunctionDescription
           << " is the address of the label '" << Label->getLabel()->getName()
           << "', which is not a null-terminated string";

        emitNotCStringBug(C, state, Ex, os.str());
      }
      return UndefinedVal();
    }

    // If it's not a region and not a label, give up.
    return UnknownVal();
  }

  // If we have a region, strip casts from it and see if we can figure out
  // its length. For anything we can't figure out, just return UnknownVal.
  MR = MR->StripCasts();

  switch (MR->getKind()) {
  case MemRegion::StringRegionKind: {
    // Modifying the contents of string regions is undefined [C99 6.4.5p6],
    // so we can assume that the byte length is the correct C string length.
    SValBuilder &svalBuilder = C.getSValBuilder();
    QualType sizeTy = svalBuilder.getContext().getSizeType();
    const StringLiteral *strLit = cast<StringRegion>(MR)->getStringLiteral();
    return svalBuilder.makeIntVal(strLit->getByteLength(), sizeTy);
  }
  case MemRegion::SymbolicRegionKind:
  case MemRegion::AllocaRegionKind:
  case MemRegion::NonParamVarRegionKind:
  case MemRegion::ParamVarRegionKind:
  case MemRegion::FieldRegionKind:
  case MemRegion::ObjCIvarRegionKind:
    return getCStringLengthForRegion(C, state, Ex, MR, hypothetical);
  case MemRegion::CompoundLiteralRegionKind:
    // FIXME: Can we track this? Is it necessary?
    return UnknownVal();
  case MemRegion::ElementRegionKind:
    // FIXME: How can we handle this? It's not good enough to subtract the
    // offset from the base string length; consider "123\x00567" and &a[5].
    return UnknownVal();
  default:
    // Other regions (mostly non-data) can't have a reliable C string length.
    // In this case, an error is emitted and UndefinedVal is returned.
    // The caller should always be prepared to handle this case.
    if (Filter.CheckCStringNotNullTerm) {
      SmallString<120> buf;
      llvm::raw_svector_ostream os(buf);

      assert(CurrentFunctionDescription);
      os << "Argument to " << CurrentFunctionDescription << " is ";

      if (SummarizeRegion(os, C.getASTContext(), MR))
        os << ", which is not a null-terminated string";
      else
        os << "not a null-terminated string";

      emitNotCStringBug(C, state, Ex, os.str());
    }
    return UndefinedVal();
  }
}

// TODO.
void dumpCStringLengths(ProgramStateRef State, raw_ostream &Out, const char *NL,
                        const char *Sep) {
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

/// TODO: It should not be here!
const StringLiteral *getCStringLiteral(SVal val) {
  // Get the memory region pointed to by the val.
  const MemRegion *bufRegion = val.getAsRegion();
  if (!bufRegion)
    return nullptr;

  // Strip casts off the memory region.
  bufRegion = bufRegion->StripCasts();

  // Cast the memory region to a string region.
  const StringRegion *strRegion = dyn_cast<StringRegion>(bufRegion);
  if (!strRegion)
    return nullptr;

  // Return the actual string in the string region.
  return strRegion->getStringLiteral();
}

} // namespace cstring
} // namespace ento
} // namespace clang

/// #####################  IMPL  ######################################

namespace {
using namespace cstring;

class CStringModelingV2
    : public Checker<check::PreStmt<DeclStmt>, check::LiveSymbols,
                     check::DeadSymbols, check::RegionChanges> {
public:
  void checkPreStmt(const DeclStmt *DS, CheckerContext &C) const {
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
      DefinedOrUnknownSVal strLength = getCStringLength(C, state, Init, StrVal)
                                           .castAs<DefinedOrUnknownSVal>();

      state = state->set<CStringLengthMap>(MR, strLength);
    }

    C.addTransition(state);
  }

  void checkLiveSymbols(ProgramStateRef state, SymbolReaper &SR) const {
    // Mark all symbols in our string length map as valid.
    CStringLengthMapTy Entries = state->get<CStringLengthMap>();

    for (CStringLengthMapTy::iterator I = Entries.begin(), E = Entries.end();
         I != E; ++I) {
      SVal Len = I.getData();

      for (SymExpr::symbol_iterator si = Len.symbol_begin(),
                                    se = Len.symbol_end();
           si != se; ++si)
        SR.markInUse(*si);
    }
  }

  void checkDeadSymbols(SymbolReaper &SR, CheckerContext &C) const {
    ProgramStateRef state = C.getState();
    CStringLengthMapTy Entries = state->get<CStringLengthMap>();
    if (Entries.isEmpty())
      return;

    CStringLengthMapTy::Factory &F = state->get_context<CStringLengthMap>();
    for (CStringLengthMapTy::iterator I = Entries.begin(), E = Entries.end();
         I != E; ++I) {
      SVal Len = I.getData();
      if (SymbolRef Sym = Len.getAsSymbol()) {
        if (SR.isDead(Sym))
          Entries = F.remove(Entries, I.getKey());
      }
    }

    state = state->set<CStringLengthMap>(Entries);
    C.addTransition(state);
  }

  ProgramStateRef
  checkRegionChanges(ProgramStateRef state, const InvalidatedSymbols *,
                     ArrayRef<const MemRegion *> ExplicitRegions,
                     ArrayRef<const MemRegion *> Regions,
                     const LocationContext *LCtx, const CallEvent *Call) const {
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

  void printState(raw_ostream &Out, ProgramStateRef State, const char *NL,
                  const char *Sep) const {
    dumpCStringLengths(State, Out, NL, Sep);
  }
}; // class CStringModelingV2
} // namespace

void ento::registerCStringModeling(CheckerManager &Mgr) {
  Mgr.registerChecker<CStringModelingV2>();
}

bool ento::shouldRegisterCStringModeling(const CheckerManager &) {
  return true;
}
