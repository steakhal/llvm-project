//===- DynamicExtent.cpp - Dynamic extent related APIs ----------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file defines APIs that track and query dynamic extent information.
//
//===----------------------------------------------------------------------===//

#include "clang/StaticAnalyzer/Core/PathSensitive/DynamicExtent.h"
#include "clang/AST/Expr.h"
#include "clang/Basic/LLVM.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/MemRegion.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramState.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/SValBuilder.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/SVals.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/SymbolManager.h"
#include <optional>

REGISTER_MAP_WITH_PROGRAMSTATE(DynamicExtentMap, const clang::ento::MemRegion *,
                               clang::ento::DefinedOrUnknownSVal)

namespace clang {
namespace ento {

static bool isLastField(const FieldDecl *FD) {
  RecordDecl::field_iterator FI(
      DeclContext::decl_iterator(const_cast<FieldDecl *>(FD)));
  return ++FI == FD->getParent()->field_end();
}

/// Check if the region refers to a field that is the last one
/// transitively across parent records.
static bool isLastField(const FieldRegion *FR) {
  for (; FR; FR = dyn_cast<FieldRegion>(FR->getSuperRegion())) {
    if (!isLastField(FR->getDecl()))
      return false;
  }
  return true;
}

/// Calculate the extent of the FAM based on the extent of the parent region.
static std::optional<DefinedOrUnknownSVal>
getDynamicExtentOfFlexibleArrayMembers(ProgramStateRef State,
                                       const MemRegion *MR, SValBuilder &SVB) {
  const auto *FR = dyn_cast<FieldRegion>(MR);

  if (!FR || !isa<ArrayType>(FR->getValueType()) || !isLastField(FR))
    return std::nullopt;

  RegionOffset RelPos = FR->getAsOffset();
  if (!RelPos.isValid() || RelPos.hasSymbolicOffset())
    return std::nullopt;

  // Check if we have an associated extent with this base region.
  const auto *BaseExtent = State->get<DynamicExtentMap>(RelPos.getRegion());
  if (!BaseExtent)
    return std::nullopt;

  QualType IndexTy = SVB.getArrayIndexType();
  int64_t OffsetInBits = RelPos.getOffset();
  uint64_t CharBits = SVB.getContext().getCharWidth();
  assert(OffsetInBits % CharBits == 0);
  DefinedSVal OffsetInBytes = SVB.makeIntVal(OffsetInBits / CharBits, IndexTy);
  SVal FAMExtent =
      SVB.evalBinOp(State, BO_Sub, *BaseExtent, OffsetInBytes, IndexTy);
  return FAMExtent.castAs<DefinedOrUnknownSVal>();
}

DefinedOrUnknownSVal getDynamicExtent(ProgramStateRef State,
                                      const MemRegion *MR, SValBuilder &SVB) {
  MR = MR->StripCasts();

  if (const DefinedOrUnknownSVal *Size = State->get<DynamicExtentMap>(MR)) {
    return *Size;
  }

  if (auto FAMSize = getDynamicExtentOfFlexibleArrayMembers(State, MR, SVB)) {
    return *FAMSize;
  }

  return MR->getMemRegionManager().getStaticSize(MR, SVB);
}

DefinedOrUnknownSVal getElementExtent(QualType Ty, SValBuilder &SVB) {
  return SVB.makeIntVal(SVB.getContext().getTypeSizeInChars(Ty).getQuantity(),
                        SVB.getArrayIndexType());
}

DefinedOrUnknownSVal getDynamicElementCount(ProgramStateRef State,
                                            const MemRegion *MR,
                                            SValBuilder &SVB,
                                            QualType ElementTy) {
  assert(MR != nullptr && "Not-null region expected");
  MR = MR->StripCasts();

  DefinedOrUnknownSVal Size = getDynamicExtent(State, MR, SVB);
  SVal ElementSize = getElementExtent(ElementTy, SVB);

  SVal ElementCount =
      SVB.evalBinOp(State, BO_Div, Size, ElementSize, SVB.getArrayIndexType());

  return ElementCount.castAs<DefinedOrUnknownSVal>();
}

SVal getDynamicExtentWithOffset(ProgramStateRef State, SVal BufV) {
  SValBuilder &SvalBuilder = State->getStateManager().getSValBuilder();
  const MemRegion *MRegion = BufV.getAsRegion();
  if (!MRegion)
    return UnknownVal();
  RegionOffset Offset = MRegion->getAsOffset();
  if (Offset.hasSymbolicOffset())
    return UnknownVal();
  const MemRegion *BaseRegion = MRegion->getBaseRegion();
  if (!BaseRegion)
    return UnknownVal();

  NonLoc OffsetInBytes = SvalBuilder.makeArrayIndex(
      Offset.getOffset() /
      MRegion->getMemRegionManager().getContext().getCharWidth());
  DefinedOrUnknownSVal ExtentInBytes =
      getDynamicExtent(State, BaseRegion, SvalBuilder);

  return SvalBuilder.evalBinOp(State, BinaryOperator::Opcode::BO_Sub,
                               ExtentInBytes, OffsetInBytes,
                               SvalBuilder.getArrayIndexType());
}

ProgramStateRef setDynamicExtent(ProgramStateRef State, const MemRegion *MR,
                                 DefinedOrUnknownSVal Size, SValBuilder &SVB) {
  MR = MR->StripCasts();

  if (Size.isUnknown())
    return State;

  return State->set<DynamicExtentMap>(MR->StripCasts(), Size);
}

} // namespace ento
} // namespace clang
