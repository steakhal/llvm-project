//===--- StrictAliasingCheck.cpp - clang-tidy------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include "StrictAliasingCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

#include <algorithm>
#include <tuple>
#include <type_traits>

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace bugprone {
enum class StrictAliasingError {
  valid,
  incomplete,
  non_standard_layout,

  builtin_to_different_builtin,
  builtin_to_different_than_first_member,

  enum_to_non_stdbyte,
  enum_to_different_enum,

  empty_record,
  first_member_is_bitfield,
  cannot_alias_with_first_member,

  unrelated_record_types
};

/*
static unsigned current_line; // TODO: remove these
static void invalid(const char *msg) {
  llvm::outs() << current_line << ": " << msg
               << "  ((\033[31;1minvalid\033[0m))\n\n";
}
static void valid(const char *msg) {
  llvm::outs() << current_line << ": " << msg
               << "  ((\033[32;1mvalid\033[0m))\n\n";
}
 */

/*
static void printCastTypes(const Type *SrcTy, const Type *DstTy,
                           const PrintingPolicy &pol) {
  std::string fst, snd;
  if (!SrcTy->isPointerType())
    fst = "&";
  while (SrcTy->isPointerType()) {
    fst += '*';
    SrcTy = SrcTy->getPointeeType().getTypePtr();
  }

  if (auto *BT = dyn_cast<BuiltinType>(SrcTy))
    llvm::outs() << BT->getName(pol);
  else if (auto *RT = dyn_cast<RecordType>(SrcTy))
    llvm::outs() << RT->getDecl()->getName();
  else if (auto *ET = dyn_cast<EnumType>(SrcTy))
    llvm::outs() << ET->getDecl()->getName();
  else
    SrcTy->dump(llvm::outs());

  llvm::outs() << fst << " -> ";

  if (!DstTy->isPointerType())
    snd = "&";
  while (DstTy->isPointerType()) {
    snd += '*';
    DstTy = DstTy->getPointeeType().getTypePtr();
  }

  if (auto *BT = dyn_cast<BuiltinType>(DstTy))
    llvm::outs() << BT->getName(pol);
  else if (auto *RT = dyn_cast<RecordType>(DstTy))
    llvm::outs() << RT->getDecl()->getName();
  else if (auto *ET = dyn_cast<EnumType>(DstTy))
    llvm::outs() << ET->getDecl()->getName();
  else
    DstTy->dump(llvm::outs());
  llvm::outs() << snd << '\n';
}
*/

/*
static bool equalFieldTypeWithoutCV(ASTContext &Ctx, const FieldDecl *Lhs,
                                    const FieldDecl *Rhs) {
  if (Lhs->getType().getTypePtr() != Rhs->getType().getTypePtr())
    return false;
  if (Lhs->isBitField() != Rhs->isBitField())
    return false;
  // if bitfield, should have same width
  if (Lhs->isBitField())
    return Lhs->getBitWidthValue(Ctx) == Rhs->getBitWidthValue(Ctx);
  return true;
}
*/

/*
// [class.mem] 12.2/20
static SmallVector<const Type *, 4>
commonInitialSequence(ASTContext &Ctx, CXXRecordDecl *Lhs,
                      const CXXRecordDecl *Rhs) {
  assert(Lhs && Rhs);

  if (!Lhs->isStandardLayout() || !Rhs->isStandardLayout())
    return {};

  using IT = CXXRecordDecl::field_iterator;
  IT LhsField = Lhs->field_begin(), RhsField = Rhs->field_begin();
  const IT LhsFieldEnd = Lhs->field_end(), RhsFieldEnd = Rhs->field_end();

  SmallVector<const Type *, 4> common{};
  for (; LhsField != LhsFieldEnd && RhsField != RhsFieldEnd &&
         equalFieldTypeWithoutCV(Ctx, *LhsField, *RhsField);
       ++LhsField, ++RhsField) {
    common.push_back(LhsField->getType().getTypePtr());
  }
  return common;
}
 */

/*
// [class.mem] 12.2/21-22
static bool areLayoutCompatibleClasses(ASTContext &Ctx, CXXRecordDecl *Lhs,
                                       const CXXRecordDecl *Rhs) {
  if (Lhs->isUnion() || Rhs->isUnion())
    return false; // TODO: handle unions
  const auto common = commonInitialSequence(Ctx, Lhs, Rhs);
  return common.size() == static_cast<std::size_t>(std::distance(
                              Lhs->field_begin(), Lhs->field_end()));
}
*/

// if the type is [unsigned|signed] char, std::byte, void
// than true
// otherwise false
static bool isAnyAlias(const Type *Ty) {
  if (Ty->isStdByteType())
    return true;
  if (Ty->isVoidType())
    return true;
  if (Ty->isCharType())
    return true;
  return false;
}

static StrictAliasingError canAlias(const BuiltinType *SrcTy,
                                    const BuiltinType *DstTy) {
  if (SrcTy == DstTy)
    return StrictAliasingError::valid;

  if (isAnyAlias(SrcTy) || isAnyAlias(DstTy))
    return StrictAliasingError::valid;

  // handle unsigned / signed compatibility for each type
  // ignore signess in case of wchar_t
  if (SrcTy->isWideCharType() && DstTy->isWideCharType())
    return StrictAliasingError::valid;

  // handle other signed unsigned builtin types
  using BTKind = BuiltinType::Kind;
  const BTKind SrcKind = SrcTy->getKind();
  const BTKind DstKind = DstTy->getKind();

  auto both_are_of = [SrcKind, DstKind](BTKind sign, BTKind unsign) {
    auto one_of = [](BTKind x, BTKind sign, BTKind unsign) {
      return x == sign || x == unsign;
    };
    return one_of(SrcKind, sign, unsign) && one_of(DstKind, sign, unsign);
  };

  if (both_are_of(BTKind::Short, BTKind::UShort))
    return StrictAliasingError::valid;

  if (both_are_of(BTKind::Int, BTKind::UInt))
    return StrictAliasingError::valid;

  if (both_are_of(BTKind::Long, BTKind::ULong))
    return StrictAliasingError::valid;

  if (both_are_of(BTKind::LongLong, BTKind::ULongLong))
    return StrictAliasingError::valid;

  if (both_are_of(BTKind::Int128, BTKind::UInt128))
    return StrictAliasingError::valid;

  return StrictAliasingError::builtin_to_different_builtin;
}

static bool areSameTypeOrAliasableBuiltins(const Type *SrcTy,
                                           const Type *DstTy) {
  if (SrcTy == DstTy)
    return true;

  // char-like? -> char-like?
  if (isAnyAlias(SrcTy) || isAnyAlias(DstTy))
    return true;

  if (SrcTy->isBuiltinType() && DstTy->isBuiltinType()) {
    StrictAliasingError CanAlias =
        canAlias(SrcTy->getAs<BuiltinType>(), DstTy->getAs<BuiltinType>());
    return CanAlias == StrictAliasingError::valid;
  }
  // otherwise it cannot alias
  return false;
}

/*
// [conv.qual] 7.5 (similarity)
//   (1) A cv-decomposition of a type T is a sequence of cv_i and P_i such that
//       T is:  "cv0 P0 cv1 P1 ... cvN-1 PN-1 cvN U" for N > 0,
//       where each cv_i is a set of cv-qualifiers (6.9.3), and each P_i is
//       "pointer to" (11.3.1), "pointer to member of class C_i of type"
//       (11.3.3), "array of N_i", or "array of unknown bound of" (11.3.4). If
//       P_i designates an array, the cv-qualifiers cv_i+1 on the element type
//       are also taken as the cv-qualifiers cv_i of the array.
//       [ Example: The type denoted by the type-id const int ** has two
//       cv-decompositions, taking U as "int **" and as "pointer to const int".
//       — end example ] The N-tuple of cv-qualifiers after the first one in the
//       longest cv-decomposition of T, that is, cv1, cv2, ..., cvN, is called
//       the cv-qualification signature of T.
//   (2) Two types T1 and T2 are similar if they have cv-decompositions with the
//       same N such that corresponding P_i components are the same and the
//       types denoted by U are the same.
//   (3) A prvalue expression of type T1 can be converted to type T2 if the
//       following conditions are satisfied, where cv_i^j denotes the
//       cv-qualifiers in the cv-qualification signature of T_j:
//       — T1 and T2 are similar.
//       — For every i > 0, if const is in cv_i^1 then
//         const is in cv_k^2 for 0 < k < i.
//       — If the cv_i^1 and cv_i^2 are different, then const is in every cv_k^2
//         for 0 < k < i.
static bool areSimilar(ASTContext &Ctx, const Type *Lhs, const Type *Rhs) {
  // Informally, two types are similar if, ignoring top-level cv-qualification:
  // -   they are the same type; or
  if (Lhs == Rhs)
    return true;

  // -   they are both pointers, and the pointed-to types are similar; or
  if (Lhs->isPointerType() && Rhs->isPointerType() &&
      areSimilar(Ctx, Lhs->getPointeeType().getTypePtr(),
                 Rhs->getPointeeType().getTypePtr()))
    return true;

  // -   they are both pointers to member of the same class, and the types of
  //     the pointed-to members are similar; or
  if (Lhs->isMemberPointerType() && Rhs->isMemberPointerType()) {
    const MemberPointerType *LhsMemPtr = Lhs->getAs<MemberPointerType>();
    const MemberPointerType *RhsMemPtr = Rhs->getAs<MemberPointerType>();
    if (LhsMemPtr->getClass() == RhsMemPtr->getClass() &&
        areSimilar(Ctx, LhsMemPtr->getPointeeType().getTypePtr(),
                   RhsMemPtr->getPointeeType().getTypePtr()))
      return true;
  }

  // -   they are both arrays of the same size or both arrays of unknown bound,
  //     and the array element types are similar.
  if (Lhs->isIncompleteArrayType() && Rhs->isIncompleteArrayType()) {
    return areSimilar(Ctx, Lhs->getArrayElementTypeNoTypeQual(),
                      Rhs->getArrayElementTypeNoTypeQual());
  }

  if (Lhs->isConstantArrayType() && Rhs->isConstantArrayType()) {
    const ConstantArrayType *LhsArr =
        Ctx.getAsConstantArrayType(QualType(Lhs, 0));
    const ConstantArrayType *RhsArr =
        Ctx.getAsConstantArrayType(QualType(Rhs, 0));
    if (LhsArr->getSize() != RhsArr->getSize())
      return false;
    return areSimilar(Ctx, Lhs->getArrayElementTypeNoTypeQual(),
                      Rhs->getArrayElementTypeNoTypeQual());
  }

  return false;
}
 */

static StrictAliasingError isOneLevelDeepPrefix(const RecordType *OuterTy,
                                                const Type *InnerTy);

/////////////////////////////////////// declarations start

// checks whether and object with SrcTy type could be reinterpreted as DstTy
// type without violating the strict aliasing rules.
static StrictAliasingError arePointerInterchangeable(const Type *SrcTy,
                                                     const Type *DstTy);
template <class T>
static StrictAliasingError arePointerInterchangeable(const T *SrcTy,
                                                     const Type *DstTy);

static StrictAliasingError arePointerInterchangeable(const BuiltinType *SrcTy,
                                                     const BuiltinType *DstTy);
static StrictAliasingError arePointerInterchangeable(const RecordType *SrcTy,
                                                     const BuiltinType *DstTy);
static StrictAliasingError arePointerInterchangeable(const EnumType *SrcTy,
                                                     const BuiltinType *DstTy);

/// cast to RecordType
static StrictAliasingError arePointerInterchangeable(const BuiltinType *SrcTy,
                                                     const RecordType *DstTy);
static StrictAliasingError arePointerInterchangeable(const RecordType *SrcTy,
                                                     const RecordType *DstTy);
static StrictAliasingError arePointerInterchangeable(const EnumType *SrcTy,
                                                     const RecordType *DstTy);

/// cast to EnumType
static StrictAliasingError arePointerInterchangeable(const BuiltinType *SrcTy,
                                                     const EnumType *DstTy);
static StrictAliasingError arePointerInterchangeable(const RecordType *SrcTy,
                                                     const EnumType *DstTy);
static StrictAliasingError arePointerInterchangeable(const EnumType *SrcTy,
                                                     const EnumType *DstTy);
/////////////////////////////////////// declarations end

// [class.mem] 12.2/24
//   If a standard-layout class object has any non-static data members, its
//   address is the same as the address of its first non-static data member.
//   Otherwise, its address is the same as the address of its first base class
//   subobject (if any). [ Note: There might therefore be unnamed padding within
//   a standard-layout struct object, but not at its beginning, as necessary to
//   achieve appropriate alignment. — end note ] [ Note: The object and its
//   first subobject are pointer-interconvertible (6.9.2, 8.2.9). — end note ]
//
// [class.union] 12.3/2
//   The size of a union is sufficient to contain the largest of its non-static
//   data members. Each non-static data member is allocated as if it were the
//   sole member of a struct. [ Note: A standard-layout union object and its
//   non-static data members are pointer-interconvertible (6.9.2, 8.2.9). As a
//   consequence, all non-static data members of such a union object have the
//   same address. — end note ]
static StrictAliasingError isOneLevelDeepPrefix(const RecordType *OuterTy,
                                                const Type *InnerTy) {
  const RecordDecl *RecDecl = OuterTy->getAsRecordDecl();
  const auto *CXXRecDecl = OuterTy->getAsCXXRecordDecl();

  if (OuterTy == InnerTy)
    return StrictAliasingError::valid;

  if (OuterTy->isIncompleteType(nullptr))
    return StrictAliasingError::incomplete;

  const auto UnionHasAnyValidMember = [RecDecl, InnerTy]() {
    return std::any_of(RecDecl->field_begin(), RecDecl->field_end(),
                       [InnerTy](const FieldDecl *FieldDecl) {
                         return areSameTypeOrAliasableBuiltins(
                             FieldDecl->getType().getTypePtr(), InnerTy);
                       });
  };

  // if union (and has standard layout), each non-static data member should
  // considered as the first member
  if (OuterTy->isUnionType() && UnionHasAnyValidMember()) {
    if (CXXRecDecl && !CXXRecDecl->isStandardLayout())
      return StrictAliasingError::non_standard_layout;

    // union has a compatible non-static data member
    return StrictAliasingError::valid;
  }

  // handle C++ struct/class standard layout inheritance
  if (CXXRecDecl) {
    // if c++ record decl, it should have standard layout to be able to cast
    // to the first non-static data member but
    if (!CXXRecDecl->isStandardLayout())
      return StrictAliasingError::non_standard_layout;

    // if itself has no data members but neither empty
    // so it must have at least one non-empty base
    if (CXXRecDecl->field_empty() && !CXXRecDecl->isEmpty()) {
      const auto isNonEmptyBase = [](const CXXBaseSpecifier &base) {
        assert(base.getType()->isRecordType());
        return !base.getType()->getAsCXXRecordDecl()->isEmpty();
      };

      // find the first non-empty base class
      const auto BaseIt = std::find_if(CXXRecDecl->bases_begin(),
                                       CXXRecDecl->bases_end(), isNonEmptyBase);
      assert(BaseIt != CXXRecDecl->bases_end() && "non-empty base must exist");

      const Type *BaseTy = BaseIt->getType().getTypePtr();
      return arePointerInterchangeable(BaseTy, InnerTy); // recurse
    }
  }

  if (RecDecl->field_empty())
    return StrictAliasingError::empty_record;

  // if the C/C++ struct has non-static data members, pick the first one
  const FieldDecl *FirstField = *RecDecl->field_begin();
  if (FirstField->isBitField())
    return StrictAliasingError::first_member_is_bitfield;

  // don't recurse, just do a one level deep check
  if (areSameTypeOrAliasableBuiltins(FirstField->getType().getTypePtr(),
                                     InnerTy))
    return StrictAliasingError::valid;

  return StrictAliasingError::cannot_alias_with_first_member;
}

/// cast to BuiltinType
// only signess difference for each BuiltinType
static StrictAliasingError arePointerInterchangeable(const BuiltinType *SrcTy,
                                                     const BuiltinType *DstTy) {
  return canAlias(SrcTy, DstTy);
}

static StrictAliasingError arePointerInterchangeable(const RecordType *SrcTy,
                                                     const BuiltinType *DstTy) {
  return isOneLevelDeepPrefix(SrcTy, DstTy);
  /*if (isOneLevelDeepPrefix(SrcTy, DstTy)) {
  valid("record to prefix builtin member");
  return true;
}
invalid("record to NON-prefix builtin member");
return false;*/
}

static StrictAliasingError arePointerInterchangeable(const EnumType *SrcTy,
                                                     const BuiltinType *DstTy) {
  static_cast<void>(DstTy);

  // if the type is std::byte, magically it can alias just like char does
  if (SrcTy->isStdByteType())
    return StrictAliasingError::valid;

  return StrictAliasingError::enum_to_non_stdbyte;
}

/// cast to RecordType
static StrictAliasingError arePointerInterchangeable(const BuiltinType *SrcTy,
                                                     const RecordType *DstTy) {
  if (DstTy->isIncompleteType(nullptr))
    return StrictAliasingError::incomplete;

  if (const CXXRecordDecl *DstCXXRecordDecl = DstTy->getAsCXXRecordDecl()) {
    // invalid, if not standard layout
    if (!DstCXXRecordDecl->isStandardLayout())
      return StrictAliasingError::non_standard_layout;
  }

  // if the builtin type is the first of the record type (DstTy)
  // RELAX: what if the pointer points NOT to the first member of the record,
  //  maybe consider all the members transitively?
  return isOneLevelDeepPrefix(DstTy, SrcTy);
  /*if (isOneLevelDeepPrefix(DstTy, SrcTy)) {
    valid("builtin to prefix of record");
    return true;
  }
  invalid("builtin to NON-prefix of record");
  return false;*/
}
static StrictAliasingError arePointerInterchangeable(const RecordType *SrcTy,
                                                     const RecordType *DstTy) {
  constexpr auto Valid = StrictAliasingError::valid;

  if (SrcTy == DstTy)
    return Valid;

  if (isOneLevelDeepPrefix(SrcTy, DstTy) == Valid)
    return Valid;

  if (isOneLevelDeepPrefix(DstTy, SrcTy) == Valid)
    return Valid;

  return StrictAliasingError::unrelated_record_types;
}
static StrictAliasingError arePointerInterchangeable(const EnumType *SrcTy,
                                                     const RecordType *DstTy) {
  // std::byte can alias just like char does
  if (SrcTy->isStdByteType())
    return StrictAliasingError::valid;

  if (const CXXRecordDecl *DstCXXRecordDecl = DstTy->getAsCXXRecordDecl()) {
    // invalid, if not standard layout
    if (!DstCXXRecordDecl->isStandardLayout())
      return StrictAliasingError::non_standard_layout;
  }

  // if the enum type is the first of the record type (DstTy)
  // RELAX: what if the pointer points NOT to the first member of the record,
  //  maybe consider all the members transitively?
  return isOneLevelDeepPrefix(DstTy, SrcTy);
}

/// cast to EnumType
static StrictAliasingError arePointerInterchangeable(const BuiltinType *SrcTy,
                                                     const EnumType *DstTy) {
  // underlying type type and enum -> NOT alias
  //  enum E1 : unsigned {Val1 = 1};           // f(E1*, unsigned int*):
  //  int f2(E1 *p, unsigned *q) {             //   movl $1, (%rdi)
  //    *p = Val1;                             //   movl $2, (%rsi)
  //    *q = 2;                                //   movl $1, %eax
  //    return static_cast<int>(*p);           //   retq
  //  }
  // but std::byte can alias everything
  static_cast<void>(SrcTy);

  if (DstTy->isStdByteType())
    return StrictAliasingError::valid;

  return StrictAliasingError::enum_to_different_enum;
}
static StrictAliasingError arePointerInterchangeable(const RecordType *SrcTy,
                                                     const EnumType *DstTy) {
  if (DstTy->isStdByteType())
    return StrictAliasingError::valid;

  // if the record has the given enum as it's first non-static data member
  // (and everything standard layout) than OK, otherwise BAD
  return isOneLevelDeepPrefix(SrcTy, DstTy);
  /*if (isOneLevelDeepPrefix(SrcTy, DstTy)) {
    valid("record to prefix enum member");
    return true;
  }
  invalid("record to NON-prefix enum member");
  return false;*/
}

static StrictAliasingError arePointerInterchangeable(const EnumType *SrcTy,
                                                     const EnumType *DstTy) {
  // different enums -> NOT alias
  //  enum E1 {Val1 = 1}; enum E2 {Val2 = 2}; // f(E1*, E2*):
  //  int f(E1 *p, E2 *q) {                   //   movl $1, (%rdi)
  //    *p = Val1;                            //   movl $2, (%rsi)
  //    *q = Val2;                            //   movl $1, %eax
  //    return static_cast<int>(*p);          //   retq
  //  }
  // different enum with common underlying type -> NOT alias
  //  same example, same assembly but:
  //  enum E1 : unsigned {Val1 = 1}; enum E2 : unsigned {Val2 = 2};
  // same with enum classes

  if (SrcTy == DstTy)
    return StrictAliasingError::valid;

  // std::byte? to std::byte?
  if (SrcTy->isStdByteType() || DstTy->isStdByteType())
    return StrictAliasingError::valid;

  return StrictAliasingError ::enum_to_different_enum;
}

/// dispatch to the appropriate overload
template <class T>
static StrictAliasingError arePointerInterchangeable(const T *SrcTy,
                                                     const Type *DstTy) {
  static_assert(std::is_same<T, BuiltinType>::value ||
                    std::is_same<T, RecordType>::value ||
                    std::is_same<T, EnumType>::value,
                "");
  if (DstTy->isArrayType())
    return arePointerInterchangeable(SrcTy,
                                     DstTy->getArrayElementTypeNoTypeQual());
  if (const auto *BT = DstTy->getAs<BuiltinType>())
    return arePointerInterchangeable(SrcTy, BT);
  if (const auto *RT = DstTy->getAs<RecordType>())
    return arePointerInterchangeable(SrcTy, RT);
  if (const auto *ET = DstTy->getAs<EnumType>())
    return arePointerInterchangeable(SrcTy, ET);

  llvm::outs() << "destination type is unknown\n";
  SrcTy->dump(llvm::outs());
  llvm_unreachable("Exhaustive list?");
}

static StrictAliasingError arePointerInterchangeable(const Type *SrcTy,
                                                     const Type *DstTy) {
  assert(SrcTy);
  assert(DstTy);

  if (SrcTy->isArrayType())
    return arePointerInterchangeable(SrcTy->getArrayElementTypeNoTypeQual(),
                                     DstTy);
  if (const auto *BT = SrcTy->getAs<BuiltinType>())
    return arePointerInterchangeable(BT, DstTy);
  if (const auto *RT = SrcTy->getAs<RecordType>())
    return arePointerInterchangeable(RT, DstTy);
  if (const auto *ET = SrcTy->getAs<EnumType>())
    return arePointerInterchangeable(ET, DstTy);

  llvm::outs() << "source type is unknown\n";
  SrcTy->dump(llvm::outs());
  llvm_unreachable("exhaustive list");
}

static std::pair<const Type *, const Type *>
findPointeeTypeTransitively(const Type *SrcTy, const Type *DstTy) {
  // find the pointee type
  while (SrcTy && DstTy && SrcTy->isPointerType() && DstTy->isPointerType()) {
    SrcTy = SrcTy->getPointeeType().getTypePtr();
    DstTy = DstTy->getPointeeType().getTypePtr();
  }
  return {SrcTy, DstTy};
}

StrictAliasingCheck::StrictAliasingCheck(StringRef Name,
                                         ClangTidyContext *Context)
    : ClangTidyCheck(Name, Context),
      WarnOnlyIfDereferenced(
          Options.get("WarnOnlyIfDereferenced", "true").find("false") ==
          std::string::npos) {}

static void registerBitcastExprs(ClangTidyCheck *Checker, MatchFinder *Finder) {
  const auto refcast =
      explicitCastExpr(hasCastKind(CK_LValueBitCast)).bind("bitcast");
  const auto ptrcast =
      explicitCastExpr(hasCastKind(CK_BitCast)).bind("bitcast");
  Finder->addMatcher(refcast, Checker);
  Finder->addMatcher(ptrcast, Checker);
}

static void registerBitcastDereferenceExprs(ClangTidyCheck *Checker,
                                            MatchFinder *Finder) {
  const auto refcast =
      explicitCastExpr(hasCastKind(CK_LValueBitCast)).bind("bitcast");
  const auto ptrcast =
      explicitCastExpr(hasCastKind(CK_BitCast)).bind("bitcast");

  const auto iptrcast = ignoringParenImpCasts(ptrcast);
  const auto member = memberExpr(hasObjectExpression(iptrcast), isArrow());
  const auto array = arraySubscriptExpr(hasBase(iptrcast));
  const auto deref =
      unaryOperator(hasOperatorName("*"),
                    hasUnaryOperand(allOf(hasType(isAnyPointer()), iptrcast)));

  Finder->addMatcher(refcast, Checker);
  Finder->addMatcher(member, Checker);
  Finder->addMatcher(array, Checker);
  Finder->addMatcher(deref, Checker);
}

void StrictAliasingCheck::registerMatchers(MatchFinder *Finder) {
  if (WarnOnlyIfDereferenced)
    registerBitcastDereferenceExprs(this, Finder);
  else
    registerBitcastExprs(this, Finder);
}

void StrictAliasingCheck::storeOptions(ClangTidyOptions::OptionMap &Opts) {
  Options.store(Opts, "WarnOnlyIfDereferenced", WarnOnlyIfDereferenced);
}

void StrictAliasingCheck::check(const MatchFinder::MatchResult &Result) {
  // const ASTContext *Ctx = Result.Context;
  const auto *ECE = Result.Nodes.getNodeAs<ExplicitCastExpr>("bitcast");
  //const auto beg = Result.SourceManager->getPresumedLoc(ECE->getBeginLoc());
  //const auto line = beg.getLine();
  // current_line = line; // TODO: remove this

  // types
  const Type *DstTy = ECE->getType().getTypePtr();
  const Type *SrcTy = ECE->getSubExpr()->getType().getTypePtr();

  // llvm::outs() << beg.getLine() << ": seen cast: ";
  // printCastTypes(SrcTy, DstTy, pol);

  if (SrcTy->isPointerType() != DstTy->isPointerType()) {
    //llvm::outs() << line
    //             << ": not both src and dst are pointers/refs  ((skip))\n\n";
    return; // TODO what to do in this case?
  }

  std::tie(SrcTy, DstTy) = findPointeeTypeTransitively(SrcTy, DstTy);

  if (const auto *ET = dyn_cast<ElaboratedType>(SrcTy))
    SrcTy = ET->desugar().getTypePtr();
  if (const auto *ET = dyn_cast<ElaboratedType>(DstTy))
    DstTy = ET->desugar().getTypePtr();

  // llvm::outs() << line << ": after findPointeeTypeTransitively: ";
  // printCastTypes(SrcTy, DstTy, pol);

  if (SrcTy->isPointerType() || DstTy->isPointerType()) {
    // invalid("not same 'deep' pointer chain");
    return; // TODO what to do in this case?
  }

  if (arePointerInterchangeable(SrcTy, DstTy) != StrictAliasingError::valid)
    diag(ECE->getBeginLoc(), "checker result");
}

} // namespace bugprone
} // namespace tidy
} // namespace clang
