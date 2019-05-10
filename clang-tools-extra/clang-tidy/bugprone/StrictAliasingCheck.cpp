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

/// \brief represents the internal information of the checker
/// it will be used to distinguish the reasons why two pointers can not alias
enum class StrictAliasingError {
  valid,
  incomplete,
  non_standard_layout,

  builtin_to_different_builtin,
  enum_to_builtin,
  enum_to_different_enum,

  empty_record,
  first_member_is_bitfield,
  cannot_alias_with_first_member,

  unrelated_record_types,

  unrelated_function_types
};

/// \brief These functions are handling all the possible meaningful combinations
/// of casting one type to the other
///
/// checks whether and object with SrcTy type could be reinterpreted as DstTy
/// type without violating the strict aliasing rules
///
/// utility function that dispatches according to the first type
static StrictAliasingError arePointerInterchangeable(const Type *SrcTy,
                                                     const Type *DstTy);
/// \brief utility function that dispatches according to the second type,
/// this will call the arePointerInterchangeable function with the real types
/// for both arguments
template <class T>
static StrictAliasingError arePointerInterchangeable(const T *SrcTy,
                                                     const Type *DstTy);

/// all possible casts to BuiltinType
static StrictAliasingError arePointerInterchangeable(const BuiltinType *SrcTy,
                                                     const BuiltinType *DstTy);
static StrictAliasingError arePointerInterchangeable(const RecordType *SrcTy,
                                                     const BuiltinType *DstTy);
static StrictAliasingError arePointerInterchangeable(const EnumType *SrcTy,
                                                     const BuiltinType *DstTy);

/// all possible casts to RecordType
static StrictAliasingError arePointerInterchangeable(const BuiltinType *SrcTy,
                                                     const RecordType *DstTy);
static StrictAliasingError arePointerInterchangeable(const RecordType *SrcTy,
                                                     const RecordType *DstTy);
static StrictAliasingError arePointerInterchangeable(const EnumType *SrcTy,
                                                     const RecordType *DstTy);

/// all possible casts to EnumType
static StrictAliasingError arePointerInterchangeable(const BuiltinType *SrcTy,
                                                     const EnumType *DstTy);
static StrictAliasingError arePointerInterchangeable(const RecordType *SrcTy,
                                                     const EnumType *DstTy);
static StrictAliasingError arePointerInterchangeable(const EnumType *SrcTy,
                                                     const EnumType *DstTy);

/// [conv.qual] 7.5 (similarity)
///   (1) A cv-decomposition of a type T is a sequence of cv_i and P_i such that
///       T is:  "cv0 P0 cv1 P1 ... cvN-1 PN-1 cvN U" for N > 0,
///       where each cv_i is a set of cv-qualifiers (6.9.3), and each P_i is
///       "pointer to" (11.3.1), "pointer to member of class C_i of type"
///       (11.3.3), "array of N_i", or "array of unknown bound of" (11.3.4). If
///       P_i designates an array, the cv-qualifiers cv_i+1 on the element type
///       are also taken as the cv-qualifiers cv_i of the array.
///       [ Example: The type denoted by the type-id const int ** has two
///       cv-decompositions, taking U as "int **" and as "pointer to const int".
///       — end example ] The N-tuple of cv-qualifiers after the first one in
///       the longest cv-decomposition of T, that is, cv1, cv2, ..., cvN, is
///       called the cv-qualification signature of T.
///   (2) Two types T1 and T2 are similar if they have cv-decompositions with
///   the
///       same N such that corresponding P_i components are the same and the
///       types denoted by U are the same.
///   (3) A prvalue expression of type T1 can be converted to type T2 if the
///       following conditions are satisfied, where cv_i^j denotes the
///       cv-qualifiers in the cv-qualification signature of T_j:
///       — T1 and T2 are similar.
///       — For every i > 0, if const is in cv_i^1 then const is in cv_k^2
///       for 0 < k < i.
///       — If the cv_i^1 and cv_i^2 are different, then const is in every
///       cv_k^2 for 0 < k < i.
///
/// For example:
///  - const int * volatile * and int * * const are similar;
///  - const int (* volatile S::* const)[20] and int (* const S::* volatile)[20]
///    are similar;
///  - int (* const *)(int *) and int (* volatile *)(int *) are similar;
///  - int (S::*)() const and int (S::*)() are not similar;
///  - int (*)(int *) and int (*)(const int *) are not similar;
///  - const int (*)(int *) and int (*)(int *) are not similar;
///  - int (*)(int * const) and int (*)(int *) are similar (they are the same
///    type);
///  - std::pair<int, int> and std::pair<const int, int> are not similar.
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

/// \brief checks whether the type can alias every other type
/// this is the case with [unsigned|signed] char, std::byte, void
/// \note it will be true for signed char too, even the standard does not
/// require this
static bool isAnyAlias(const Type *Ty) {
  if (Ty->isStdByteType())
    return true;
  if (Ty->isVoidType())
    return true;
  if (Ty->isCharType())
    return true;
  return false;
}

/// \brief checks whether the two builtin types can alias eachother.
/// \returns StrictAliasingError::valid if one can alias with the other
/// otherwise returns StrictAliasingError::builtin_to_different_builtin
static StrictAliasingError canAlias(const BuiltinType *SrcTy,
                                    const BuiltinType *DstTy) {
  constexpr StrictAliasingError Valid = StrictAliasingError::valid;
  if (SrcTy == DstTy)
    return Valid;

  if (isAnyAlias(SrcTy) || isAnyAlias(DstTy))
    return Valid;

  // handle unsigned / signed compatibility for each type
  // ignore signess in case of wchar_t
  if (SrcTy->isWideCharType() || DstTy->isWideCharType())
    return Valid;

  // handle other signed unsigned builtin types
  using BTKind = BuiltinType::Kind;
  const BTKind SrcKind = SrcTy->getKind();
  const BTKind DstKind = DstTy->getKind();

  auto bothAreOf = [SrcKind, DstKind](BTKind Signed, BTKind Unsigned) {
    auto oneOf = [](BTKind X, BTKind Signed, BTKind Unsigned) {
      return X == Signed || X == Unsigned;
    };
    return oneOf(SrcKind, Signed, Unsigned) && oneOf(DstKind, Signed, Unsigned);
  };

  if (bothAreOf(BTKind::Short, BTKind::UShort))
    return Valid;

  if (bothAreOf(BTKind::Int, BTKind::UInt))
    return Valid;

  if (bothAreOf(BTKind::Long, BTKind::ULong))
    return Valid;

  if (bothAreOf(BTKind::LongLong, BTKind::ULongLong))
    return Valid;

  if (bothAreOf(BTKind::Int128, BTKind::UInt128))
    return Valid;

  return StrictAliasingError::builtin_to_different_builtin;
}

/// \brief Utility function that decides whether two (general) type are the same
/// or builtin types that can alias each other \returns true if the types are
/// the same or any of them can alias everything or both are builtin types which
/// differs only in signess etc
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
      const auto IsNonEmptyBase = [](const CXXBaseSpecifier &Base) {
        assert(Base.getType()->isRecordType());
        return !Base.getType()->getAsCXXRecordDecl()->isEmpty();
      };

      // find the first non-empty base class
      const auto BaseIt = std::find_if(CXXRecDecl->bases_begin(),
                                       CXXRecDecl->bases_end(), IsNonEmptyBase);
      assert(BaseIt != CXXRecDecl->bases_end() && "non-empty base must exist");
      const Type *BaseTy = BaseIt->getType().getTypePtr();

      // do the same checking on the non-empty base (recurse)
      return arePointerInterchangeable(BaseTy, InnerTy);
    }
  }

  if (RecDecl->field_empty())
    return StrictAliasingError::empty_record;

  // if the C/C++ struct has non-static data members, pick the first one
  const FieldDecl *FirstField = *RecDecl->field_begin();
  if (FirstField->isBitField())
    return StrictAliasingError::first_member_is_bitfield;

  // don't recurse, just do a one level deep check whether the first member of
  // this struct could alias with the other
  if (areSameTypeOrAliasableBuiltins(FirstField->getType().getTypePtr(),
                                     InnerTy))
    return StrictAliasingError::valid;

  return StrictAliasingError::cannot_alias_with_first_member;
}

/// all possible casts to BuiltinType

/// \brief casting builtin type to builtin type is only valid if they can alias
/// each other
static StrictAliasingError arePointerInterchangeable(const BuiltinType *SrcTy,
                                                     const BuiltinType *DstTy) {
  return canAlias(SrcTy, DstTy);
}

static StrictAliasingError arePointerInterchangeable(const EnumType *SrcTy,
                                                     const BuiltinType *DstTy) {
  static_cast<void>(DstTy);

  // if the type is std::byte, magically it can alias just like char does
  if (SrcTy->isStdByteType())
    return StrictAliasingError::valid;

  return StrictAliasingError::enum_to_builtin;
}

/// \brief cast to RecordType
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
  return isOneLevelDeepPrefix(DstTy, SrcTy);
}

/// \brief casting a record to a builtin type is valid if the record the first
/// member (non-transitively) of that record has exactly that type
static StrictAliasingError arePointerInterchangeable(const RecordType *SrcTy,
                                                     const BuiltinType *DstTy) {
  return isOneLevelDeepPrefix(SrcTy, DstTy);
}

/// \brief casting record to record is valid if they are the same
/// or the destination record type is the first member of the source record type
///
/// \note we allow the other way around because there are cases when the
/// user knows for sure that the dynamic type was really that type
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

/// \brief casting an enum to a record
/// if that enum is the std::byte, than valid to convert it to anything
/// if the destination type is not standard layout, than it is bad
/// if the record type that we converting to has the enum type as first member,
/// than ok otherwise bad
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
  return isOneLevelDeepPrefix(DstTy, SrcTy);
}

/// \brief casting a builtin to enum
/// no builtin type can alias with an enum except with std::byte which aliasses
/// with everything
///
/// \code{.cpp}
/// enum E1 : unsigned {Val1 = 1};  // f2(E1*, unsigned int*):
/// int f2(E1 *p, unsigned *q) {    //   movl $1, (%rdi)
///    *p = Val1;                   //   movl $1, %eax
///    *q = 2;                      //   movl $2, (%rsi)
///    return static_cast<int>(*p); //   ret
/// }
/// \endcode
static StrictAliasingError arePointerInterchangeable(const BuiltinType *SrcTy,
                                                     const EnumType *DstTy) {
  static_cast<void>(SrcTy);

  if (DstTy->isStdByteType())
    return StrictAliasingError::valid;

  return StrictAliasingError::enum_to_different_enum;
}

/// \brief casting a record to enum
/// if the destination enum type is std::byte than ok
/// otherwise if the source record type has a first member exactly of that enum
/// type than it is ok
static StrictAliasingError arePointerInterchangeable(const RecordType *SrcTy,
                                                     const EnumType *DstTy) {
  if (DstTy->isStdByteType())
    return StrictAliasingError::valid;

  // if the record has the given enum as it's first non-static data member
  // (non-transitively) (and everything standard layout) than OK, otherwise BAD
  return isOneLevelDeepPrefix(SrcTy, DstTy);
}

/// \brief casting a record to enum
/// can not cast an enum to an other enum except if
/// they are the same or at least one of them is std::byte
/// even if they have the same underlying type
///
/// \code{.cpp}
///  // gcc-8.3 -O2
///  enum E1 {Val1 = 1}; enum E2 {Val2 = 2}; // f1(E1*, E2*):
///  int f1(E1 *p, E2 *q) {                  //   movl $1, (%rdi)
///    *p = Val1;                            //   movl $1, %eax
///    *q = Val2;                            //   movl $2, (%rsi)
///    return static_cast<int>(*p);          //   ret
///  }
///  enum EE1 : unsigned int {Val3 = 1};
///  enum EE2 : unsigned int {Val4 = 2}; // f2(EE1*, EE2*):
///  int f2(EE1 *p, EE2 *q) {            //   movl $1, (%rdi)
///    *p = Val3;                        //   movl $1, %eax
///   *q = Val4;                        //   movl $2, (%rsi)
///    return static_cast<int>(*p);      //   ret
///  }
/// \endcode
static StrictAliasingError arePointerInterchangeable(const EnumType *SrcTy,
                                                     const EnumType *DstTy) {
  if (SrcTy == DstTy)
    return StrictAliasingError::valid;

  // std::byte? to std::byte?
  if (SrcTy->isStdByteType() || DstTy->isStdByteType())
    return StrictAliasingError::valid;

  return StrictAliasingError ::enum_to_different_enum;
}

/// \brief dispatch to the appropriate overload using the second argument
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

// warn if cast to different one
static StrictAliasingError
pointerInterchangeableFunctions(const FunctionProtoType *SrcFun,
                                const FunctionProtoType *DstFun) {
  assert(SrcFun);
  assert(DstFun);

  // different return type
  if (SrcFun->getReturnType().getTypePtr() !=
      DstFun->getReturnType().getTypePtr())
    return StrictAliasingError::unrelated_function_types;

  // different parameter count
  if (SrcFun->getNumParams() != DstFun->getNumParams())
    return StrictAliasingError::unrelated_function_types;

  // if any arguments in order doesn't match exactly without CV qualifiers
  for (auto i = 0u; i < SrcFun->getNumParams(); ++i) {
    if (SrcFun->getParamType(i).getTypePtr() !=
        DstFun->getParamType(i).getTypePtr())
      return StrictAliasingError::unrelated_function_types;
  }

  // otherwise it should be OK
  // we are casting to the same type (possibly to have different CV
  // qualifiers)
  return StrictAliasingError::valid;
}

static StrictAliasingError arePointerInterchangeable(const Type *SrcTy,
                                                     const Type *DstTy) {
  assert(SrcTy);
  assert(DstTy);

  {
    // we are only interested in C++ typed functions
    const auto *SrcFun = SrcTy->getAs<FunctionProtoType>();
    const auto *DstFun = DstTy->getAs<FunctionProtoType>();

    // if both are function types
    if (SrcFun && DstFun)
      return pointerInterchangeableFunctions(SrcFun, DstFun);

    // not both types are function types lets say OK for the cases when only one
    // type is function type
    if (SrcFun || DstFun)
      return StrictAliasingError::valid;
  }

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
peelOffPointersTogether(const Type *SrcTy, const Type *DstTy) {
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
      WarnOnlyIfDereferenced(Options.get("WarnOnlyIfDereferenced", 1) != 0) {}

/// \brief registers the AST matcher to the MatchFinder
/// the ast matcher will match on all the bitcast expressions even if the
/// expression is not dereferenced or used
static void registerBitcastExprs(ClangTidyCheck *Checker, MatchFinder *Finder) {
  const auto refcast =
      explicitCastExpr(hasCastKind(CK_LValueBitCast)).bind("bitcast");
  const auto ptrcast =
      explicitCastExpr(hasCastKind(CK_BitCast)).bind("bitcast");
  Finder->addMatcher(refcast, Checker);
  Finder->addMatcher(ptrcast, Checker);
}

/// \brief registers the AST matcher to the MatchFinder
/// the ast matcher will match on all lvalue reference bitcast expressions and
/// on all pointer dereferences using *, ->, [] operators immediately after
/// bitcast expressions
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

/// \brief registers the appropiate AST matcher according to the
/// 'WarnOnlyIfDereferenced' boolean
void StrictAliasingCheck::registerMatchers(MatchFinder *Finder) {
  if (WarnOnlyIfDereferenced)
    registerBitcastDereferenceExprs(this, Finder);
  else
    registerBitcastExprs(this, Finder);
}

/// \brief declares the WarnOnlyIfDereferenced checker option
/// it will be used to enable the more aggressive AST matcher mechanism
/// which will warn on creating bad pointers
void StrictAliasingCheck::storeOptions(ClangTidyOptions::OptionMap &Opts) {
  Options.store(Opts, "WarnOnlyIfDereferenced", WarnOnlyIfDereferenced);
}

/// \brief utility function for getting the printable name of a type
/// for this usage, if the type is not a pointer type it should be a reference
/// so appends its sign to the name
static std::string getPrettyTypeName(QualType QT) {
  if (!QT->isPointerType())
    return (llvm::Twine(QT.getAsString()) + " &").str();
  return QT.getAsString();
}

void StrictAliasingCheck::check(const MatchFinder::MatchResult &Result) {
  const auto *ECE = Result.Nodes.getNodeAs<ExplicitCastExpr>("bitcast");

  const Type *DstTy = ECE->getType().getTypePtr();
  const Type *SrcTy = ECE->getSubExpr()->getType().getTypePtr();

  std::tie(SrcTy, DstTy) = peelOffPointersTogether(SrcTy, DstTy);

  if (const auto *ET = dyn_cast<ElaboratedType>(SrcTy))
    SrcTy = ET->desugar().getTypePtr();
  if (const auto *ET = dyn_cast<ElaboratedType>(DstTy))
    DstTy = ET->desugar().getTypePtr();

  // if the pointers have different indirection count, return
  if (SrcTy->isPointerType() || DstTy->isPointerType())
    return;

  // function to emit warning message to the cast expression
  const auto Warn = [&](StringRef Message) {
    diag(ECE->getBeginLoc(), Message);
  };

  // text representation of the cast expression's argument
  const std::string SourceExprText = [&Result, &ECE]() {
    std::string Buffer;
    llvm::raw_string_ostream Stream(Buffer);
    ECE->getSubExpr()->printPretty(Stream, nullptr,
                                   Result.Context->getPrintingPolicy());
    return Stream.str();
  }();

  // text representation of the types for diagnostic messages
  const std::string SourceTyText =
      getPrettyTypeName(ECE->getSubExpr()->getType());
  const std::string DestinationTyText = getPrettyTypeName(ECE->getType());

  // analysis result whether the two pointers are interchangeable or not
  const StrictAliasingError result = arePointerInterchangeable(SrcTy, DstTy);

  switch (result) {
  case StrictAliasingError::valid:
    return;
  case StrictAliasingError::incomplete:
    Warn("some type is incomplete. Could not validate cast operation "
         "; may include the types's definition");
    break;
  case StrictAliasingError::non_standard_layout:
    Warn("c++ forbids reinterpret casts to non-standard layout types");
    break;
  case StrictAliasingError::builtin_to_different_builtin:
    Warn("can not cast this builtin type to that different builtin "
         "type safely; consider using std::memcpy instead");
    break;
  case StrictAliasingError::enum_to_builtin:
    Warn((llvm::Twine("invalid to cast '") + SourceExprText + "' of type '" +
          SourceTyText + "' to a fundamental type '" + DestinationTyText +
          "', even if it would be the underlying type")
             .str());
    break;
  case StrictAliasingError::enum_to_different_enum:
    Warn("can not cast enum to different enum type other than std::byte");
    break;
  case StrictAliasingError::empty_record:
    Warn("can not cast to/from an empty struct/class type");
    break;
  case StrictAliasingError::first_member_is_bitfield:
    Warn("the first member of the struct/class is a bitfield, not "
         "allowed to point to it");
    break;
  case StrictAliasingError::cannot_alias_with_first_member:
    Warn("the first member of the struct/class has incompatible type");
    break;
  case StrictAliasingError::unrelated_record_types:
    Warn("the structs/classes are unrelated, can not cast between them");
    break;
  case StrictAliasingError::unrelated_function_types:
    Warn("the function types does not match, can not safely use the resulting "
         "pointer");
    break;
  default:
    llvm_unreachable("all possible errors should be handled");
  }
}

} // namespace bugprone
} // namespace tidy
} // namespace clang
