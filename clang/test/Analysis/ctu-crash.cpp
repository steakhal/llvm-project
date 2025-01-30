// RUN: rm -rf %t && mkdir %t
// RUN: split-file %s %t

// RUN: %clang_cc1 -emit-pch -o %t/remote.ast %t/remote.cpp

// RUN: %clang_cc1 -analyze \
// RUN:   -analyzer-checker=core \
// RUN:   -analyzer-config experimental-enable-naive-ctu-analysis=true \
// RUN:   -analyzer-config  display-ctu-progress=true \
// RUN:   -analyzer-config ctu-dir=%t \
// RUN:   -verify %t/entrypoint.cpp

//--- entrypoint.cpp
namespace mozilla {
void remote();
} // namespace mozilla

void entrypoint() {
  mozilla::remote();
}

//--- remote.cpp
template <typename _Tp> struct underlying_type {
  using type = __underlying_type(_Tp);
};

namespace mozilla {
template <typename Enum> auto UnderlyingValue(Enum v) {
  return static_cast<typename underlying_type<Enum>::type>(v);
}

enum class RootKind : char { Nothing };

void remote() {
  UnderlyingValue(RootKind::Nothing);
}
} // namespace mozilla

/*
When importing `remote()`, we encounter a CallExpr, in which we have an ImplicitCastExpr ('typename identity<RootKind>::type (*)(mozilla::RootKind)' <FunctionToPointerDecay>)
The import of `ImplicitCastExpr` immediatelly triggers the import of the type of the expression.
That imports the `FunctionProtoType` of the `underlying_type<enum RootKind>` specialization.
The first thing a FunctionProtoType imports is the return type, which is AutoType in our case.
An AutoType immediatelly imports the deduced type.
That will eventually import the SubstTemplateTypeParmType of `T=enum mozilla::RootKind`.
Well, a SubstTemplateTypeParmType has an associated decl, the decl of the template specialization.
That needs to be imported before importing the SubstTemplateTypeParmType itself.
*/

//--- externalDefMap.txt
22:c:@N@mozilla@F@remote# remote.ast
