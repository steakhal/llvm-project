// RUN: %check_clang_tidy -check-suffix=DEFAULT %s bugprone-strict-aliasing %t -- -- -std=c++17

// RUN: %check_clang_tidy %s bugprone-strict-aliasing %t \
// RUN:   -check-suffixes=DEFAULT,STRICT \
// RUN:   -config="{CheckOptions: \
// RUN:             [{key: bugprone-strict-aliasing.WarnOnlyIfDereferenced, \
// RUN:               value: 0 }]}" \
// RUN:   -- -std=c++17

namespace std {
    enum class byte : unsigned char {};
}  // namespace std


struct Empty1 {}; // empty, has no members
struct Empty2 {};
struct Empty3 {};
struct Empty4 : Empty3 {};
struct BitfieldBase : Empty3 {
  int i : 2;
  float f;
};
struct Base : Empty3 {
    int i;
    float f;
};
struct ProxyBase : Empty1, BitfieldBase, Empty2 {};
struct Derived : ProxyBase {
  // empty, has no members
};

struct Derived2 : Empty1, Base, Empty2 {};
struct NonStdDerived2 : Empty1, Base, Empty2 {
    double dbl;
};

struct A {
  int i;
};
struct SameA {
  int i;
};


void record_to_builtin(Derived *d) {
  *(int *)d;
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:4: warning: the first member of the struct/class is a bitfield, not allowed to point to it [bugprone-strict-aliasing]

  using DerivedAlias = Derived;
  DerivedAlias *alias = d;
  *(int *)alias;
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:4: warning: the first member of the struct/class is a bitfield, not allowed to point to it [bugprone-strict-aliasing]


  Derived2 d2;
  *(int*)&d2; // OK, Derived2 starts with an integer which is inherited from Base
  *(unsigned int*)&d2; // OK

  NonStdDerived2 nond2;
  *(int*)&nond2; // bad, since NonStdDerived2 has non standard layout
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:4: warning: c++ forbids reinterpret casts to non-standard layout types [bugprone-strict-aliasing]

  *(unsigned int*)&nond2; // bad, for same reason
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:4: warning: c++ forbids reinterpret casts to non-standard layout types [bugprone-strict-aliasing]
}

void casting_to_and_from_empty_struct(Empty4 *p) {
  *reinterpret_cast<Empty4*>(p); // OK, self cast

  *(Empty2 *)p;
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:4: warning: the structs/classes are unrelated, can not cast between them [bugprone-strict-aliasing]

  *(int *)p;
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:4: warning: can not cast to/from an empty struct/class type [bugprone-strict-aliasing]
}

void casting_unrelated() {
  A a;
  SameA aa;

  *(SameA*)&a;
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:4: warning: the structs/classes are unrelated, can not cast between them [bugprone-strict-aliasing]

  *(A*)&aa;
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:4: warning: the structs/classes are unrelated, can not cast between them [bugprone-strict-aliasing]
}

enum E { DUMMY };
void enum_to_int(enum E *e, int *i) {
  *(int *)e;
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:4: warning: invalid to cast 'e' of type 'enum E *' to a fundamental type 'int *', even if it would be the underlying type [bugprone-strict-aliasing]

  *(enum E *)i;
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:4: warning: can not cast enum to different enum type other than std::byte [bugprone-strict-aliasing]
}

struct Forw;
void ignore_forward_decls(Forw *f, float *g) {
  *(Forw *)g; // considered bad
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:4: warning: some type is incomplete. Could not validate cast operation ; may include the types's definition [bugprone-strict-aliasing]

  *(float *)f; // considered bad
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:4: warning: some type is incomplete. Could not validate cast operation ; may include the types's definition [bugprone-strict-aliasing]
}

template <class T> struct Dependent;
template <class T>
void ignore_dependant(Dependent<T> *f, float *g) {
  *(Dependent<T> *)g; // should ignore Dependant casts
  *(float *)f; // should ignore Dependant casts
}

void ignore_integer_to_pointer(long ip) {
  *(BitfieldBase *)ip; // should ignore IntegralToPointer casts
  ip = (long)&ip; // should ignore PointerToIntegral casts
}


void non_similar_ptr_cast(BitfieldBase **ip) {
  *(BitfieldBase *)ip; // casting a BitfieldBase** to BitfieldBase*, which is not similar to it
  // TODO catch this
}

enum SimpleEnum : int { EE, BB };
enum class EnumClass : long { CC, DD, EE };
void enum_to_enum_casts(SimpleEnum *p, EnumClass *q) {
  *(EnumClass *)p;
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:4: warning: can not cast enum to different enum type other than std::byte [bugprone-strict-aliasing]

  *(SimpleEnum *)q;
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:4: warning: can not cast enum to different enum type other than std::byte [bugprone-strict-aliasing]

  *reinterpret_cast<EnumClass *>(q); // noop cast, ignore
}

void lval_ref_cast() {
  int myint = -1;
  (reinterpret_cast<char &>(myint)) = '\0'; // can not detect this

  (reinterpret_cast<float &>(myint)) = 3.14; // accessing it through float type
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:4: warning: can not cast this builtin type to that different builtin type safely; consider using std::memcpy instead [bugprone-strict-aliasing]

  float fl = 3.14;
  reinterpret_cast<A &>(fl).i = '\0'; // 'A' contains an int as first direct member
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:3: warning: the first member of the struct/class has incompatible type [bugprone-strict-aliasing]

  reinterpret_cast<A &>(myint).i = '\0'; // should not warn
}

void simple_with_parens(float *f) {
  *(int *)f;
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:4: warning: can not cast this builtin type to that different builtin type safely; consider using std::memcpy instead [bugprone-strict-aliasing]

  *(((int *)f));
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:6: warning: can not cast this builtin type to that different builtin type safely; consider using std::memcpy instead [bugprone-strict-aliasing]
}

void member_access_with_parens(float *fl) {
  ((A *)fl)->i; // 'A' contains an int as first direct member
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:4: warning: the first member of the struct/class has incompatible type [bugprone-strict-aliasing]

  ((((A *)fl)))->i;
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:6: warning: the first member of the struct/class has incompatible type [bugprone-strict-aliasing]

  reinterpret_cast<A &>(*fl).i;
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:3: warning: the first member of the struct/class has incompatible type [bugprone-strict-aliasing]
}

void array_index_with_parens(double *d) {
  ((int *)d)[2];
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:4: warning: can not cast this builtin type to that different builtin type safely; consider using std::memcpy instead [bugprone-strict-aliasing]

  ((((int *)d)))[2];
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:6: warning: can not cast this builtin type to that different builtin type safely; consider using std::memcpy instead [bugprone-strict-aliasing]

  ((int (&)[4])*d)[2]; // cast to 4 element int array, take the third of it
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:4: warning: can not cast this builtin type to that different builtin type safely; consider using std::memcpy instead [bugprone-strict-aliasing]

  ((((int (&)[4])*d)))[2];
  // CHECK-MESSAGES-DEFAULT: :[[@LINE-1]]:6: warning: can not cast this builtin type to that different builtin type safely; consider using std::memcpy instead [bugprone-strict-aliasing]
}

void ignore_static_cast_related(double *p) {
  *static_cast<int *>(static_cast<void*>(p)); // already code smell, ignore this for now

  void *vp = static_cast<void*>(p);
  vp = reinterpret_cast<void*>(p);
  *(int*)vp;
}

void char_like_access() {
    // CV qualifiers ignored
    const volatile double * const* d = nullptr;
    double * d2 = *(double **)d; // ok


    // ignore casting to char-like pointer
    *(char*)d;
    *(signed char*)d;
    *(unsigned char*)d;
    *(std::byte*)d;
    *(wchar_t*)d;


    // ignore casting from char-like pointer
    char c; *(double*)&c;
    unsigned char uc; *(double*)&uc;
    signed char sc; *(double*)&sc;
    std::byte stdb; *(double*)&stdb;
    wchar_t wc; *(double*)&wc;


    // signess difference allowed
    short s; *(unsigned short*)&s;
    unsigned short us; *(short*)&us;

    int i; *(unsigned*)&i;
    unsigned ui; *(int*)&ui;

    long l; *(unsigned long*)&l;
    unsigned long ul; *(long*)&ul;

    long long ll; *(unsigned long long*)&ll;
    unsigned long long ull; *(long long*)&ull;

    __int128_t i128; *(__uint128_t*)&i128;
    __uint128_t ui128; *(__int128_t*)&ui128;
}

void testing_checker_option(Derived *d) {
  // using strict mode we don't even allow creating bad pointers

  int *p = (int *)d;
  // CHECK-MESSAGES-STRICT: :[[@LINE-1]]:12: warning: the first member of the struct/class is a bitfield, not allowed to point to it [bugprone-strict-aliasing]

  (Empty2 *)p;
  // CHECK-MESSAGES-STRICT: :[[@LINE-1]]:3: warning: can not cast to/from an empty struct/class type [bugprone-strict-aliasing]
}



struct S { int x; };
struct T { int x; int f(); };
struct S1 : S {}; // standard-layout
struct ST : S, T {}; // not standard-layout

void struct_test_cases() {
  S s = {};
  auto p = reinterpret_cast<T *>(&s); // value of p is "pointer to s"
  // CHECK-MESSAGES-STRICT: :[[@LINE-1]]:12: warning: the structs/classes are unrelated, can not cast between them [bugprone-strict-aliasing]

  // auto i = p->x; // class member access expression is undefined behavior; s is not a T object
  // p->x = 1; // undefined behavior
  // p->f();   // undefined behavior

  S1 s1 = {};
  auto p1 = reinterpret_cast<S *>(&s1); // value of p1 is "pointer to the S subobject of s1"
  auto i = p1->x; // OK
  p1->x = 1; // OK

  ST st = {};
  auto p2 = reinterpret_cast<S *>(&st); // value of p2 is "pointer to st"
  // CHECK-MESSAGES-STRICT: :[[@LINE-1]]:13: warning: the structs/classes are unrelated, can not cast between them [bugprone-strict-aliasing]

  // auto i = p2->x; // undefined behavior
  // p2->x = 1; // undefined behavior
}


int f(int, int) { return 42; }
void casting_function_pointers() {
  void(*fp1)() = reinterpret_cast<void(*)()>(f);
  // CHECK-MESSAGES-STRICT: :[[@LINE-1]]:18: warning: the function types does not match, can not safely use the resulting pointer [bugprone-strict-aliasing]
  // casting to wrong return type
  void (*fp2)(int, int) = reinterpret_cast<void(*)(int, int)>(f);
  // CHECK-MESSAGES-STRICT: :[[@LINE-1]]:27: warning: the function types does not match, can not safely use the resulting pointer [bugprone-strict-aliasing]

  // only CV difference on parameters, OK
  int (*fp3)(const int, volatile int) = reinterpret_cast<int(*)(const int, volatile int)>(f);

  // only CV difference on parameters and return type, OK
  const volatile int (*fp4)(const int, volatile int) = reinterpret_cast<const volatile int(*)(const int, volatile int)>(f);

  // parameter taken by ref now, bad
  int (*fp5)(const int&, volatile int) = reinterpret_cast<int(*)(const int&, volatile int)>(f);
  // CHECK-MESSAGES-STRICT: :[[@LINE-1]]:42: warning: the function types does not match, can not safely use the resulting pointer [bugprone-strict-aliasing]
}
