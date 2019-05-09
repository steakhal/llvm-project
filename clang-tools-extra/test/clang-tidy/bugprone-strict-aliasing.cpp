// RUN: %check_clang_tidy %s bugprone-strict-aliasing %t -- -- -std=c++17

// RUN: %check_clang_tidy %s bugprone-strict-aliasing %t \
// RUN:   -config="{CheckOptions: \
// RUN:             [{key: bugprone-strict-aliasing.WarnOnlyIfDereferenced, \
// RUN:               value: 0 }]}" \
// RUN:   -- -std=c++17 -DWARN_ONLY_IF_DEREFERENCED_OFF

namespace std {
    enum class byte : unsigned char {};
}  // namespace std


struct Empty1 {}; // empty, has no members
struct Empty2 {};
struct Empty3 {};
struct Empty4 : Empty3 {};
struct Base : Empty3 {
  int i : 2;
  float f;
};
struct ProxyBase : Empty1, Base, Empty2 {};
struct Derived : ProxyBase {
  // empty, has no members
};

struct A {
  int i;
};

Base b;

void record_to_builtin(Derived *d) {
  *(int *)d;
  // CHECK-MESSAGES: :[[@LINE-1]]:4: warning: the first member of the struct/class is a bitfield, not allowed to point to it [bugprone-strict-aliasing]

  using DerivedAlias = Derived;
  DerivedAlias *alias = d;
  *(int *)alias;
  // CHECK-MESSAGES: :[[@LINE-1]]:4: warning: the first member of the struct/class is a bitfield, not allowed to point to it [bugprone-strict-aliasing]
}

void casting_to_and_from_empty_struct(Empty4 *p) {
  *(Empty2 *)p;
  // CHECK-MESSAGES: :[[@LINE-1]]:4: warning: the structs/classes are unrelated, can not cast between them [bugprone-strict-aliasing]

  *(int *)p;
  // CHECK-MESSAGES: :[[@LINE-1]]:4: warning: can not cast to/from an empty struct/class type [bugprone-strict-aliasing]
}

enum E { DUMMY };
void enum_to_int(enum E *e, int *i) {
  *(int *)e;
  // CHECK-MESSAGES: :[[@LINE-1]]:4: warning: invalid to cast 'e' of type 'enum E *' to a fundamental type 'int *', even if it would be the underlying type [bugprone-strict-aliasing]

  *(enum E *)i;
  // CHECK-MESSAGES: :[[@LINE-1]]:4: warning: can not cast enum to different enum type other than std::byte [bugprone-strict-aliasing]
}

struct Forw;
void ignore_forward_decls(Forw *f, float *g) {
  *(Forw *)g; // considered bad
  // CHECK-MESSAGES: :[[@LINE-1]]:4: warning: some type is incomplete. Could not validate cast operation ; may include the types's definition [bugprone-strict-aliasing]

  *(float *)f; // considered bad
  // CHECK-MESSAGES: :[[@LINE-1]]:4: warning: some type is incomplete. Could not validate cast operation ; may include the types's definition [bugprone-strict-aliasing]
}

template <class T> struct Dependent;
template <class T>
void ignore_dependant(Dependent<T> *f, float *g) {
  *(Dependent<T> *)g; // should ignore Dependant casts
  *(float *)f; // should ignore Dependant casts
}

void ignore_integer_to_pointer(long ip) {
  *(Base *)ip; // should ignore IntegralToPointer casts
  ip = (long)&ip; // should ignore PointerToIntegral casts
}


void non_similar_ptr_cast(Base **ip) {
  *(Base *)ip; // casting a Base** to Base*, which is not similar to it
  // TODO catch this
}

enum SimpleEnum : int { EE, BB };
enum class EnumClass : long { CC, DD, EE };
void enum_to_enum_casts(SimpleEnum *p, EnumClass *q) {
  *(EnumClass *)p;
  // CHECK-MESSAGES: :[[@LINE-1]]:4: warning: can not cast enum to different enum type other than std::byte [bugprone-strict-aliasing]

  *(SimpleEnum *)q;
  // CHECK-MESSAGES: :[[@LINE-1]]:4: warning: can not cast enum to different enum type other than std::byte [bugprone-strict-aliasing]

  *reinterpret_cast<EnumClass *>(q); // noop cast, ignore
}

void lval_ref_cast() {
  int myint = -1;
  (reinterpret_cast<char &>(myint)) = '\0'; // can not detect this

  (reinterpret_cast<float &>(myint)) = 3.14; // accessing it through float type
  // CHECK-MESSAGES: :[[@LINE-1]]:4: warning: can not cast this builtin type to that different builtin type safely; consider using std::memcpy instead [bugprone-strict-aliasing]

  float fl = 3.14;
  reinterpret_cast<A &>(fl).i = '\0'; // 'A' contains an int as first direct member
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: the first member of the struct/class has incompatible type [bugprone-strict-aliasing]

  reinterpret_cast<A &>(myint).i = '\0'; // should not warn
}

void simple_with_parens(float *f) {
  *(int *)f;
  // CHECK-MESSAGES: :[[@LINE-1]]:4: warning: can not cast this builtin type to that different builtin type safely; consider using std::memcpy instead [bugprone-strict-aliasing]

  *(((int *)f));
  // CHECK-MESSAGES: :[[@LINE-1]]:6: warning: can not cast this builtin type to that different builtin type safely; consider using std::memcpy instead [bugprone-strict-aliasing]
}

void member_access_with_parens(float *fl) {
  ((A *)fl)->i; // 'A' contains an int as first direct member
  // CHECK-MESSAGES: :[[@LINE-1]]:4: warning: the first member of the struct/class has incompatible type [bugprone-strict-aliasing]

  ((((A *)fl)))->i;
  // CHECK-MESSAGES: :[[@LINE-1]]:6: warning: the first member of the struct/class has incompatible type [bugprone-strict-aliasing]

  reinterpret_cast<A &>(*fl).i;
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: the first member of the struct/class has incompatible type [bugprone-strict-aliasing]
}

void array_index_with_parens(double *d) {
  ((int *)d)[2];
  // CHECK-MESSAGES: :[[@LINE-1]]:4: warning: can not cast this builtin type to that different builtin type safely; consider using std::memcpy instead [bugprone-strict-aliasing]

  ((((int *)d)))[2];
  // CHECK-MESSAGES: :[[@LINE-1]]:6: warning: can not cast this builtin type to that different builtin type safely; consider using std::memcpy instead [bugprone-strict-aliasing]

  ((int (&)[4])*d)[2]; // cast to 4 element int array, take the third of it
  // CHECK-MESSAGES: :[[@LINE-1]]:4: warning: can not cast this builtin type to that different builtin type safely; consider using std::memcpy instead [bugprone-strict-aliasing]

  ((((int (&)[4])*d)))[2];
  // CHECK-MESSAGES: :[[@LINE-1]]:6: warning: can not cast this builtin type to that different builtin type safely; consider using std::memcpy instead [bugprone-strict-aliasing]
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


#ifdef WARN_ONLY_IF_DEREFERENCED_OFF
void testing_checker_option(Derived *d) {
  // using strict mode we don't even allow creating bad pointers

  int *p = (int *)d;
  // CHECK-MESSAGES: :[[@LINE-1]]:12: warning: the first member of the struct/class is a bitfield, not allowed to point to it [bugprone-strict-aliasing]

  (Empty2 *)p;
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: can not cast to/from an empty struct/class type [bugprone-strict-aliasing]
}
#endif
