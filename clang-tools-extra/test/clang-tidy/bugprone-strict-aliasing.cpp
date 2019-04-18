// RUN: %check_clang_tidy %s bugprone-strict-aliasing %t

struct Empty1 {}; // empty, has no members
struct Empty2 {};
struct Empty3 {};
struct Empty43 : Empty3 {};
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

// check record to builtin type
void rec_to_bt(Derived *d) {
  *(int *)d; // bad

  using Derived_alias = Derived;
  Derived_alias *p = d; // bad
  *(int *)p; // bad
}

void qqqqqqq(Empty43 *p) {
  *(Empty2 *)p; // bad
  *(int *)p; // bad
}

enum E { DUMMY };
void incomplete_enum(enum E *e, int *i) {
  *(int *)e; // bad
  *(enum E *)i; // bad
}

struct Forw;
void hh(Forw *f, float *g) {
  *(Forw *)g; // considered bad
  *(float *)f; // considered bad
}

// ignore: Dependant casts
template <class T>
struct Dependent;
template <class T>
void hg(Dependent<T> *f, float *g) {
  *(Dependent<T> *)g;
  *(float *)f;
}

// ignore: IntegralToPointer, PointerToIntegral casts
void dd(int ip) {
  *(Base *)ip;
}

void TODO(Base **ip) {
  *(Base *)ip;
}

enum ENum : int { EE,
                  BB };
enum class ENUM : long { CC,
                         DD,
                         EE };
void uuu(ENum *p, ENUM *q) {
  *(ENUM *)p;
  *(ENum *)q;
  *reinterpret_cast<ENUM *>(q); // noop cast
}

void f() {
  bool b;
  // CHECK-MESSAGES: :[[@LINE+1]]:4: warning: cast is bugprone? [bugprone-strict-aliasing]
  (reinterpret_cast<char &>(b)) = '\0';
}

void f2(float &f) {
  // CHECK-MESSAGES: :[[@LINE+1]]:3: warning: cast is bugprone? [bugprone-strict-aliasing]
  reinterpret_cast<A &>(f).i = '\0';
}

int g(float *f) {
  // CHECK-MESSAGES: :[[@LINE+1]]:12: warning: cast is bugprone? [bugprone-strict-aliasing]
  return *((int *)f);
}

int h(float *f) {
  // CHECK-MESSAGES: :d[[@LINE+1]]:11: warning: cast is bugprone? [bugprone-strict-aliasing]
  return ((A *)f)->i;
}

/* // handled by other checkers
void i() {
  int val = 12;
	float *g = reinterpret_cast<float*>(val);
}*/

int k(float &f) {
  // CHECK-MESSAGES: :[[@LINE+1]]:10: warning: cast is bugprone? [bugprone-strict-aliasing]
  return reinterpret_cast<A &>(f).i;
}

// multiple paren around
int z(float *f) {
  // CHECK-MESSAGES: :[[@LINE+1]]:13: warning: cast is bugprone? [bugprone-strict-aliasing]
  return ((((A *)f)))->i;
}

int w(float *f) {
  // CHECK-MESSAGES: :[[@LINE+1]]:12: warning: cast is bugprone? [bugprone-strict-aliasing]
  return (((int *)f))[2];
}

int ww(void *p) {
  return *static_cast<int *>(p);
}

const volatile float *www(volatile float *p) {
  reinterpret_cast<const volatile float *>(p)[0];
  return reinterpret_cast<const volatile float *>(p);
}
