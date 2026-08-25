// RUN: %clang_analyze_cc1 -std=c++2b %s \
// RUN:   -analyzer-checker=debug.AnalysisOrder \
// RUN:   -analyzer-config debug.AnalysisOrder:PreCall=true \
// RUN:   2>&1 | FileCheck %s

// RUN: %clang_analyze_cc1 -std=c++2b -verify %s \
// RUN:   -analyzer-checker=core,cplusplus.Move,debug.ExprInspection

// Check how calls to member functions with an explicit object parameter
// ("deducing this") are represented by CallEvent, and that checkers keying on
// CXXInstanceCall see them.
//
// Top-level functions are analyzed in reverse source order, hence CHECK-DAG.

void clang_analyzer_eval(bool);

namespace std {
template <class T> T &&move(T &);
} // namespace std

namespace classification {
struct S {
  int operator+(int) const;
  int operator-(this const S &self, int);

  static int operator()(int);
};

void calls(S s) {
  (void)(s + 1);
  // CHECK-DAG: PreCall (classification::S::operator+) [CXXMemberOperatorCall]

  (void)(s - 1);
  // CHECK-DAG: PreCall (classification::S::operator-) [CXXMemberOperatorCall]

  s(1);
  // CHECK-DAG: PreCall (classification::S::operator()) [CXXStaticOperatorCall]
}
} // namespace classification

namespace use_after_move {
struct A {
  A();
  A(A &&);
  A &operator=(A &&);

  bool operator>(const A &) const;
  bool operator<(this const A &self, const A &);
};

void implicit_operator(A a, A c) {
  A b = std::move(a);
  (void)(a > c); // expected-warning {{Method called on moved-from object 'a'}}
}

void explicit_operator(A a, A c) {
  A b = std::move(a);
  (void)(a < c); // expected-warning {{Method called on moved-from object 'a'}}
}
} // namespace use_after_move

namespace trivial_assignment {
// A defaulted assignment operator may declare its object parameter explicitly.
// ExprEngine::performTrivialCopy() reads the object through getCXXThisVal() and
// the source through getArgSVal(0), so both must be right.
struct S {
  int n;
  S &operator=(this S &self, const S &) = default;
};

void top(S a, S b) {
  b.n = 7;
  a = b;
  clang_analyzer_eval(a.n == 7); // expected-warning {{TRUE}}
}
} // namespace trivial_assignment
