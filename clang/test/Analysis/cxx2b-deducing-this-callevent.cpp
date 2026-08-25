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
  int implicit_method(int);
  int explicit_method(this const S &self, int);

  int operator+(int) const;
  int operator-(this const S &self, int);

  static int operator()(int);
};

void calls(S s) {
  s.implicit_method(1);
  // CHECK-DAG: PreCall (classification::S::implicit_method) [CXXMemberCall]

  s.explicit_method(1);
  // CHECK-DAG: PreCall (classification::S::explicit_method) [CXXMemberCall]

  (void)(s + 1);
  // CHECK-DAG: PreCall (classification::S::operator+) [CXXMemberOperatorCall]

  (void)(s - 1);
  // CHECK-DAG: PreCall (classification::S::operator-) [CXXMemberOperatorCall]

  s(1);
  // CHECK-DAG: PreCall (classification::S::operator()) [CXXStaticOperatorCall]
}

// A call through a pointer to an explicit object member function has no direct
// callee at the call site, so it remains a plain function call.
void through_function_pointer(S s) {
  int (*fp)(const S &, int) = &S::explicit_method;
  fp(s, 1);
  // CHECK-DAG: PreCall (classification::S::explicit_method) [SimpleFunctionCall]
}

// An explicit object conversion function is also built as a plain CallExpr,
// whose only argument is the object.
struct Convertible {
  operator int(this const Convertible &self);
};
void conversion(Convertible c) {
  int i = c;
  (void)i;
  // CHECK-DAG: PreCall (classification::Convertible::operator int) [CXXMemberCall]
}
} // namespace classification

namespace use_after_move {
struct A {
  A();
  A(A &&);
  A &operator=(A &&);

  void implicit_use() const;
  void explicit_use(this const A &self);

  bool operator>(const A &) const;
  bool operator<(this const A &self, const A &);
};

void implicit_method(A a) {
  A b = std::move(a);
  a.implicit_use(); // expected-warning {{Method called on moved-from object 'a'}}
}

void explicit_method(A a) {
  A b = std::move(a);
  a.explicit_use(); // expected-warning {{Method called on moved-from object 'a'}}
}

void implicit_operator(A a, A c) {
  A b = std::move(a);
  (void)(a > c); // expected-warning {{Method called on moved-from object 'a'}}
}

void explicit_operator(A a, A c) {
  A b = std::move(a);
  (void)(a < c); // expected-warning {{Method called on moved-from object 'a'}}
}
} // namespace use_after_move

namespace null_object {
struct B {
  int implicit_method();
  int explicit_method(this const B &self);
};

void implicit_method(B *p) {
  if (p)
    return;
  p->implicit_method(); // expected-warning {{Called C++ object pointer is null}}
}

void explicit_method(B *p) {
  if (p)
    return;
  p->explicit_method(); // expected-warning {{Called C++ object pointer is null}}
}
} // namespace null_object

namespace nonnull_attribute {
// The 'nonnull' attribute counts the explicit object parameter, while the
// CallEvent does not count the object argument. Both spellings must blame the
// same argument.
struct S {
  void implicit_arg(int *p) __attribute__((nonnull(2)));
  void explicit_arg(this S &self, int *p) __attribute__((nonnull(2)));
  void explicit_param_attr(this S &self, int *p __attribute__((nonnull)));
};

void implicit_arg(S s, int *p) {
  if (p)
    return;
  s.implicit_arg(p); // expected-warning {{Null pointer passed to 1st parameter expecting 'nonnull'}}
}

void explicit_arg(S s, int *p) {
  if (p)
    return;
  s.explicit_arg(p); // expected-warning {{Null pointer passed to 1st parameter expecting 'nonnull'}}
}

void explicit_param_attr(S s, int *p) {
  if (p)
    return;
  s.explicit_param_attr(p); // expected-warning {{Null pointer passed to 1st parameter expecting 'nonnull'}}
}
} // namespace nonnull_attribute

namespace invalidation {
// A const explicit object parameter preserves the object's contents just like a
// const implicit object parameter does...
struct Const {
  int n;
  void implicit_peek() const;
  void explicit_peek(this const Const &self);
};

void implicit_const() {
  Const c;
  c.n = 3;
  c.implicit_peek();
  clang_analyzer_eval(c.n == 3); // expected-warning {{TRUE}}
}

void explicit_const() {
  Const c;
  c.n = 3;
  c.explicit_peek();
  clang_analyzer_eval(c.n == 3); // expected-warning {{TRUE}}
}

// ... including the exception for mutable fields.
struct Mutable {
  int n;
  mutable int m;
  void implicit_peek() const;
  void explicit_peek(this const Mutable &self);
};

void implicit_mutable() {
  Mutable c;
  c.n = 3;
  c.implicit_peek();
  clang_analyzer_eval(c.n == 3); // expected-warning {{TRUE}} expected-warning {{FALSE}}
}

void explicit_mutable() {
  Mutable c;
  c.n = 3;
  c.explicit_peek();
  clang_analyzer_eval(c.n == 3); // expected-warning {{TRUE}} expected-warning {{FALSE}}
}

// A by-value explicit object parameter cannot modify the caller's object.
struct ByValue {
  int n;
  void peek(this ByValue self);
};

void by_value() {
  ByValue c;
  c.n = 3;
  c.peek();
  clang_analyzer_eval(c.n == 3); // expected-warning {{TRUE}}
}

// A non-const explicit object parameter must invalidate it.
struct NonConst {
  int n;
  void poke(this NonConst &self);
};

void non_const() {
  NonConst c;
  c.n = 3;
  c.poke();
  clang_analyzer_eval(c.n == 3); // expected-warning {{TRUE}} expected-warning {{FALSE}}
}
} // namespace invalidation

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
