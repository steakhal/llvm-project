// RUN: %clang_cc1 -analyze -analyzer-checker=core,unix.Malloc,debug.ExprInspection \
// RUN:   -verify=false-positive,true-positive,refuted-true-positive %s
// RUN: %clang_cc1 -analyze -analyzer-checker=core,unix.Malloc,debug.ExprInspection \
// RUN:   -analyzer-config crosscheck-with-z3=true -verify=true-positive %s
// REQU IRES: z3

void clang_analyzer_warnIfReached();

int foo(int x) 
{
  int *z = 0;
  if ((x & 1) && ((x & 1) ^ 1))
    return *z; // false-positive-warning {{Dereference of null pointer (loaded from variable 'z')}}
  return 0;
}

void g(int d);

void f(int *a, int *b) {
  int c = 5;
  if ((a - b) == 0)
    c = 0;
  if (a != b)
    g(3 / c); // no-warning
}

_Bool nondet_bool();

void h(int d) {
  int x, y, k, z = 1;
  while (z < k) { // true-positive-warning {{The right operand of '<' is a garbage value}}
    z = 2 * z;
  }
}

void i() {
  _Bool c = nondet_bool();
  if (c) {
    h(1);
  } else {
    h(2);
  }
}

void should_warn(int a, unsigned char b) {
  // Consider this interpretation: {a: -250, b: 6}
  // It should satisfy both branches since (unsigned char)-250 == 6.
  if (a < -200) {
    clang_analyzer_warnIfReached(); // true-positive-warning {{REACHABLE}}
    if ((unsigned char)a == b) {
      // FIXME: Should be reachable even with refutation!
      // The cause of this that we are cheap on modeling integral casts as written.
      // If we were adding the symbolic cast, it would work.
      clang_analyzer_warnIfReached(); // refuted-true-positive-warning {{REACHABLE}}
    }
  }
}
