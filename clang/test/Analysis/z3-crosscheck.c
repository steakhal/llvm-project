// RUN: %clang_analyze_cc1_range -analyzer-checker=core,unix.Malloc,debug.ExprInspection \
// RUN:   -verify=false-positive,expected %s
// RUN: %clang_analyze_cc1_range -analyzer-checker=core,unix.Malloc,debug.ExprInspection \
// RUN:   -analyzer-config crosscheck-with-z3=true -verify=expected %s
// REQUIRES: z3

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
  while (z < k) { // expected-warning {{The right operand of '<' is a garbage value}}
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
