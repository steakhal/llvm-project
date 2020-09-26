// RUN: %clang_analyze_cc1 -std=c++11 -Wno-array-bounds \
// RUN:   -analyzer-checker=unix,core,alpha.security.ArrayBoundV2,debug.ExprInspection \
// RUN:   -analyzer-config eagerly-assume=false -verify %s

void clang_analyzer_eval(int);

// Tests doing an out-of-bounds access after the end of an array using:
// - constant integer index
// - constant integer size for buffer
void test1(int x) {
  int *buf = new int[100];
  buf[100] = 1; // expected-warning{{Out of bound memory access}}
}

void test1_ok(int x) {
  int *buf = new int[100];
  buf[99] = 1; // no-warning
}

// Tests doing an out-of-bounds access after the end of an array using:
// - indirect pointer to buffer
// - constant integer index
// - constant integer size for buffer
void test1_ptr(int x) {
  int *buf = new int[100];
  int *p = buf;
  p[101] = 1; // expected-warning{{Out of bound memory access}}
}

void test1_ptr_ok(int x) {
  int *buf = new int[100];
  int *p = buf;
  p[99] = 1; // no-warning
}

// Tests doing an out-of-bounds access before the start of an array using:
// - indirect pointer to buffer, manipulated using simple pointer arithmetic
// - constant integer index
// - constant integer size for buffer
void test1_ptr_arith(int x) {
  int *buf = new int[100];
  int *p = buf;
  p = p + 100;
  p[0] = 1; // expected-warning{{Out of bound memory access}}
}

void test1_ptr_arith_ok(int x) {
  int *buf = new int[100];
  int *p = buf;
  p = p + 99;
  p[0] = 1; // no-warning
}

void test1_ptr_arith_bad(int x) {
  int *buf = new int[100];
  int *p = buf;
  p = p + 99;
  p[1] = 1; // expected-warning{{Out of bound memory access}}
}

void test1_ptr_arith_ok2(int x) {
  int *buf = new int[100];
  int *p = buf;
  p = p + 99;
  p[-1] = 1; // no-warning
}

// Tests doing an out-of-bounds access before the start of an array using:
// - constant integer index
// - constant integer size for buffer
void test2(int x) {
  int *buf = new int[100];
  buf[-1] = 1; // expected-warning{{Out of bound memory access}}
}

// Tests doing an out-of-bounds access before the start of an array using:
// - indirect pointer to buffer
// - constant integer index
// - constant integer size for buffer
void test2_ptr(int x) {
  int *buf = new int[100];
  int *p = buf;
  p[-1] = 1; // expected-warning{{Out of bound memory access}}
}

// Tests doing an out-of-bounds access before the start of an array using:
// - indirect pointer to buffer, manipulated using simple pointer arithmetic
// - constant integer index
// - constant integer size for buffer
void test2_ptr_arith(int x) {
  int *buf = new int[100];
  int *p = buf;
  --p;
  p[0] = 1; // expected-warning {{Out of bound memory access (accessed memory precedes memory block)}}
}

// Tests under-indexing
// of a multi-dimensional array
void test2_multi(int x) {
  auto buf = new int[100][100];
  buf[0][-1] = 1; // expected-warning{{Out of bound memory access}}
}

// Tests under-indexing
// of a multi-dimensional array
void test2_multi_b(int x) {
  auto buf = new int[100][100];
  buf[-1][0] = 1; // expected-warning{{Out of bound memory access}}
}

// Tests over-indexing
// of a multi-dimensional array
void test2_multi_c(int x) {
  auto buf = new int[100][100];
  buf[100][0] = 1; // expected-warning{{Out of bound memory access}}
}

// Tests over-indexing
// of a multi-dimensional array
void test2_multi_2(int x) {
  auto buf = new int[100][100];
  buf[99][100] = 1; // expected-warning{{Out of bound memory access}}
}

// Tests normal access of
// a multi-dimensional array
void test2_multi_ok(int x, int y, int z) {
  auto buf = new int[100][100];
  buf[0][0] = 1;  // no-warning

  // The single symbol gets the constraint.
  buf[x][99] = 1; // no-warning
  clang_analyzer_eval(0 <= x && x <= 99); // expected-warning{{TRUE}}

  // The lexically present complex symbol gets the constraint:
  buf[x+y][99] = 1; // no-warning
  clang_analyzer_eval(0 <= x+y && x+y <= 99); // expected-warning{{TRUE}}

  // The lexically equivalent pair of the expression is still unknown.
  // We are not smart enough to deal with such cases...
  clang_analyzer_eval(0 <= y+x); // expected-warning{{UNKNOWN}}
}

// Tests over-indexing using different types
// array
void test_diff_types() {
  int *buf = new int[10]; //10*sizeof(int) Bytes allocated
  char *cptr = (char *)buf;
  cptr[sizeof(int) * 9] = 1;  // no-warning
  cptr[sizeof(int) * 10] = 1; // expected-warning{{Out of bound memory access}}
}

// Tests over-indexing
//if the allocated area is non-array
void test_non_array() {
  int *ip = new int;
  ip[0] = 1; // no-warning
  ip[1] = 2; // expected-warning{{Out of bound memory access}}
}

//Tests over-indexing
//if the allocated area size is a runtime parameter
void test_dynamic_size(int s) {
  int *buf = new int[s];
  buf[0] = 1;     // no-warning
  buf[s - 1] = 1; // no-warning
  buf[s] = 1;     // no-warning
}

void test_dynamic_size2(unsigned s) {
  int *buf = new int[s];
  buf[0] = 1;     // no-warning
  buf[s - 1] = 1; // no-warning
  buf[s] = 1;     // no-warning
}

//Tests complex arithmetic
//in new expression, signed
void test_dynamic_size3(int m, int n, int z){
  // The extent is symbolic, thus we check the lower bound...
  unsigned *U = new unsigned[m + 1];
  U[99999] = 1; // no-warning
  U[n] = 1;     // no-warning
  // We don't constrain the acces with the symbolic access.
  // FIXME: With some smart, we could cover this case as well.
  clang_analyzer_eval(n <= m + 1); // expected-warning{{UNKNOWN}}

  // Constrain z to be negative.
  if (z >= 0)
    return;

  // We know that we access an element at a negative index.
  U[z] = 1; // expected-warning {{Out of bound memory access (accessed memory precedes memory block)}}
}

// TODO: Add tests covering VLAs, mutlidim VLAs, partially known VLAs.
// TODO: Add tests showing that the given symbolic expression gets the proper constraints.
// TODO: Add test using taint.

