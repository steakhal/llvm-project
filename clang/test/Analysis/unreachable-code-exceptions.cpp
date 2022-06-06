// RUN: %clang_analyze_cc1 -verify %s -fcxx-exceptions -fexceptions \
// RUN:   -analyzer-checker=core \
// RUN:   -analyzer-checker=alpha.deadcode.UnreachableCode

// expected-no-diagnostics

int test(int a) {
  try { // no-warning
    a *= 2;
  } catch (int) {
    return -1; // FIXME: We should mark this dead.
  }
  return a;
}
