// RUN: %clang_analyze_cc1 -analyzer-checker=core,debug.ExprInspection \
// RUN:    -analyzer-config eagerly-assume=false -verify %s
//
// expected-no-diagnostics

void clang_analyzer_warnIfReached();

void top(int *p) {
  if (&*(p + 1))
    return;

  // Dead code! The address of an object can never be null.
  clang_analyzer_warnIfReached(); // no-warning
}
