// RUN: %clang_analyze_cc1 -w -triple i386-apple-darwin10 -analyzer-checker=core,debug.ExprInspection -verify %s

void clang_analyzer_eval(int);

struct S {
  int x, y;
  int z[2];
};

void testOffsets(struct S *s, int coin) {
  if (s != 0)
    return;

  // FIXME: Here we are testing the hack that computes offsets to null pointers
  // as 0 in order to find null dereferences of not-exactly-null pointers,
  // such as &(s->y) below, which is equal to 4 rather than 0 in run-time.

  // These are indeed null.
  clang_analyzer_eval(s == 0); // expected-warning{{TRUE}}
  clang_analyzer_eval(&(s->x) == 0); // expected-warning{{The left operand of '==' is a garbage value}}
}
