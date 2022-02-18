// RUN: %clang_analyze_cc1 -analyzer-checker=core \
// RUN:   -analyzer-checker=debug.ExprInspection  \
// RUN:   -analyzer-config eagerly-assume=false   \
// RUN:   %s -verify

#define assert(expr) \
  if (!(expr))       \
    return;

template <class T> void clang_analyzer_dump(T) {}
void clang_analyzer_warnIfReached() {}
void clang_analyzer_printState() {}

int global;

template <class... Ts> void invalidate(Ts &...); // no-definition!
/*
void global_variable() {
  clang_analyzer_dump(global);

  assert(global == 44);

  // constraints:
  // reg_$0<int global> -> [44, 44]

  // clang_analyzer_printState();

  invalidate();
  // invalidation assigns:
  //   $1 to GlobalInternalSpace
  //   $2 to GlobalSystemSpace

  // constraints:
  // reg_$0<int global> -> [44, 44]

  // clang_analyzer_printState();

  assert(global != 44);

  // constraints:
  // derived{conj_$1,global} -> [...,43] U [45,...]
  // reg<int global>         -> [44, 44]

  clang_analyzer_warnIfReached();
}
*/
/*
void dont_suppress_direct_escapes(int x) {
  clang_analyzer_dump(x); // expected-warning-re {{reg_${{[0-9]+}}<int x>}}
  if (x != 44)
    return;

  // constraints:
  // reg<int x> -> [44, 44]

  invalidate(x);          // direct-escape
  clang_analyzer_dump(x); // expected-warning-re {{conj_${{[0-9]+}}{int, LC1, S{{[0-9]+}}, #{{[0-9]+}}}}}

  if (x == 44)
    return;

  // constraints:
  // conj -> [...,43] U [45,...]
  // reg  -> [44, 44]

  // Warning below is not suppressed, since `x` was directly invalidated.
  clang_analyzer_warnIfReached(); // expected-warning {{REACHABLE}}
  (void)x;
}*/

struct Message {
  int *payload;
};
void indirect_escape(int x) {
  clang_analyzer_dump(x); // expected-warning-re {{reg_${{[0-9]+}}<int x>}}
  if (x != 44)
    return;

  // constraints:
  // reg<int x> -> [44, 44]

  Message msg = {&x};
  invalidate(msg); // indirect-escape

  clang_analyzer_dump(x); // expected-warning-re {{conj_${{[0-9]+}}{int, LC1, S{{[0-9]+}}, #{{[0-9]+}}}}}

  if (x == 44)
    return;

  clang_analyzer_warnIfReached(); // expected-warning {{REACHABLE}}
  (void)x;
}
