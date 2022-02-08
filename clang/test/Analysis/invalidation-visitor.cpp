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

void toplevel_parameter(int x) {
  clang_analyzer_dump(x); // expected-warning-re {{reg_${{[0-9]+}}<int x>}}
  assert(x == 44);

  // constraints:
  // reg<int x> -> [44, 44]

  invalidate(x);
  // PostStmt<CallExpr> InvalidateRegionsWorker::VisitCluster() binds new conjured symbol to VarDecl.

  clang_analyzer_dump(x); // expected-warning-re {{conj_${{[0-9]+}}{int, LC1, S{{[0-9]+}}, #{{[0-9]+}}}}}
  assert(x != 44);

  // constraints:
  // conj -> [...,43] U [45,...]
  // reg  -> [44, 44]

  clang_analyzer_warnIfReached(); // expected-warning {{REACHABLE}}
  (void)x;
}
