// RUN: %clang_analyze_cc1 -analyzer-checker=core,alpha.deadcode.UnreachableCode,debug.ExprInspection %s -std=c++17 -verify

constexpr void clang_analyzer_warnIfReached() {}

constexpr int generateReport() {
  clang_analyzer_warnIfReached(); // expected-warning {{REACHABLE}}
  return 0;
}

constexpr int createBugreportWhichWillBesuppressed() {
  clang_analyzer_warnIfReached(); // no-warning: suppressed since the callsite is constexpr context
  return 0;
}

int TestNonConstexprVarDecl() {
  int x = generateReport(); // will have a warning
  return x;
}

int TestSingleVarDecl() {
  constexpr int x = createBugreportWhichWillBesuppressed(); // no-warning
  return x;
}

int TestMultipleVarDecl() {
  constexpr int y = createBugreportWhichWillBesuppressed(), // no-warning
                z = createBugreportWhichWillBesuppressed(); // no-warning
  return y + z;
}

int TestCommaExprAndLambdas() {
  // FIXME: For Eval::Call-ed functions the location context is the
  // callee's context, so there is no CallSite. The CallSite should
  // still refer to the DeclStmt.
  // expected-warning@+2 {{REACHABLE}}
  constexpr auto f = (
      clang_analyzer_warnIfReached(), // We should have no warning for this.
      [](){
        clang_analyzer_warnIfReached(); // Only evaluated later.
      }
    );

  // Now evaluate 'f' in constexpr context.
  constexpr int x = (f(), 1); // no-warning
  constexpr int y = [](){ clang_analyzer_warnIfReached(); return 1; }(); // no-warning
  return x + y;
}

constexpr int TestDeadCodeTopLevelFn() {
  if (false) {
    // Dead code!
    generateReport(); // expected-warning {{This statement is never executed}}
    return 42;
  }
  return 66;
}

constexpr int deadcode_callee(bool cond) {
  if (cond) {
    // Dead code!
    // FIXME: We shouldn't suppress deadcode warnings from constexpr contexts.
    createBugreportWhichWillBesuppressed(); // no-deadcode-warning: it would come from constexpr context
    return 42;
  }
  return 66;
}

int TestDeadCodeInCallee() {
  constexpr int x = deadcode_callee(/*cond=*/false); // no-warning
  return x;
}
