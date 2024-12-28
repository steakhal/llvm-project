// DEFINE: %{run_csa} = %clang_analyze_cc1 %s \
// DEFINE:   -analyzer-checker=core,debug.ExprInspection \
// DEFINE:   -analyzer-config eagerly-assume=false

// RUN: %{run_csa} -DPURE_VIRTUAL -verify=common,pure
// RUN: %{run_csa} -UPURE_VIRTUAL -verify=common,ret-zero

void clang_analyzer_warnIfReached();
template <class T> void clang_analyzer_dump(T);
template <class T> void clang_analyzer_sinkIfSValIs(T value, const char *regex);

namespace eval_example {
struct Op {
  virtual ~Op() = default;
#ifdef PURE_VIRTUAL
  virtual int eval(int x, int y) const = 0; // Pure virtual!
#else
  virtual int eval(int x, int y) const { return 0; };
#endif
};
struct Add final : Op { int eval(int x, int y) const override { return x + y; } };
struct Sub final : Op { int eval(int x, int y) const override { return x - y; } };
struct Mul final : Op { int eval(int x, int y) const override { return x * y; } };
struct Div final : Op { int eval(int x, int y) const override { return x / y; } };

void entry_point(const Op &op) {
  // We don't know anything about the dynamic type yet.
  int res1 = op.eval(10, 2);
  clang_analyzer_dump(res1); // #dump1
  // ret-zero-warning-re@#dump1 {{{{^0 S32b}}}}  "default" impl for non-pure virtual
  // common-warning-re@#dump1   {{{{^12 S32b}}}} "Add" impl
  // common-warning-re@#dump1   {{{{^20 S32b}}}} "Mul" impl
  // common-warning-re@#dump1   {{{{^5 S32b}}}}  "Div" impl
  // common-warning-re@#dump1   {{{{^8 S32b}}}}  "Sub" impl
  // common-warning-re@#dump1   {{{{^conj_}}}}   conservative evaluation

  // If we speculated a concrete dynamic type, then we should have the exact same output now.
  int res2 = op.eval(100, 20);
  clang_analyzer_dump(res2); // #dump2
  // ret-zero-warning-re@#dump2 {{{{^0 S32b}}}}    "default" impl for non-pure virtual
  // common-warning-re@#dump2   {{{{^120 S32b}}}}  "Add" impl
  // common-warning-re@#dump2   {{{{^2000 S32b}}}} "Mul" impl
  // common-warning-re@#dump2   {{{{^5 S32b}}}}    "Div" impl
  // common-warning-re@#dump2   {{{{^80 S32b}}}}   "Sub" impl
  // common-warning-re@#dump2   {{{{^conj_}}}}     conservative evaluation

  // Sink the conservative eval path.
  clang_analyzer_sinkIfSValIs(res1, "^conj_"); // common-warning {{Path sunk}}

  // Let's see if we have consistent dynamic types across the two virtual calls.
  switch (res1) {
    case 0:  clang_analyzer_dump(res2); break; // "default" impl for non-pure virtual
    // ret-zero-warning-re@-1 {{{{^0 S32b}}}}
    case 5:  clang_analyzer_dump(res2); break; // "Div" impl
    // common-warning-re@-1 {{{{^5 S32b}}}}
    case 8:  clang_analyzer_dump(res2); break; // "Sub" impl
    // common-warning-re@-1 {{{{^80 S32b}}}}
    case 12: clang_analyzer_dump(res2); break; // "Add" impl
    // common-warning-re@-1 {{{{^120 S32b}}}}
    case 20: clang_analyzer_dump(res2); break; // "Mul" impl
    // common-warning-re@-1 {{{{^2000 S32b}}}}
    default: clang_analyzer_warnIfReached(); break; // no-warning: Dead code
    // We don't have a conservative eval path here, because after sinking that
    // path we are only left with paths where we exactly know the dynamic type,
    // thus we won't have a speculated path even for the second invocation of
    // the same virtual function.
  }
}

} // namespace eval_example
