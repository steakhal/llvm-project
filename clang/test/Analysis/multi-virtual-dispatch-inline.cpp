// DEFINE: %{run_csa} = %clang_analyze_cc1 %s \
// DEFINE:   -analyzer-checker=core,debug.ExprInspection \
// DEFINE:   -analyzer-config eagerly-assume=false

// RUN: %{run_csa} -DPURE_VIRTUAL -verify=common
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

namespace calling_pure_methods {
  struct Base {
    virtual ~Base() = default;
    virtual const char *baseFn() const = 0;
  };
  struct Derived1 : Base {
    virtual const char *derived1Fn() const = 0;
  };
  struct Derived2 : Base {
    const char *baseFn() const override { return "Derived2"; }
  };
  void entry_point(Derived1 *p) {
    // Dynamic type is unknown for "p", because it's abstract, and we see no
    // classes inheriting from it. But we know it can't be "Base" or "Derived2".
    clang_analyzer_dump(p->baseFn());
    // common-warning@-1 {{Derived2}} // FIXME: On a "Derived1" object we can never call a "Derived2" method - assuming we see the whole inheritance graph.
    // common-warning@-2 {{conj_}}

    clang_analyzer_dump(p->derived1Fn()); // no-crash
    // common-warning@-1 {{conj_}}
  }
} // namespace calling_pure_methods

namespace defined_in_dependent_context {
struct Base {
  virtual ~Base() = default;
  virtual const char *name() const { return "Base"; }
};
template <typename T = int> struct not_instantiated_dependent_context {
  struct NotActuallyDependent final : Base {
    const char *name() const override { return "NotActuallyDependent"; }
  };
};
void entry_point(Base *p) {
  // The "NotActuallyDependent" class is a dependent context because it's declared inside a dependent context.
  // However, it does not actually depend on the template parameter, because it doesn't use it.
  // In theory, we could somehow inline the method here on one day.
  clang_analyzer_dump(p->name());
  // common-warning@-1 {{Base}}
  // common-warning@-2 {{conj_}}
}
} // namespace defined_in_dependent_context

namespace inherit_from_dependent_context {
struct Base {
  virtual ~Base() = default;
  virtual const char *name() const { return "Base"; }
};
template <typename T = int> struct not_instantiated_dependent_context {
  struct ReallyDependent final : Base, T {
    const char *name() const override { return "ReallyDependent"; }
  };
};
void entry_point(Base *p) {
  // The "ReallyDependent" class is not instantiated.
  // In the analyzer, we assume methods we inline have complete type and have a layout.
  // Otherwise we would just crash, so we must ignore incomplete/dependent classes.
  clang_analyzer_dump(p->name());
  // common-warning@-1 {{Base}}
  // common-warning@-2 {{conj_}}
}
} // namespace inherit_from_dependent_context

namespace placement_new {
using size_t = decltype(sizeof(int));
struct Base {
  virtual ~Base() = default;
  virtual const char *name() const { return "Base"; }
};
struct Derived final : Base {
  const char *name() const override { return "Derived"; }
  void* operator new(size_t size, void* ptr) noexcept; // placement new operator
};

// We know the accurate dynamic type, so we have a single path.
void entry_point() {
  alignas(64) unsigned char stack[1000];
  auto *p = new (stack) Derived();
  clang_analyzer_dump(p->name()); // common-warning {{Derived}}

  unsigned char *heap = new unsigned char[1000];
  auto *q = new (heap) Derived();
  clang_analyzer_dump(q->name()); // common-warning {{Derived}}
  delete[] heap;
}
} // namespace placement_new
