// RUN: %clang_cc1 -analyze -analyzer-checker=debug.DumpCFG -std=c++20 %s > %t 2>&1
// RUN: FileCheck --input-file=%t %s

// RUN: %clang_cc1 -analyze -analyzer-checker=core,debug.ExprInspection -std=c++20 %s \
// RUN:    -verify=constexpr-as-top-level

// RUN: %clang_cc1 -analyze -analyzer-checker=core,debug.ExprInspection -std=c++20 %s \
// RUN:    -verify -DHAS_CALLSITE_FOR_CONSTEXPR_CTOR

#ifdef HAS_CALLSITE_FOR_CONSTEXPR_CTOR
// If we have callsite for invoking the constexpr ctor/function, we should
// not analyze that constexpr function in top-level context.
// Consequently, we should know that in the given execution N is not equal
// to 100, thus there is no division by zero error.
//
// expected-no-diagnostics
#endif

struct Fib;
struct Fib {
  // n:   1 2 3 4 5 6  7 ...
  // fib: 1 1 2 3 5 8 13 ...
  constexpr Fib(int n) {
    if (n < 2) {
      result = 1;
      return;
    }
    Fib Prev(n - 2);
    Fib Curr(n - 1);
    result = Prev.result + Curr.result;
  }
  constexpr Fib(int n, int m) : result{42} {}
  char home = 'a';
  int result;
};

constexpr int incOrTrap(int N) {
  if (N == 100)
    return 42 / (N - 100);
  // constexpr-as-top-level-warning@-1 {{Division by zero}}
  return N + 1;
}

#ifdef HAS_CALLSITE_FOR_CONSTEXPR_CTOR
int constexpr_called_from_regular_context() {
  // We don't force constexpr context, consequently we emit initializer calls.
  const Fib F{incOrTrap(4)};
  return F.result;
}
#endif

int constexpr_called_from_constexpr_context() {
  constexpr Fib F{incOrTrap(3)};
  return F.result;
  // CHECK:       1: constexpr Fib F{incOrTrap(3)};
  // CHECK-NEXT:  2: F
}

template <typename T>
void clang_analyzer_dump(T);
void clang_analyzer_printState() {}
void asd() {
  int arr[10] = {0, incOrTrap(1), 1, incOrTrap(2)};
  clang_analyzer_printState();
  // clang_analyzer_dump(arr);
  (void)arr;
  (void)(arr[0] + arr[1] + arr[2] + arr[3]);
}

struct Fib;

void foobar() {
  // constexpr int a[] = {0, incOrTrap(1), 1, incOrTrap(2)};
  constexpr Fib V{3, 3};
}
