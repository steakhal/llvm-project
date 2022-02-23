// RUN: %clang_analyze_cc1 -triple i686-unknown-linux -std=c++17 \
// RUN:   -analyzer-checker=core,alpha.security.ArrayBoundV2,debug.ExprInspection \
// RUN:   -analyzer-config eagerly-assume=false -verify %s

void clang_analyzer_eval(int);
void clang_analyzer_printState();
template <typename T> void clang_analyzer_dump(T);


template <typename T, typename U> struct is_same {
  static constexpr bool value = false;
};
template <typename T> struct is_same<T, T> {
  static constexpr bool value = true;
};

template <typename T> constexpr bool is_unsigned() {
  return T(0) < T(-1);
};

template <typename T, int N> constexpr int array_size(const T (&)[N]) {
  return N;
}

/// Expression tree
struct sym {};
template <auto Value> struct cons {};
template <typename Left, typename Right> struct plus {};
template <typename Left, typename Right> struct minus {};
template <typename Left, typename Right> struct mul {};
template <typename Left, typename Right> struct div {};

/// Count syms.
template <typename ExpressionTree> struct count_syms;
template <> struct count_syms<sym> {
  static constexpr unsigned value = 1;
};
template <auto Value> struct count_syms<cons<Value>> {
  static constexpr unsigned value = 0;
};
template <typename L, typename R> struct count_syms<plus<L, R>> {
  static constexpr unsigned value = count_syms<L>::value + count_syms<R>::value;
};
template <typename L, typename R> struct count_syms<minus<L, R>> {
  static constexpr unsigned value = count_syms<L>::value + count_syms<R>::value;
};
template <typename L, typename R> struct count_syms<mul<L, R>> {
  static constexpr unsigned value = count_syms<L>::value + count_syms<R>::value;
};
template <typename L, typename R> struct count_syms<div<L, R>> {
  static constexpr unsigned value = count_syms<L>::value + count_syms<R>::value;
};

/// Expression tree evaluator
template <typename ExpressionTree> struct eval;
template <> struct eval<sym> {
  template <typename T> static constexpr auto f(T x) { return x; }
};
template <auto Value> struct eval<cons<Value>> {
  template <typename T> static constexpr auto f(T x) { return Value; }
};
template <typename L, typename R> struct eval<plus<L, R>> {
  static_assert(count_syms<L>::value + count_syms<R>::value <= 1, "Single symbolic expression allowed currently.");
  template <typename T> static constexpr auto f(T x) { return eval<L>::f(x) + eval<R>::f(x); }
};
template <typename L, typename R> struct eval<minus<L, R>> {
  static_assert(count_syms<L>::value + count_syms<R>::value <= 1, "Single symbolic expression allowed currently.");
  template <typename T> static constexpr auto f(T x) { return eval<L>::f(x) - eval<R>::f(x); }
};
template <typename L, typename R> struct eval<mul<L, R>> {
  static_assert(count_syms<L>::value + count_syms<R>::value <= 1, "Single symbolic expression allowed currently.");
  template <typename T> static constexpr auto f(T x) { return eval<L>::f(x) * eval<R>::f(x); }
};
template <typename L, typename R> struct eval<div<L, R>> {
  static_assert(count_syms<L>::value + count_syms<R>::value <= 1, "Single symbolic expression allowed currently.");
  template <typename T> static constexpr auto f(T x) { return eval<L>::f(x) / eval<R>::f(x); }
};

void test_expression_tree_evaluator(int x) {
  // The analyzer reorders the expression to have the symbol on the left.
  clang_analyzer_dump(eval<mul<plus<sym, cons<1>>, cons<3>>>::f(x));
  clang_analyzer_dump(eval<mul<plus<cons<1>, sym>, cons<3>>>::f(x));
  clang_analyzer_dump(eval<mul<cons<3>, plus<cons<1>, sym>>>::f(x));
  // expected-warning-re@-3 {{((reg_${{[0-9]+}}<int x>) + 1) * 3}}
  // expected-warning-re@-3 {{((reg_${{[0-9]+}}<int x>) + 1) * 3}}
  // expected-warning-re@-3 {{((reg_${{[0-9]+}}<int x>) + 1) * 3}}

  // The analyzer constant folds the expression.
  clang_analyzer_dump(eval<plus<cons<3>, plus<cons<1>, sym>>>::f(x));
  // expected-warning-re@-1 {{(reg_${{[0-9]+}}<int x>) + 4}}
  clang_analyzer_dump(eval<plus<mul<cons<3>, cons<2>>, sym>>::f(x));
  // expected-warning-re@-1 {{(reg_${{[0-9]+}}<int x>) + 6}}

  // Can not constant fold everything.
  clang_analyzer_dump(eval<mul<cons<3>, plus<cons<2>, sym>>>::f(x));
  // expected-warning-re@-1 {{((reg_${{[0-9]+}}<int x>) + 2) * 3}}
}

template <auto Lowerbound, auto UpperBound>
struct range {
  static_assert(is_same<decltype(Lowerbound), decltype(UpperBound)>::value);
  static_assert(Lowerbound <= UpperBound);
  static constexpr auto lowerbound = Lowerbound;
  static constexpr auto upperbound = UpperBound;
};

template <typename SubscriptExpression, typename ExpectedRange, typename ArrayT, typename SymbolType>
void valid(SymbolType x) {
  static_assert(ExpectedRange::upperbound - ExpectedRange::lowerbound > 1, "Ranges require at least 3 elements for automatic testing.");

  // Using constexpr to prevent this from compilation if underflow/overflow UB happens.
  constexpr auto after_lowerbound = ExpectedRange::lowerbound + 1;
  constexpr auto before_upperbound = ExpectedRange::upperbound - 1;

  // Just to make sure that the test is relevant, aka. no overflow/underflow happened during the calculation.
  static_assert(ExpectedRange::lowerbound < after_lowerbound, "The expected lowerbound is too large for this test.");
  static_assert(before_upperbound < ExpectedRange::upperbound, "The expected upperbound is too small for this test.");

  // Test-code:
  constexpr ArrayT buf = {};

  // Sanity check if lowerbound and upperbound are valid elements of the array.
  // We utilize the constexpr engine of clang to check the given access.
  static_assert(1 + buf[eval<SubscriptExpression>::f(ExpectedRange::lowerbound)]);
  static_assert(1 + buf[eval<SubscriptExpression>::f(ExpectedRange::upperbound)]);
  if constexpr (is_unsigned<SymbolType>()) {
    static_assert(0 <= ExpectedRange::lowerbound);
  }

  // Evaluate the expression tree with the symbolic variable.
  const auto subscipt_expr = eval<SubscriptExpression>::f(x);
  // clang_analyzer_dump(subscipt_expr);

  // Access the element at that index.
  const auto tmp = buf[subscipt_expr]; // no-warning
  //clang_analyzer_printState();

  // Verify that we infered that the symbolic variable must be in the [lowerbound, upperbound] range to be valid.
  clang_analyzer_eval(ExpectedRange::lowerbound <= x); // expected-warning {{TRUE}}
  clang_analyzer_eval(x <= ExpectedRange::upperbound); // expected-warning {{TRUE}}

  // Verify that the ends of the range is sharp.
  clang_analyzer_eval(after_lowerbound <= x); // expected-warning {{UNKNOWN}}
  clang_analyzer_eval(x < before_upperbound); // expected-warning {{UNKNOWN}}
}

// Valid array access test-cases:
// Each row is an explicit template function instantiation of the given test-case.

//------- buf[x+1] ------
//  0 <= x + 1 < 5    [ sub 1 ]
// -1 <= x < 4
// x in [-1, 3]
using x_plus_1 = plus<sym, cons<1>>;
template void valid<x_plus_1, range<-1, 3>, int[5]>(char x);
template void valid<x_plus_1, range<-1, 3>, int[5]>(signed char x);
template void valid<x_plus_1, range<-1, 3>, int[5]>(short x);
template void valid<x_plus_1, range<-1, 3>, int[5]>(int x);
template void valid<x_plus_1, range<-1, 3>, int[5]>(long x);
template void valid<x_plus_1, range<-1, 3>, int[5]>(long long x);
template void valid<x_plus_1, range<0, 3>, int[5]>(unsigned char x);
template void valid<x_plus_1, range<0, 3>, int[5]>(unsigned short x);
template void valid<x_plus_1, range<0, 3>, int[5]>(unsigned int x);
template void valid<x_plus_1, range<0, 3>, int[5]>(unsigned long x);
template void valid<x_plus_1, range<0, 3>, int[5]>(unsigned long long x);

//------- buf[x-1] ------
// 0 <= x - 1 < 5    [ add 1 ]
// 1 <= x < 6
// x in [1, 5]
using x_minus_1 = minus<sym, cons<1>>;
template void valid<x_minus_1, range<1, 5>, int[5]>(char x);
template void valid<x_minus_1, range<1, 5>, int[5]>(signed char x);
template void valid<x_minus_1, range<1, 5>, int[5]>(short x);
template void valid<x_minus_1, range<1, 5>, int[5]>(int x);
template void valid<x_minus_1, range<1, 5>, int[5]>(long x);
template void valid<x_minus_1, range<1, 5>, int[5]>(long long x);
template void valid<x_minus_1, range<1, 5>, int[5]>(unsigned char x);
template void valid<x_minus_1, range<1, 5>, int[5]>(unsigned short x);
template void valid<x_minus_1, range<1, 5>, int[5]>(unsigned int x);
template void valid<x_minus_1, range<1, 5>, int[5]>(unsigned long x);
template void valid<x_minus_1, range<1, 5>, int[5]>(unsigned long long x);

//------- buf[x+1ull] ------
//  0 <= x + 1ull < 5    [ sub 1 ]
// -1 <= x < 4
// x in [-1, 3]
using x_plus_1ull = plus<sym, cons<1ull>>;
template void valid<x_plus_1ull, range<-1, 3>, int[5]>(char x);
template void valid<x_plus_1ull, range<-1, 3>, int[5]>(signed char x);
template void valid<x_plus_1ull, range<-1, 3>, int[5]>(short x);
template void valid<x_plus_1ull, range<-1, 3>, int[5]>(int x);
template void valid<x_plus_1ull, range<-1, 3>, int[5]>(long x);
template void valid<x_plus_1ull, range<-1, 3>, int[5]>(long long x);
template void valid<x_plus_1ull, range<0, 3>, int[5]>(unsigned char x);
template void valid<x_plus_1ull, range<0, 3>, int[5]>(unsigned short x);
template void valid<x_plus_1ull, range<0, 3>, int[5]>(unsigned int x);
template void valid<x_plus_1ull, range<0, 3>, int[5]>(unsigned long x);
template void valid<x_plus_1ull, range<0, 3>, int[5]>(unsigned long long x);

//------- buf[x-1ull] ------
// 0 <= x - 1ull < 5    [ add 1 ]
// 1 <= x < 6
// x in [1, 5]
using x_minus_1ull = minus<sym, cons<1ull>>;
template void valid<x_minus_1ull, range<1, 5>, int[5]>(char x);
template void valid<x_minus_1ull, range<1, 5>, int[5]>(signed char x);
template void valid<x_minus_1ull, range<1, 5>, int[5]>(short x);
template void valid<x_minus_1ull, range<1, 5>, int[5]>(int x);
template void valid<x_minus_1ull, range<1, 5>, int[5]>(long x);
template void valid<x_minus_1ull, range<1, 5>, int[5]>(long long x);
template void valid<x_minus_1ull, range<1, 5>, int[5]>(unsigned char x);
template void valid<x_minus_1ull, range<1, 5>, int[5]>(unsigned short x);
template void valid<x_minus_1ull, range<1, 5>, int[5]>(unsigned int x);
template void valid<x_minus_1ull, range<1, 5>, int[5]>(unsigned long x);
template void valid<x_minus_1ull, range<1, 5>, int[5]>(unsigned long long x);

//------- buf[2x+1] ------
//  0 <= 2x + 1 < 9    [ sub 1 ]
// -1 <= 2x < 8        [ div 2 ]
// -0.5 <= x < 4
// x in [0, 3]
using x2_plus_1 = plus<mul<sym, cons<2>>, cons<1>>;
template void valid<x2_plus_1, range<0, 3>, int[9]>(char x);
template void valid<x2_plus_1, range<0, 3>, int[9]>(signed char x);
template void valid<x2_plus_1, range<0, 3>, int[9]>(short x);
template void valid<x2_plus_1, range<0, 3>, int[9]>(int x);
template void valid<x2_plus_1, range<0, 3>, int[9]>(long x);
template void valid<x2_plus_1, range<0, 3>, int[9]>(long long x);
template void valid<x2_plus_1, range<0, 3>, int[9]>(unsigned char x);
template void valid<x2_plus_1, range<0, 3>, int[9]>(unsigned short x);
template void valid<x2_plus_1, range<0, 3>, int[9]>(unsigned int x);
template void valid<x2_plus_1, range<0, 3>, int[9]>(unsigned long x);
template void valid<x2_plus_1, range<0, 3>, int[9]>(unsigned long long x);

//------- buf[2x-4ull] ------
// 0 <= 2x - 4ull < 9    [ add 4 ]
// 4 <= 2x < 13          [ div 2 ]
// 2 <= x < 6.5
// x in [2, 6]
using x2_minus_1ull = minus<mul<sym, cons<2>>, cons<4ull>>;
template void valid<x2_minus_1ull, range<2, 6>, int[9]>(char x);
template void valid<x2_minus_1ull, range<2, 6>, int[9]>(signed char x);
template void valid<x2_minus_1ull, range<2, 6>, int[9]>(short x);
template void valid<x2_minus_1ull, range<2, 6>, int[9]>(int x);
template void valid<x2_minus_1ull, range<2, 6>, int[9]>(long x);
template void valid<x2_minus_1ull, range<2, 6>, int[9]>(long long x);
template void valid<x2_minus_1ull, range<2, 6>, int[9]>(unsigned char x);
template void valid<x2_minus_1ull, range<2, 6>, int[9]>(unsigned short x);
template void valid<x2_minus_1ull, range<2, 6>, int[9]>(unsigned int x);
template void valid<x2_minus_1ull, range<2, 6>, int[9]>(unsigned long x);
template void valid<x2_minus_1ull, range<2, 6>, int[9]>(unsigned long long x);

//------- buf[2-4ull] ------
// 0 <= 2ulx - 4ull < 9    [ add 4 ]
// 4 <= 2ulx < 13          [ div 2ul ]
// 2 <= x < 6.5
// x in [2, 6]
using x2ul_minus_1ull = minus<mul<sym, cons<2ul>>, cons<4ull>>;
template void valid<x2ul_minus_1ull, range<2, 6>, int[9]>(char x);
template void valid<x2ul_minus_1ull, range<2, 6>, int[9]>(signed char x);
template void valid<x2ul_minus_1ull, range<2, 6>, int[9]>(short x);
template void valid<x2ul_minus_1ull, range<2, 6>, int[9]>(int x);
template void valid<x2ul_minus_1ull, range<2, 6>, int[9]>(long x);
template void valid<x2ul_minus_1ull, range<2, 6>, int[9]>(long long x);
template void valid<x2ul_minus_1ull, range<2, 6>, int[9]>(unsigned char x);
template void valid<x2ul_minus_1ull, range<2, 6>, int[9]>(unsigned short x);
template void valid<x2ul_minus_1ull, range<2, 6>, int[9]>(unsigned int x);
template void valid<x2ul_minus_1ull, range<2, 6>, int[9]>(unsigned long x);
template void valid<x2ul_minus_1ull, range<2, 6>, int[9]>(unsigned long long x);

//------- buf[x+-3] ------
// 0 <= x + -3 < 9    [ add 3 ]
// 3 <= x < 12
// x in [3, 11]
using x_plus_minus_3 = plus<sym, cons<-3>>;
template void valid<x_plus_minus_3, range<3, 11>, int[9]>(char x);
template void valid<x_plus_minus_3, range<3, 11>, int[9]>(signed char x);
template void valid<x_plus_minus_3, range<3, 11>, int[9]>(short x);
template void valid<x_plus_minus_3, range<3, 11>, int[9]>(int x);
template void valid<x_plus_minus_3, range<3, 11>, int[9]>(long x);
template void valid<x_plus_minus_3, range<3, 11>, int[9]>(long long x);
template void valid<x_plus_minus_3, range<3, 11>, int[9]>(unsigned char x);
template void valid<x_plus_minus_3, range<3, 11>, int[9]>(unsigned short x);
// The uint, ulong, ulonglong, needs spacial attention:
// Integer promotion kicks in and the unary negated value wraps around.
//
// 0 <= x + -3 < 9                    [ equivalent, due to integer promotion ]
// 0 <= x + 4294967293 < 9            [ subtract 4294967293 aka. unsigned(-3) ]
// While rearranging, we will assume that the expression 'x + 4294967293' did not overflow,
// thus x must be in [0, 2] at this point.
//
// -4294967293 (33 bits) <= x < -4294967284 (33 bits)
// There is no possible x value to be in the given range, without considering wrapping,
// thus the simplification fails.
// The same happens for ulong and ulonglong as well.


//------- buf[x- -6] ------
// 0 <= x - -6 < 9
// 0 <= x - -6 < 9
// Which is the same as:
// 0 <= x + 6 < 9   [ sub 6 ]
// -6 <= x < 3
// x in [-6, 2]
using x_minus_minus_6 = minus<sym, cons<-6>>;
template void valid<x_minus_minus_6, range<-6, 2>, int[9]>(char x);
template void valid<x_minus_minus_6, range<-6, 2>, int[9]>(signed char x);
template void valid<x_minus_minus_6, range<-6, 2>, int[9]>(short x);
template void valid<x_minus_minus_6, range<-6, 2>, int[9]>(int x);
template void valid<x_minus_minus_6, range<-6, 2>, int[9]>(long x);
template void valid<x_minus_minus_6, range<-6, 2>, int[9]>(long long x);
template void valid<x_minus_minus_6, range<0, 2>, int[9]>(unsigned char x);
template void valid<x_minus_minus_6, range<0, 2>, int[9]>(unsigned short x);
// The uint, ulong, ulonglong, needs spacial attention:
// Integer promotion kicks in and the unary negated value wraps around.
//
// 0 <= x - -6 < 9                    [ equivalent, due to integer promotion ]
// 0 <= x - 4294967290 < 9            [ add 4294967290 aka. unsigned(-6) ]
// While rearranging, we will assume that the expression 'x - 4294967290' did not underflow,
// thus x must be in [4294967290, 4294967295] at this point.
//
// 4294967290 (33 bits) <= x < 4294967299 (33 bits)
// x in [0xfffffffa, 0xffffffff]
// The same happens for ulong and ulonglong as well, but with their bit width.
template void valid<x_minus_minus_6, range<0xfffffffa, 0xffffffff>, int[9]>(unsigned int x);
template void valid<x_minus_minus_6, range<0xfffffffa, 0xffffffff>, int[9]>(unsigned long x);
template void valid<x_minus_minus_6, range<0xfffffffffffffffa, 0xffffffffffffffff>, int[9]>(unsigned long long x);

