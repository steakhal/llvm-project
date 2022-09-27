// llvm-project/build/reldeb/bin/clang -cc1 -analyze preprocessed.reproducer.cpp -analyzer-checker=core

// RUN: %clang_analyze_cc1 -analyzer-checker=core -verify %s
// expected-no-diagnostics


template <typename... Ts>
void escape(Ts&...);
struct Dummy {};

int strange(Dummy param) {
  Dummy local_pre_lambda;
  int ref_captured = 0;

  auto fn = [&] {
    escape(param, local_pre_lambda);
    return ref_captured; // no-warning: The value is not garbage.
  };

  int local_defined_after_lambda; // Unused, but necessary! Important that it's before the call.
  return fn();
}
