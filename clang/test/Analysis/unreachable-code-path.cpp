// RUN: %clang_analyze_cc1 -verify %s \
// RUN:   -analyzer-checker=core,deadcode,alpha.deadcode

// expected-no-diagnostics

struct NonTrivial {
  ~NonTrivial();
};
struct NonTrivialPair {
  NonTrivial a, b;
};
enum Kind { Kind1 };

// This code creates a null CFGBlock in the unoptimized CFG.
// Should not crash.
void NullCFGBlock(enum Kind k) {
  { // Block start
    NonTrivialPair a;
  }
  switch (k) {
  case Kind1:;
  }
}
