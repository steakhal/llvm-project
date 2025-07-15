// RUN: rm -rf %t && mkdir %t
// RUN: split-file %s %t

// RUN: %analyzer-emit-ast -o "%t/derived.ast" "%t/derived.cpp"

// RUN: cd "%t" && %clang_extdef_map "%t/derived.ast" >> externalDefMap.txt
// RUN: %direct-overriders-merger.py %t %t/direct-overriders.txt

// RUN: %clang_analyze_cc1 -I %t \
// RUN:   -analyzer-checker=core,debug.ExprInspection \
// RUN:   -analyzer-config experimental-enable-naive-ctu-analysis=true \
// RUN:   -analyzer-config ctu-dir=%t \
// RUN:   -analyzer-config display-ctu-progress=true \
// RUN:   -analyzer-config direct-overriders-file=%t/direct-overriders.txt \
// RUN:   -verify %t/main.cpp

//--- base.h
struct Base {
  virtual ~Base() = default;
  virtual int num() = 0;
};
//--- derived.cpp
#include "base.h"
struct Derived final : Base {
  int num() override { return 22; }
};

//--- main.cpp
#include "base.h"
void clang_analyzer_dump(int);

void top(Base *p) {
  int v = p->num();
  clang_analyzer_dump(v);
  // expected-warning@-1 {{22 S32b}}  derived.cpp:Derived::num
  // expected-warning@-2 {{conj_}}    conservative case
}
