// RUN: rm -rf %t && mkdir %t
// RUN: split-file %s %t

// RUN: %clang_cc1 -triple x86_64-pc-linux-gnu -std=c++17 \
// RUN:   -emit-pch -o "%t/remote.cpp.ast" "%t/remote.cpp" -fanalyze-module

//  Make sure the extension blocks are actually there.
//  RUN: llvm-bcanalyzer "%t/remote.cpp.ast" | FileCheck -check-prefix=CHECK-BCANALYZER %s
//  CHECK-BCANALYZER: {{Block ID.*EXTENSION_BLOCK}}
//  CHECK-BCANALYZER: {{100.00.*EXTENSION_METADATA}}

// RUN: cd "%t" && %clang_extdef_map "%t/remote.cpp.ast" 2>/dev/null > externalDefMap.txt

// RUN: %clang_cc1 -triple x86_64-pc-linux-gnu -std=c++17 -analyze \
// RUN:   -analyzer-checker=core,debug.ExprInspection \
// RUN:   -analyzer-config experimental-enable-naive-ctu-analysis=true \
// RUN:   -analyzer-config ctu-dir=%t \
// RUN:   -verify %t/main.cpp -dump-deserialized-decls

// I temporarily pause the development of the AnalisisExtension, so this test doesn't make sense for now.
// UNSUPPORTED: true

//--- main.cpp
void clang_analyzer_dump(int);
int remote();

void top() {
  int v = remote();
  clang_analyzer_dump(v); // expected-warning {{22 S32b}}
}

//--- remote.cpp
struct Base {
  virtual ~Base() = default;
};
struct Derived : Base {
  virtual void foo() {
    if (true) {}
  }
};

int remote() {
  return 22;
}
