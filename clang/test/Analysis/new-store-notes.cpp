// RUN: %clang_analyze_cc1 -analyzer-output=text -verify %s \
// RUN:   -analyzer-checker=core,unix.Malloc,unix.cstring,debug.ExprInspection \
// RUN:   -analyzer-config core.CallAndMessage:ArgPointeeInitializedness=true \
// RUN:   -analyzer-config enable-new-store=true

void doStuff_pointerToConstInt(const int *u){}

void f_1_1(void) {
  int t;                          // expected-note {{'t' declared without an initial value}}
  int *tp1 = &t;                  // expected-note {{'tp1' initialized here}}
  // The following flow step depends on RegionStoreManagerV2::includedInBindings
  int *tp2 = tp1;                 // expected-note {{'tp2' initialized to the value of 'tp1'}}
  doStuff_pointerToConstInt(tp2); // expected-warning {{1st function call argument is a pointer to uninitialized value}}
                                  // expected-note@-1 {{1st function call argument is a pointer to uninitialized value}}
}
