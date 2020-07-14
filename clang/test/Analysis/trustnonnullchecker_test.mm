// RUN: %clang_analyze_cc1 -fblocks -analyzer-checker=core,nullability,apiModeling -verify %s
// expected-no-diagnostics

#include "Inputs/system-header-simulator-for-nullability-cxx.h"

void blah() {
  foo(); // no-crash
}
