// RUN: %clang_cc1 -emit-pch -o %t %s
// RUN: %clang_analyze_cc1 -error-on-deserialized-decl S1_method -include-pch %t -analyzer-checker=core %s
// RUN: %clang_analyze_cc1 -include-pch %t -analyzer-checker=core -verify %s

// Unfortunately, due to multi virtual dispatch, we must eagerly traverse
// (and load) the whole translation unit.
// We could likely fix this in a couple of ways:
// 1) Serialize the TU into pch while adding a custom attribute to the
//    enclosing DeclContexts if the context declares a polymorphic CXXRecordDecl.
//    When parsing in the `HandleTopLevelDecl` callback check if the decl has
//    the attribute and if so, traverse that decl too.
// 2) Serialize the necessary data into the pch file that encodes the
//    inheritance hierarchy and the overridden methods.
//    This is more complicated than (1), but it would (maybe) work without
//    deserializing only the necessary CXXMethodDecls, and not the whole
//    CXXRecordDecl it corresponds to.
//
// Never mind, (1) wouldn't work because a single CXXRecordDecl would poison
// the whole TU anyways. Option (2) might work though.
// XFAIL: *

#ifndef HEADER
#define HEADER
// Header.

void S1_method(); // This should not be deserialized.


#else
// Using the header.

int test() {
  int x = 0;
  return 5/x; //expected-warning {{Division by zero}}
}

#endif
