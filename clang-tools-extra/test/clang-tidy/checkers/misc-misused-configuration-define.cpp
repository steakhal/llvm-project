// RUN: %check_clang_tidy %s misc-misused-configuration-define %t -- \
// RUN:   -header-filter=.* -- -I%S/Inputs/misc-misused-configuration-define

// clang-format off
#include "3pp/some_library.hpp"
#include "some_internal_header.hpp"
#include "other_internal_header.hpp"
#include "3pp/other_library.hpp"
// clang-format on

// CHECK-NOTES: some_internal_header.hpp:4:9: warning: the macro being defined here was preveusly used in macro directive which might be unexpected [misc-misused-configuration-define]
// CHECK-NOTES: #define SOME_LIBRARY_NO_EXCEPTIONS
// CHECK-NOTES:         ^
// CHECK-NOTES: 3pp/some_library.hpp:6:2: note: first accessed here
// CHECK-NOTES: #ifdef SOME_LIBRARY_NO_EXCEPTIONS
// CHECK-NOTES:  ^

// OTHER_LIBRARY_NO_EXCEPTIONS is defined before any actual use of it,
// so we should not expect a warning about it.

int main() {
  try {
    doOtherStuff();
    doSomeStuff();
  } catch (int ex) {
    printf("error %d\n", ex);
  }
}
