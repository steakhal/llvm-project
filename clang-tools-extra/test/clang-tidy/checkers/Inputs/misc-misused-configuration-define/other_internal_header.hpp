#ifndef OTHER_INTERNAL_HEADER_HPP
#define OTHER_INTERNAL_HEADER_HPP

#define OTHER_LIBRARY_NO_EXCEPTIONS foo bar
#include "3pp/other_library.hpp"

extern "C" int printf(const char *format, ...);

void handleOtherException(int ex) {
  printf("Custom other exception handler! code: %d\n", ex);
}

void doOtherStuff() {
  printf("Doing other stuff!\n");
  printf("3^16  = %f\n", pow3(16));
  printf("3^100 = %f\n", pow3(100));
}

#endif // OTHER_INTERNAL_HEADER_HPP
