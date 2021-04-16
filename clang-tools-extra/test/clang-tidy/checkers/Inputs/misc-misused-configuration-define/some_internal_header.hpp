#ifndef SOME_INTERNAL_HEADER_HPP
#define SOME_INTERNAL_HEADER_HPP

#define SOME_LIBRARY_NO_EXCEPTIONS
#include "3pp/some_library.hpp"

extern "C" int printf(const char *format, ...);

void handleSomeException(int ex) {
  printf("Custom exception handler! code: %d\n", ex);
}

void doSomeStuff() {
  printf("Doing some stuff!\n");
  printf("2^16  = %f\n", pow2(16));
  printf("2^100 = %f\n", pow2(100));
}

#endif // SOME_INTERNAL_HEADER_HPP
