#ifndef SOME_LIBRARY_HPP
#define SOME_LIBRARY_HPP

extern "C" double pow(double base, double exponent);

#ifdef SOME_LIBRARY_NO_EXCEPTIONS
// The implementation can NEVER return control to the caller.
void handleSomeException(int ex);
#endif

double pow2(unsigned exponent) {
  if (exponent > 60) {
    auto ex = -1;
#ifdef SOME_LIBRARY_NO_EXCEPTIONS
    handleSomeException(ex);
#else
    throw ex;
#endif
  }

  return pow(2, exponent);
}

#endif // SOME_LIBRARY_HPP
