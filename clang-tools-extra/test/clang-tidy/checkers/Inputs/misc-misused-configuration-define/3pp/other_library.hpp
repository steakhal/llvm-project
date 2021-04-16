#ifndef OTHER_LIBRARY_HPP
#define OTHER_LIBRARY_HPP

extern "C" double pow(double base, double exponent);

#ifdef OTHER_LIBRARY_NO_EXCEPTIONS
// The implementation can NEVER return control to the caller.
void handleOtherException(int ex);
#endif

double pow3(unsigned exponent) {
  if (exponent > 38) {
    auto ex = -2;
#ifdef OTHER_LIBRARY_NO_EXCEPTIONS
    handleOtherException(ex);
#else
    throw ex;
#endif
  }

  return pow(3, exponent);
}

#endif // OTHER_LIBRARY_HPP
