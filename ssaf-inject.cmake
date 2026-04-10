set(_ssaf_wrapper "python3 /Users/benics/git/upstream-llvm-ssaf/ssaf-wrapper.py")

# --- Compilation: inject SSAF extraction flags ---
string(FIND "${CMAKE_CXX_COMPILE_OBJECT}" "--ssaf-extract-summaries" _idx)
if(_idx EQUAL -1)
  string(APPEND CMAKE_CXX_COMPILE_OBJECT
    " --ssaf-extract-summaries=CallGraph --ssaf-tu-summary-file=<OBJECT>.json"
  )
  message(STATUS "SSAF: enabled extraction in CMAKE_CXX_COMPILE_OBJECT")
endif()

# --- Executables: wrap the link command to also produce exe.json ---
string(FIND "${CMAKE_CXX_LINK_EXECUTABLE}" "ssaf-wrapper" _idx)
if(_idx EQUAL -1)
  string(PREPEND CMAKE_CXX_LINK_EXECUTABLE "${_ssaf_wrapper} ")
  message(STATUS "SSAF: wrapped CMAKE_CXX_LINK_EXECUTABLE")
endif()

# Note: Static library wrapping is done via -DCMAKE_LIBTOOL on the
# cmake command line, pointing to ssaf-libtool.sh.
