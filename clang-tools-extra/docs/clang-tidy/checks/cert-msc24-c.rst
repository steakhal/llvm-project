.. title:: clang-tidy - cert-msc24-c

cert-msc24-c
============

Checks for deprecated and obsolescent functions listed in
CERT C Coding Standard Recommendation MSC24-C (`MSC24-C. Do not use deprecated or obsolescent functions
<https://wiki.sei.cmu.edu/confluence/display/c/MSC24-C.+Do+not+use+deprecated+or+obsolescent+functions>`_.).
For the listed functions, an alternative, more secure replacement is suggested, if available.
The checker heavily relies on the functions from annex K (Bounds-checking interfaces) of C11.

| For the following functions, replacements are suggested from
  annex K:
  `asctime`, `ctime`, `fopen`, `freopen`, `bsearch`, `fprintf`,
  `fscanf`, `fwprintf`, `fwscanf`, `getenv`, `gmtime`, `localtime`, `mbsrtowcs`,
  `mbstowcs`, `memcpy`, `memmove`, `printf`, `qsort`, `setbuf`, `snprintf`,
  `sprintf`,  `sscanf`, `strcat`, `strcpy`, `strerror`, `strncat`, `strncpy`,
  `strtok`, `swprintf`, `swscanf`, `vfprintf`, `vfscanf`, `vfwprintf`,
  `vfwscanf`, `vprintf`, `vscanf`, `vsnprintf`,
  `vsprintf`, `vsscanf`, `vswprintf`, `vswscanf`, `vwprintf`, `vwscanf`,
  `wcrtomb`, `wcscat`, `wcscpy`, `wcsncat`, `wcsncpy`, `wcsrtombs`, `wcstok`,
  `wcstombs`, `wctomb`, `wmemcpy`, `wmemmove`, `wprintf`, `wscanf`. If annex K is not available,
  the checker ignores these functions.

The availability of annex K is checked based on the following macros:
 - `__STDC_LIB_EXT1__`: feature macro, which indicates the presence of
   annex K (Bounds-checking interfaces) in the implementation
 - `__STDC_WANT_LIB_EXT1__`: user defined macro, which must be defined to `1`.

| The following depracated function is checked, and a warning is issued if used: `gets`.

| For the following functions, alternative replacement functions are suggested: `rewind`, `setbuf`.

| The following function are covered in checker `cert-err34-c`, so this checker will IGNORE them:
  `atof`, `atoi`, `atol`, `atoll`.

Examples:

.. code-block:: c++

	#define __STDC_WANT_LIB_EXT1__ 1

	#include <string.h>
	#include <stdio.h>
  
	enum { BUFSIZE = 32 };
	void complain(const char *msg) { 
		static const char prefix[] = "Error: ";
		static const char suffix[] = "\n";
		char buf[BUFSIZE]; 
		
		strcpy(buf, prefix); // warning, strcpy_s is suggested
		strcat(buf, msg); // warning, strcat_s is suggested
		strcat(buf, suffix); // warning, strcat_s is suggested
		fputs(buf, stderr);
    }