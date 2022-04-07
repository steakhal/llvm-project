// RUN: %check_clang_tidy %s bugprone-wide-character-count-misuse %t

typedef unsigned long size_t;
wchar_t *wmemcpy(wchar_t *ws1, const wchar_t *ws2, size_t n);
void *memcpy(void *dest, const void *src, size_t n);
size_t strlen(const char *s);
size_t wcslen(const wchar_t *s);

wchar_t dst[64];
wchar_t src[] = L"Hello world";

void test1(void) {
  wmemcpy(dst, src, sizeof(src)); // bad (bytes)
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: function `wmemcpy' expects wide-character count instead of bytes count [bugprone-wide-character-count-misuse]
  // CHECK-MESSAGES: :[[@LINE-2]]:21: note: Sizeof expressions represent bytes count

  wmemcpy(dst, src, sizeof("foobar"));
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: function `wmemcpy' expects wide-character count instead of bytes count [bugprone-wide-character-count-misuse]
  // CHECK-MESSAGES: :[[@LINE-2]]:21: note: Sizeof expressions represent bytes count
}

void test2(void) {
  int bytes = sizeof(src);
  wmemcpy(dst, src, bytes); // bad (bytes)
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: function `wmemcpy' expects wide-character count instead of bytes count [bugprone-wide-character-count-misuse]
  // CHECK-MESSAGES: :[[@LINE-2]]:21: note: Variable represents bytes count
  // CHECK-MESSAGES: :[[@LINE-4]]:15: note: Sizeof expressions represent bytes count
}

void test3(void) {
  int bytes = sizeof(src);
  wmemcpy(dst, src, bytes - 2); // bad (bytes)
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: function `wmemcpy' expects wide-character count instead of bytes count [bugprone-wide-character-count-misuse]
  // CHECK-MESSAGES: :[[@LINE-2]]:21: note: Operator - represents bytes count
  // CHECK-MESSAGES: :[[@LINE-3]]:21: note: Variable represents bytes count
  // CHECK-MESSAGES: :[[@LINE-5]]:15: note: Sizeof expressions represent bytes count
}

void test4(void) {
  int bytes = sizeof(src);
  wmemcpy(dst, src, bytes - 2 * sizeof(wchar_t)); // no-warning
  wmemcpy(dst, src, bytes / sizeof(wchar_t));     // no-warning
  wmemcpy(dst, src, bytes / sizeof(wchar_t) - 1); // no-warning
  wmemcpy(dst, src, 8);                           // no-warning
}

void test5(void) {
  int bytes = sizeof(src);
  int two = 2;
  int two_plus_one = two + 1;

  wmemcpy(dst, src, bytes - two_plus_one);
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: function `wmemcpy' expects wide-character count instead of bytes count [bugprone-wide-character-count-misuse]
  // CHECK-MESSAGES: :[[@LINE-2]]:21: note: Operator - represents bytes count
  // CHECK-MESSAGES: :[[@LINE-3]]:21: note: Variable represents bytes count
  // CHECK-MESSAGES: :[[@LINE-8]]:15: note: Sizeof expressions represent bytes count
  // CHECK-MESSAGES: :[[@LINE-5]]:29: note: Variable represents characters count
  // CHECK-MESSAGES: :[[@LINE-8]]:22: note: Operator + represents character count
  // CHECK-MESSAGES: :[[@LINE-9]]:22: note: Variable represents characters count
}

static const char str[] = "Hello world";
static const wchar_t w_str[] = L"Hello world";
void test6(char *dst, wchar_t *wdst) {
  memcpy(dst, str, strlen(str) + 1);
  wmemcpy(wdst, w_str, wcslen(w_str) + 1);
}

void test7(wchar_t *wdst) {
  wmemcpy(wdst, w_str, strlen(str) + 1);
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: function `wmemcpy' expects wide-character count instead of bytes count [bugprone-wide-character-count-misuse]
  // CHECK-MESSAGES: :[[@LINE-2]]:24: note: Operator + represents bytes count
  // CHECK-MESSAGES: :[[@LINE-3]]:24: note: Return value of `strlen' represents bytes count
}

void test8(char *dst) {
  memcpy(dst, str, wcslen(w_str) + 1); // FIXME: We should warn about this.
}

void test9(wchar_t *wdst, wchar_t *wsrc) {
  // FIXME: We should not warn about this.
  wmemcpy(wdst, wsrc, sizeof(wchar_t) * 8);
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: function `wmemcpy' expects wide-character count instead of bytes count [bugprone-wide-character-count-misuse]
  // CHECK-MESSAGES: :[[@LINE-2]]:23: note: Operator * represents bytes count
  // CHECK-MESSAGES: :[[@LINE-3]]:23: note: Sizeof expressions represent bytes count
}

wchar_t *wcsncpy(wchar_t *dst, const wchar_t *src, size_t n);
void test_wcsncpy(wchar_t *wdst, wchar_t *wsrc) {
  wcsncpy(wdst, wsrc, sizeof(int));
  // CHECK-MESSAGES: :[[@LINE-1]]:3: warning: function `wmemcpy' expects wide-character count instead of bytes count [bugprone-wide-character-count-misuse]
  // CHECK-MESSAGES: :[[@LINE-2]]:21: note: Sizeof expressions represent bytes count
}

// build/debug/bin/clang-tidy 'clang-tools-extra/test/clang-tidy/checkers/bugprone-wide-character-count-misuse.cpp' '--checks=-*,bugprone-wide-character-count-misuse'

// https://wiki.sei.cmu.edu/confluence/display/c/ARR38-C.+Guarantee+that+library+functions+do+not+form+invalid+pointers