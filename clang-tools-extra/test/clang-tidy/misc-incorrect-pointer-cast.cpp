// RUN: %check_clang_tidy %s misc-incorrect-pointer-cast %t -- \
// RUN:   -config="{CheckOptions: [{key: misc-incorrect-pointer-cast.WarnForDifferentSignedness, value: 1}]}" --

char __attribute__((aligned(4))) a[16];

struct S0 {
  char a[16];
};

struct S01 {
  char __attribute__((aligned(4))) a[16];
  struct S0 __attribute__((aligned(4))) s0;
};

struct S1 {
  int a;
  int b;
};

struct S2 {
  int a;
};

struct S3 {
  int a;
  double b;
};

struct S4 {
  int x;
  double y;
};

struct S5 {
  double y;
  int x;
};

struct __attribute__((aligned(16))) SAligned {
  char buffer[16];
};

void initDouble(double *d) {
  *d = 0.5;
}

void castCharToInt(void) {
  char c = 'x';
  int *i = (int *)&c;
  // CHECK-MESSAGES: :[[@LINE-1]]:12: warning: cast from 'char' to 'int' may lead to access memory based on invalid memory layout; pointed to type is wider than the allocated type [misc-incorrect-pointer-cast]
}

void castCharToSort() {
  char c;
  short *i = (short *)&c;
  // CHECK-MESSAGES: :[[@LINE-1]]:14: warning: cast from 'char' to 'short' may lead to access memory based on invalid memory layout; pointed to type is wider than the allocated type [misc-incorrect-pointer-cast]
}

void castShortToInt() {
  short s;
  int *i = (int *)&s;
  // CHECK-MESSAGES: :[[@LINE-1]]:12: warning: cast from 'short' to 'int' may lead to access memory based on invalid memory layout; pointed to type is wider than the allocated type [misc-incorrect-pointer-cast]
}

void castWideCharToLong() {
  wchar_t wc;
  long *f = (long *)&wc;
  // CHECK-MESSAGES: :[[@LINE-1]]:13: warning: cast from 'wchar_t' to 'long' may lead to access memory based on invalid memory layout; pointed to type is wider than the allocated type [misc-incorrect-pointer-cast]
}

void castFloatToDouble() {
  float f;
  initDouble((double *)&f);
  // CHECK-MESSAGES: :[[@LINE-1]]:14: warning: cast from 'float' to 'double' may lead to access memory based on invalid memory layout; pointed to type is wider than the allocated type [misc-incorrect-pointer-cast]
}

void castToS2(char *data, unsigned offset) {
  struct S2 *tmp;
  struct S2 header;

  tmp = (struct S2 *)(data + offset);
  // CHECK-MESSAGES: :[[@LINE-1]]:9: warning: cast from 'char' to 'struct S2' may lead to access memory based on invalid memory layout; pointed to type is wider than the allocated type [misc-incorrect-pointer-cast]
}

void castS3ToS1() {
  struct S3 s3;
  struct S1 *s1 = (struct S1 *)&s3;
  // CHECK-MESSAGES: :[[@LINE-1]]:19: warning: cast from 'struct S3' to 'struct S1' may lead to access memory based on invalid memory layout; struct members are incompatible [misc-incorrect-pointer-cast]
}

void castS4ToS5() {
  struct S4 s4;
  struct S5 *s5 = (struct S5 *)&s4;
  // CHECK-MESSAGES: :[[@LINE-1]]:19: warning: cast from 'struct S4' to 'struct S5' may lead to access memory based on invalid memory layout; struct members are incompatible [misc-incorrect-pointer-cast]
}

void castULongToLong() {
  unsigned long ul;
  long *l = (long *)&ul;
  // CHECK-MESSAGES: :[[@LINE-1]]:13: warning: cast from 'unsigned long' to 'long' may lead to access memory based on invalid memory layout; different signedness types [misc-incorrect-pointer-cast]
}

void castIntToUInt() {
  int i;
  unsigned int *ui = (unsigned int *)&i;
  // CHECK-MESSAGES: :[[@LINE-1]]:22: warning: cast from 'int' to 'unsigned int' may lead to access memory based on invalid memory layout; different signedness types [misc-incorrect-pointer-cast]
}

void castToAlignedStruct(char *P) {
  struct SAligned *a = (struct SAligned *)P;
  // CHECK-MESSAGES: :[[@LINE-1]]:24: warning: cast from 'char' to 'struct SAligned' may lead to access memory based on invalid memory layout; pointed to type is wider than the allocated type [misc-incorrect-pointer-cast]
}

void castCharToIntWithReinterpretCast(void) {
  char c = 'x';
  int *i = reinterpret_cast<int *>(&c);
  // CHECK-MESSAGES: :[[@LINE-1]]:12: warning: cast from 'char' to 'int' may lead to access memory based on invalid memory layout; pointed to type is wider than the allocated type [misc-incorrect-pointer-cast]
}

void TestDifferentAlignment() {

  struct S01 s;
  int *i = (int *)s.a;
  // CHECK-MESSAGES: :[[@LINE-1]]:12: warning: cast from 'char' to 'int' may lead to access memory based on invalid memory layout; pointed to type is wider than the allocated type [misc-incorrect-pointer-cast]
  i = (int *)&s.s0;
  // CHECK-MESSAGES: :[[@LINE-1]]:7: warning: cast from 'struct S0' to 'int' may lead to access memory based on invalid memory layout; pointed to type is strictly aligned than the allocated type [misc-incorrect-pointer-cast]
  i = (int *)a;
  // CHECK-MESSAGES: :[[@LINE-1]]:7: warning: cast from 'char' to 'int' may lead to access memory based on invalid memory layout; pointed to type is wider than the allocated type [misc-incorrect-pointer-cast]
}

// negatives
void castIntToFloat() {
  int i;
  float *f = (float *)&i;
}

void castCharToChar(char *p) {
  char *c = (char *)p;
}

void castShortToChar() {
  short s;
  char *c = (char *)&s;
}

void initInt(int *i) {
  *i = 1;
}

void castIntToInt() {
  int i;
  initInt(&i);
}

void castS1ToS2() {
  struct S1 s1;
  struct S2 *s2 = (struct S2 *)&s1;
}

void castS4ToS3() {
  struct S4 s4;
  struct S3 *s3 = (struct S3 *)&s4;
}

void IncompleteType(char *P) {
  struct B *b = (struct B *)P;
}

// Casts from void* are a special case.
void CastFromVoidPointer(void *P) {
  char *a = (char *)P;
  short *b = (short *)P;
  int *c = (int *)P;

  const volatile void *P2 = P;
  char *d = (char *)P2;
  short *e = (short *)P2;
  int *f = (int *)P2;

  const char *g = (const char *)P2;
  const short *h = (const short *)P2;
  const int *i = (const int *)P2;

  const volatile char *j = (const volatile char *)P2;
  const volatile short *k = (const volatile short *)P2;
  const volatile int *l = (const volatile int *)P2;
}

typedef int (*FnTy)(void);
unsigned int func(void);

FnTy testFunc(void) {
  return (FnTy)&func;
}

struct W;

void function3(struct W *v) {
  int *i = (int *)v;
  struct W *u = (struct W *)i;
}
