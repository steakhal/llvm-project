// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=2 -verify %s -std=c90
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=2 -verify %s -std=c99
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=2 -verify %s -std=c11
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=2 -verify %s -std=c17

// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=1 -verify %s -std=c90
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=1 -verify %s -std=c99
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=1 -verify %s -std=c11
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=1 -verify %s -std=c17

// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=0 -verify %s -std=c90
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=0 -verify %s -std=c99
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=0 -verify %s -std=c11
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=0 -verify %s -std=c17

// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=2 -verify %s -std=c++98 -x c++
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=2 -verify %s -std=c++03 -x c++
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=2 -verify %s -std=c++11 -x c++
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=2 -verify %s -std=c++14 -x c++
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=2 -verify %s -std=c++17 -x c++
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=2 -verify %s -std=c++20 -x c++

// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=1 -verify %s -std=c++98 -x c++
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=1 -verify %s -std=c++03 -x c++
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=1 -verify %s -std=c++11 -x c++
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=1 -verify %s -std=c++14 -x c++
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=1 -verify %s -std=c++17 -x c++
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=1 -verify %s -std=c++20 -x c++

// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=0 -verify %s -std=c++98 -x c++
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=0 -verify %s -std=c++03 -x c++
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=0 -verify %s -std=c++11 -x c++
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=0 -verify %s -std=c++14 -x c++
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=0 -verify %s -std=c++17 -x c++
// RUN: %clang_analyze_cc1 -triple x86_64-linux-gnu -analyzer-checker=core,unix,debug.ExprInspection -fstrict-flex-arrays=0 -verify %s -std=c++20 -x c++

typedef __typeof(sizeof(int)) size_t;
size_t clang_analyzer_getExtent(void *);
void clang_analyzer_dump(size_t);

void *alloca(size_t size);
void *malloc(size_t size);
void free(void *ptr);

typedef struct FAM {
  int size;
  int data[];
} FAM;
typedef struct FAM0 {
  int size;
  int data[0];
} FAM0;
typedef struct FAM1 {
  int size;
  int data[1];
} FAM1;
typedef struct FAM2 {
  int size;
  int data[2];
} FAM2;

void test_fam(void) {
  FAM fam;
  clang_analyzer_dump(clang_analyzer_getExtent(&fam));
  clang_analyzer_dump(clang_analyzer_getExtent(fam.data));
  // expected-warning@-2 {{4 S64b}}
  // expected-warning@-2 {{0 S64b}}

  FAM *p = (FAM *)alloca(sizeof(FAM) + 3 * sizeof(int));
  clang_analyzer_dump(clang_analyzer_getExtent(p));
  clang_analyzer_dump(clang_analyzer_getExtent(p->data));
  // expected-warning@-2 {{16 U64b}}
  // expected-warning@-2 {{12 S64b}}

  FAM *q = (FAM *)malloc(sizeof(FAM) + 3 * sizeof(int));
  clang_analyzer_dump(clang_analyzer_getExtent(q));
  clang_analyzer_dump(clang_analyzer_getExtent(q->data));
  // expected-warning@-2 {{16 U64b}}
  // expected-warning@-2 {{12 S64b}}
  free(q);
}

void test_fam0(void) {
  FAM0 fam;
  clang_analyzer_dump(clang_analyzer_getExtent(&fam));
  clang_analyzer_dump(clang_analyzer_getExtent(fam.data));
  // expected-warning@-2 {{4 S64b}}
  // expected-warning@-2 {{0 S64b}}

  FAM0 *p = (FAM0 *)alloca(sizeof(FAM0) + 3 * sizeof(int));
  clang_analyzer_dump(clang_analyzer_getExtent(p));
  clang_analyzer_dump(clang_analyzer_getExtent(p->data));
  // expected-warning@-2 {{16 U64b}}
  // expected-warning@-2 {{12 S64b}}

  FAM0 *q = (FAM0 *)malloc(sizeof(FAM0) + 3 * sizeof(int));
  clang_analyzer_dump(clang_analyzer_getExtent(q));
  clang_analyzer_dump(clang_analyzer_getExtent(q->data));
  // expected-warning@-2 {{16 U64b}}
  // expected-warning@-2 {{12 S64b}}
  free(q);
}

void test_fam1(void) {
  FAM1 fam;
  clang_analyzer_dump(clang_analyzer_getExtent(&fam));
  clang_analyzer_dump(clang_analyzer_getExtent(fam.data));
  // expected-warning@-2 {{8 S64b}}
  // expected-warning@-2 {{4 S64b}}

  FAM1 *p = (FAM1 *)alloca(sizeof(FAM1) + 2 * sizeof(int));
  clang_analyzer_dump(clang_analyzer_getExtent(p));
  clang_analyzer_dump(clang_analyzer_getExtent(p->data));
  // expected-warning@-2 {{16 U64b}}
  // expected-warning@-2 {{12 S64b}}

  FAM1 *q = (FAM1 *)malloc(sizeof(FAM1) + 2 * sizeof(int));
  clang_analyzer_dump(clang_analyzer_getExtent(q));
  clang_analyzer_dump(clang_analyzer_getExtent(q->data));
  // expected-warning@-2 {{16 U64b}}
  // expected-warning@-2 {{12 S64b}}
  free(q);
}

void test_fam2(void) {
  FAM2 fam;
  clang_analyzer_dump(clang_analyzer_getExtent(&fam));
  clang_analyzer_dump(clang_analyzer_getExtent(fam.data));
  // expected-warning@-2 {{12 S64b}}
  // expected-warning@-2 {{8 S64b}}

  FAM2 *p = (FAM2 *)alloca(sizeof(FAM2) + 1 * sizeof(int));
  clang_analyzer_dump(clang_analyzer_getExtent(p));
  clang_analyzer_dump(clang_analyzer_getExtent(p->data));
  // expected-warning@-2 {{16 U64b}}
  // expected-warning@-2 {{12 S64b}}

  FAM2 *q = (FAM2 *)malloc(sizeof(FAM2) + 1 * sizeof(int));
  clang_analyzer_dump(clang_analyzer_getExtent(q));
  clang_analyzer_dump(clang_analyzer_getExtent(q->data));
  // expected-warning@-2 {{16 U64b}}
  // expected-warning@-2 {{12 S64b}}
  free(q);
}

typedef struct SubFAM {
  int something;
  FAM fam_field;
} SubFAM;

void test_subfam(void) {
  SubFAM x;
  clang_analyzer_dump(clang_analyzer_getExtent(&x));
  clang_analyzer_dump(clang_analyzer_getExtent(x.fam_field.data));
  // expected-warning@-2 {{8 S64b}}
  // expected-warning@-2 {{0 S64b}}

  SubFAM *p = (SubFAM *)alloca(sizeof(SubFAM) + 3 * sizeof(int));
  clang_analyzer_dump(clang_analyzer_getExtent(p));
  clang_analyzer_dump(clang_analyzer_getExtent(p->fam_field.data));
  clang_analyzer_dump(clang_analyzer_getExtent(&p->fam_field.data[1]));
  // expected-warning@-3 {{20 U64b}} aka. something(4) + fam_field(4 + 3*4)
  // expected-warning@-3 {{12 S64b}} aka. 3*int(4)
  // expected-warning@-3 {{4 S64b}} aka. int(4)

  SubFAM *q = (SubFAM *)malloc(sizeof(SubFAM) + 3 * sizeof(int));
  clang_analyzer_dump(clang_analyzer_getExtent(q));
  clang_analyzer_dump(clang_analyzer_getExtent(q->fam_field.data));
  clang_analyzer_dump(clang_analyzer_getExtent(&q->fam_field.data[1]));
  // expected-warning@-3 {{20 U64b}}
  // expected-warning@-3 {{12 S64b}}
  // expected-warning@-3 {{4 S64b}}
  free(q);
}

typedef struct MiddleSubFAM {
  int something;
  FAM fam_field; // expected-warning-re {{field 'fam_field' with variable sized type 'FAM' {{.*}}not at the end of a struct or class is a GNU extension}}
  int zzz;
} MiddleSubFAM;


void test_middlesubfam(void) {
  MiddleSubFAM x;
  clang_analyzer_dump(clang_analyzer_getExtent(&x));
  clang_analyzer_dump(clang_analyzer_getExtent(x.fam_field.data));
  // expected-warning@-2 {{12 S64b}}
  // expected-warning@-2 {{0 S64b}}

  MiddleSubFAM *p = (MiddleSubFAM *)alloca(sizeof(MiddleSubFAM) + 3 * sizeof(int));
  clang_analyzer_dump(clang_analyzer_getExtent(p));
  clang_analyzer_dump(clang_analyzer_getExtent(p->fam_field.data));
  clang_analyzer_dump(clang_analyzer_getExtent(&p->fam_field.data[1]));
  // expected-warning@-3 {{24 U64b}} aka. something(4) + fam_field(4 + 3*4) + zzz(4)
  // expected-warning@-3 {{0 S64b}} not a FAM!
  // expected-warning@-3 {{4 S64b}}

  MiddleSubFAM *q = (MiddleSubFAM *)malloc(sizeof(MiddleSubFAM) + 3 * sizeof(int));
  clang_analyzer_dump(clang_analyzer_getExtent(q));
  clang_analyzer_dump(clang_analyzer_getExtent(q->fam_field.data));
  clang_analyzer_dump(clang_analyzer_getExtent(&q->fam_field.data[1]));
  // expected-warning@-3 {{24 U64b}}
  // expected-warning@-3 {{0 S64b}} not a FAM!
  // expected-warning@-3 {{4 S64b}}
  free(q);
}
