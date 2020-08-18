// RUN: %clang_analyze_cc1 -analyzer-checker=core,debug.ExprInspection \
// RUN:   -analyzer-config eagerly-assume=false -verify %s

void clang_analyzer_eval(int);
void clang_analyzer_dump_ptr(int *);
void clang_analyzer_dump_int(int);

int test1() {
  int *p = (int *)sizeof(int);
  p -= 1;
  return *p; // expected-warning {{Dereference of null pointer}}
}

int test2() {
  int *p = (int *)sizeof(int);
  p -= 2;
  p += 1;
  return *p; // expected-warning {{Dereference of null pointer}}
}

int test3() {
  int *p = (int *)sizeof(int);
  p++;
  p--;
  p--;
  return *p; // expected-warning {{Dereference of null pointer}}
}

int test4() {
  // This is a special case where pointer arithmetic is not calculated to
  // preserve useful warnings on dereferences of null pointers.
  int *p = 0;
  p += 1;
  return *p; // expected-warning {{Dereference of null pointer}}
}

void simplify_symregion_and_elementregion_pointer_arithmetic_and_comparison(int *p, int n, int m, int *q) {
  // 'q' is SymReg{q}
  // 'p' is SymReg{p}
  int *p1 = p + 1;  // Element{p,1}
  int *p0 = p1 - 1; // Element{p,0}
  int *pn = p + n;  // Element{p,n}
  int *pm = p + m;  // Element{p,m}

  clang_analyzer_dump_ptr(q);
  clang_analyzer_dump_ptr(p);
  clang_analyzer_dump_ptr(p0);
  clang_analyzer_dump_ptr(p1);
  clang_analyzer_dump_ptr(pn);
  clang_analyzer_dump_ptr(pm);
  // expected-warning-re@-6 {{&SymRegion{reg_${{[0-9]+}}<int * q>}}}
  // expected-warning-re@-6 {{&SymRegion{reg_${{[0-9]+}}<int * p>}}}
  // expected-warning-re@-6 {{&Element{SymRegion{reg_${{[0-9]+}}<int * p>},0 S64b,int}}}}
  // expected-warning-re@-6 {{&Element{SymRegion{reg_${{[0-9]+}}<int * p>},1 S64b,int}}}
  // expected-warning-re@-6 {{&Element{SymRegion{reg_${{[0-9]+}}<int * p>},reg_${{[0-9]+}}<int n>,int}}}
  // expected-warning-re@-6 {{&Element{SymRegion{reg_${{[0-9]+}}<int * p>},reg_${{[0-9]+}}<int m>,int}}}

  // Test the equality operator:
  clang_analyzer_eval(p == p0);  // expected-warning {{TRUE}}
  clang_analyzer_eval(p == p1);  // expected-warning {{FALSE}}
  clang_analyzer_eval(p1 == pn); // expected-warning {{UNKNOWN}}
  clang_analyzer_eval(pn == pm); // expected-warning {{UNKNOWN}}

  // Reverse operands:
  clang_analyzer_eval(p0 == p);  // expected-warning {{TRUE}}
  clang_analyzer_eval(p1 == p);  // expected-warning {{FALSE}}
  clang_analyzer_eval(pn == p1); // expected-warning {{UNKNOWN}}
  clang_analyzer_eval(pm == pn); // expected-warning {{UNKNOWN}}

  // Test the inequality operator:
  clang_analyzer_eval(p != p0);  // expected-warning {{FALSE}}
  clang_analyzer_eval(p != p1);  // expected-warning {{TRUE}}
  clang_analyzer_eval(p1 != pn); // expected-warning {{UNKNOWN}}
  clang_analyzer_eval(pn != pm); // expected-warning {{UNKNOWN}}

  // Reverse operands:
  clang_analyzer_eval(p0 != p);  // expected-warning {{FALSE}}
  clang_analyzer_eval(p1 != p);  // expected-warning {{TRUE}}
  clang_analyzer_eval(pn != p1); // expected-warning {{UNKNOWN}}
  clang_analyzer_eval(pm != pn); // expected-warning {{UNKNOWN}}

  // Test the subtraction operator:
  clang_analyzer_dump_int(p - q);        // expected-warning-re {{(reg_${{[0-9]+}}<int * p>) - (reg_${{[0-9]+}}<int * q>)}}
  clang_analyzer_dump_int(p - p);        // expected-warning {{0 S32b}}
  clang_analyzer_dump_int(p - p0);       // expected-warning {{0 S32b}}
  clang_analyzer_dump_int(p - p1);       // expected-warning {{-1 S32b}}
  clang_analyzer_dump_int(p - pn);       // expected-warning-re {{0 - (reg_${{[0-9]+}}<int n>)}}
  clang_analyzer_dump_int((p + 1) - q);  // expected-warning {{Unknown}} // FIXME: Might point to the same region, we should hold the expression '(p+1)-q' instead.
  clang_analyzer_dump_int((p + 1) - p);  // expected-warning {{1 S32b}}
  clang_analyzer_dump_int((p + 1) - p0); // expected-warning {{1 S32b}}
  clang_analyzer_dump_int((p + 1) - p1); // expected-warning {{0 S32b}}
  clang_analyzer_dump_int((p + 1) - pn); // expected-warning-re {{1 - (reg_${{[0-9]+}}<int n>)}}

  // Reverse operands:
  clang_analyzer_dump_int(q - p);        // expected-warning-re {{(reg_${{[0-9]+}}<int * q>) - (reg_${{[0-9]+}}<int * p>)}}
  clang_analyzer_dump_int(p - p);        // expected-warning {{0 S32b}}
  clang_analyzer_dump_int(p0 - p);       // expected-warning {{0 S32b}}
  clang_analyzer_dump_int(p1 - p);       // expected-warning {{1 S32b}}
  clang_analyzer_dump_int(pn - p);       // expected-warning-re {{reg_${{[0-9]+}}<int n>}}
  clang_analyzer_dump_int(q - (p + 1));  // expected-warning {{Unknown}} // FIXME: Might point to the same region, we should hold the expression 'q-(p+1)' instead.
  clang_analyzer_dump_int(p - (p + 1));  // expected-warning {{-1 S32b}}
  clang_analyzer_dump_int(p0 - (p + 1)); // expected-warning {{-1 S32b}}
  clang_analyzer_dump_int(p1 - (p + 1)); // expected-warning {{0 S32b}}
  clang_analyzer_dump_int(pn - (p + 1)); // expected-warning-re {{(reg_${{[0-9]+}}<int n>) - 1}}
}

void clang_analyzer_dump_ptrarray(int (*p)[10]);
void test_arrays(int (*p)[10]) {
  int(*pp)[10] = p + 2;
  clang_analyzer_dump_ptrarray(p);  // expected-warning-re {{&SymRegion{reg_${{[0-9]+}}<int (*)[10] p>}}}
  clang_analyzer_dump_ptrarray(pp); // expected-warning-re {{&Element{SymRegion{reg_${{[0-9]+}}<int (*)[10] p>},2 S64b,int [10]}}}

  // Assuming a casual x86 architecture:
  int *q = (int *)p;
  int *qq = q + 10 * 2;
  clang_analyzer_dump_ptr(q);              // expected-warning-re {{&Element{SymRegion{reg_${{[0-9]+}}<int (*)[10] p>},0 S64b,int}}}
  clang_analyzer_dump_ptr(qq);             // expected-warning-re {{&Element{SymRegion{reg_${{[0-9]+}}<int (*)[10] p>},20 S64b,int}}}
  clang_analyzer_dump_ptrarray(pp);        // expected-warning-re {{&Element{SymRegion{reg_${{[0-9]+}}<int (*)[10] p>},2 S64b,int [10]}}}
  clang_analyzer_dump_int(qq - (int *)pp); // expected-warning {{0 S32b}}
}
