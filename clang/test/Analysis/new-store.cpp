// RUN: %clang_analyze_cc1 -verify %s \
// RUN:   -analyzer-checker=core,unix.Malloc,unix.cstring,debug.ExprInspection \
// RUN:   -analyzer-config enable-new-store=true

template <class T> void clang_analyzer_dump(T);
void clang_analyzer_printState();

void store_and_load_concrete_val() {
  int z = 4;
  clang_analyzer_dump(z); // expected-warning {{4 S32b}}
}

void two_stores() {
  int z = 4;
  z = 5;
  clang_analyzer_dump(z); // expected-warning {{5 S32b}}
}

void two_vars() {
  int z = 4;
  int y = 5;
  clang_analyzer_dump(z); // expected-warning {{4 S32b}}
  clang_analyzer_dump(y); // expected-warning {{5 S32b}}
}

void store_and_load_val_assignment() {
  int x;
  x = 3;
  clang_analyzer_dump(x); // expected-warning {{3 S32b}}
}

void undef_variable() {
    int x;
    clang_analyzer_dump(x); // expected-warning {{1st function call argument is an uninitialized value}}
}

void array_subscript() {
  int arr[5];
  arr[3] = 9;
  clang_analyzer_dump(arr[3]); // expected-warning {{9 S32b}}
}

void array_two_subscripts() {
  int arr[5];
  arr[3] = 9;
  arr[2] = 7;
  clang_analyzer_dump(arr[3]); // expected-warning {{9 S32b}}
  clang_analyzer_dump(arr[2]); // expected-warning {{7 S32b}}
}

struct UnusedStruct {
  int x;
  int y;
  UnusedStruct(const UnusedStruct &Other) : x(Other.x), y(Other.y) {
    clang_analyzer_dump(Other.x);
    clang_analyzer_dump(x);
    // expected-warning-re@-2 {{reg_${{[0-9]+}}<int Element{SymRegion{reg_${{[0-9]+}}<const UnusedStruct & Other>},0 S64b,struct UnusedStruct}.x>}}
    // expected-warning-re@-2 {{reg_${{[0-9]+}}<int Element{SymRegion{reg_${{[0-9]+}}<const UnusedStruct & Other>},0 S64b,struct UnusedStruct}.x>}}
    clang_analyzer_dump(Other.y);
    clang_analyzer_dump(y);
    // expected-warning-re@-2 {{reg_${{[0-9]+}}<int Element{SymRegion{reg_${{[0-9]+}}<const UnusedStruct & Other>},0 S64b,struct UnusedStruct}.y>}}
    // expected-warning-re@-2 {{reg_${{[0-9]+}}<int Element{SymRegion{reg_${{[0-9]+}}<const UnusedStruct & Other>},0 S64b,struct UnusedStruct}.y>}}
  }
};

struct S {
  int x;
  int y;
};

void struct_access() {
  S s;
  s.x = 1;
  s.y = 2;
  clang_analyzer_dump(s.x); // expected-warning {{1 S32b}}
  clang_analyzer_dump(s.y); // expected-warning {{2 S32b}}
}

void struct_param(S s) {
  s.x = 1;
  s.y = 2;
  clang_analyzer_dump(s.x); // expected-warning {{1 S32b}}
  clang_analyzer_dump(s.y); // expected-warning {{2 S32b}}
}

void struct_aggregate_init() {
  S s{1, 2};
  clang_analyzer_dump(s.x); // expected-warning{{1 S32b}}
  clang_analyzer_dump(s.y); // expected-warning{{2 S32b}}
  clang_analyzer_dump(s); // expected-warning-re{{lazyCompoundVal{0x{{[0-9a-f]+}},s}}}
}

S makePairViaAssignments(int x, int y) {
  S s;
  s.x = x;
  s.y = y;
  return s;
}

S makePairAggrInit(int x, int y) {
  return S{x, y};
}

void struct_lcv_assignment() {
  S w = makePairViaAssignments(1, 2);
  clang_analyzer_dump(w.x); // expected-warning{{1 S32b}}
  clang_analyzer_dump(w.y); // expected-warning{{2 S32b}}
}

void struct_lcv_aggr_init() {
  S w = makePairAggrInit(1, 2);
  clang_analyzer_dump(w.x); // expected-warning{{1 S32b}}
  clang_analyzer_dump(w.y); // expected-warning{{2 S32b}}
}

void arr_aggregate_init() {
  int arr[2] = {1, 2};
  clang_analyzer_dump(arr[0]); // expected-warning{{1 S32b}}
  clang_analyzer_dump(arr[1]); // expected-warning{{2 S32b}}
}

using uint8_t = unsigned char;
using uint16_t = unsigned short;
using uint64_t = unsigned long long;
using size_t = decltype(sizeof(0));

void cpp3620() {
  uint64_t x64[2];
  x64[0] = 0;
  uint8_t* x8 = (uint8_t*)x64;
  clang_analyzer_dump(x8[1]); // expected-warning{{0 U8b}}
  clang_analyzer_dump(x8[10]); // expected-warning{{1st function call argument is an uninitialized value}}
}


void nested_subreg() {
  uint64_t x64[2];
  ((uint16_t*)x64)[0] = 0;
  uint8_t* x8 = (uint8_t*)x64;
  clang_analyzer_dump(x8[1]); // expected-warning{{0 U8b}}
  clang_analyzer_dump(x8[4]); // expected-warning{{1st function call argument is an uninitialized value}}
}

void nested_subreg_same_size_query() {
  uint64_t x64[2];
  ((uint16_t*)x64)[0] = 0;
  uint16_t* x16 = (uint16_t*)x64;
  clang_analyzer_dump(x16[0]); // expected-warning{{0 U16b}}
  clang_analyzer_dump(x16[1]); // expected-warning{{1st function call argument is an uninitialized value}}
}

void nested_subreg_ugly_overlap00() {
  uint64_t x64[2];
  ((uint16_t*)x64)[0] = 0;
  ((uint16_t*)x64)[1] = 0;
  uint16_t* x16 = (uint16_t*)(((uint8_t*)x64) + 1);
  // Limitation: two ajoint stores of 0 are not concatenated
  clang_analyzer_dump(x16[0]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x16[1]); // expected-warning{{1st function call argument is an uninitialized value}}
}

void nested_subreg_ugly_overlap0() {
  uint64_t x64[2];
  ((uint16_t*)x64)[0] = 0;
  uint16_t* x16 = (uint16_t*)(((uint8_t*)x64) + 1);
  clang_analyzer_dump(x16[0]); // expected-warning{{1st function call argument is an uninitialized value}}
}

void nested_subreg_ugly_overlap001() {
  uint64_t x64[2];
  ((uint16_t*)x64)[0] = 0;
  uint16_t* x16 = (uint16_t*)(((uint8_t*)x64) + 1);
  clang_analyzer_dump(x16[1]); // expected-warning{{1st function call argument is an uninitialized value}}
}

void nested_subreg_ugly_overlap01() {
  uint64_t x64[2];
  ((uint16_t*)x64)[0] = 0;
  ((uint16_t*)x64)[1] = 1;
  uint16_t* x16 = (uint16_t*)(((uint8_t*)x64) + 1);
  clang_analyzer_dump(x16[0]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x16[1]); // expected-warning{{1st function call argument is an uninitialized value}}
}

void small_write_spoils_big_num() {
  uint64_t x64[2];
  x64[0] = 0;
  x64[1] = 0;
  ((uint8_t*)x64)[2] = 1;
  clang_analyzer_dump(x64[0]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x64[1]); // expected-warning{{0 U64b}}
}

void small_write_doesnt_spoil_big_num_aggregate_init() {
  uint64_t x64[2]{0, 0};
  ((uint8_t*)x64)[2] = 1;
  clang_analyzer_dump(x64[0]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x64[1]); // expected-warning{{0 U64b}}
}

void proposal_issue1() {
  int x = 0x01020304;
  *(char *)&x = 0;

  clang_analyzer_dump(x); // expected-warning{{Unknown}}
  clang_analyzer_dump(*(char *)&x); // expected-warning{{0 S8b}}
}

void proposal_issue2() {
  int x = 0x01020304;
  *((char *)&x + 1) = 1;

  clang_analyzer_dump(x); // expected-warning{{Unknown}}
  clang_analyzer_dump(*((char *)&x + 1)); // expected-warning{{1 S8b}}
}

void proposal_issue3() {
  int x = 0x01020304; // = 16909060
  *((char *)&x + 1) = 1;

  clang_analyzer_dump(x); // expected-warning{{Unknown}}
  // Correct
  clang_analyzer_dump(*((char *)&x + 1)); // expected-warning{{1 S8b}}
}

void proposal_issue4() {
  char x = 0;
  clang_analyzer_dump(*((int *)&x)); // expected-warning{{1st function call argument is an uninitialized value}}
}

struct Outer {
  int x;
  struct Inner {
    int z;
  } s;
  int y;
};

void nested_struct() {
  Outer s = {1, {2}, 3};
  clang_analyzer_dump(s.x); // expected-warning{{1 S32b}}
  clang_analyzer_dump(s.y); // expected-warning{{3 S32b}}
  clang_analyzer_dump(s.s); // expected-warning-re{{lazyCompoundVal{0x{{[0-9a-f]+}},s.s}}}
  clang_analyzer_dump(s.s.z); // expected-warning{{2 S32b}}
}

void unknown_index_store(int idx) {
  int arr[5];
  arr[idx] = 9;
  clang_analyzer_dump(arr[idx]); // expected-warning {{9 S32b}}
  clang_analyzer_dump(arr[1]); // expected-warning {{Unknown}}
}

void unknown_index_load(int idx) {
  int arr[5];
  arr[2] = 9;
  clang_analyzer_dump(arr[idx]); // expected-warning {{Unknown}}
  clang_analyzer_dump(arr[2]); // expected-warning {{9 S32b}}
  clang_analyzer_dump(arr[1]); // expected-warning {{1st function call argument is an uninitialized value}}
}

int mk();

void undef_read_whole() {
  uint64_t x64;
  uint8_t *x8 = (uint8_t *)&x64;
  x8[0] = 5;
  clang_analyzer_dump(x8[0]); // expected-warning{{5 U8b}}
  clang_analyzer_dump(x64); // expected-warning{{1st function call argument is an uninitialized value}}
}

void undef_read_part() {
  uint64_t x64;
  uint8_t *x8 = (uint8_t *)&x64;
  x8[2] = 5;
  clang_analyzer_dump(x8[2]); // expected-warning{{5 U8b}}
  clang_analyzer_dump(x8[1]); // expected-warning{{1st function call argument is an uninitialized value}}
}

void unaligned_undef_read_part() {
  uint64_t x64;
  uint8_t *x8 = (uint8_t *)&x64;
  uint16_t *x16 = (uint16_t *)&x64;
  x8[1] = 5;
  clang_analyzer_dump(x16[0]); // expected-warning{{1st function call argument is an uninitialized value}}
}

uint64_t global_x64;

void global_undef_read_whole() {
  uint8_t *x8 = (uint8_t *)&global_x64;
  x8[0] = 5;
  clang_analyzer_dump(x8[0]);      // expected-warning{{5 U8b}}
  clang_analyzer_dump(global_x64); // expected-warning{{Unknown}}
}

void global_undef_read_part() {
  uint8_t *x8 = (uint8_t *)&global_x64;
  x8[2] = 5;
  clang_analyzer_dump(x8[2]); // expected-warning{{5 U8b}}
  clang_analyzer_dump(x8[1]); // expected-warning{{Unknown}}
}

void global_read_from_empty_store() {
  clang_analyzer_dump(global_x64); // expected-warning-re {{reg_${{[0-9]+}}<uint64_t global_x64>}}
}

void symbolic_write_no_undef_read() {
  uint64_t x64;
  uint8_t *x8 = (uint8_t *)&x64;
  int i1 = mk();
  x8[i1] = 5;
  // Symbolic write unvalidated entire region.
  // Now we don't know what is initialized and what is not.
  clang_analyzer_dump(x8[0]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x8[1]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x8[2]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x8[3]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x8[4]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x8[i1]); // expected-warning{{5 U8b}}
  x8[1] = 1;
  x8[3] = 8;
  clang_analyzer_dump(x8[0]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x8[1]); // expected-warning{{1 U8b}}
  clang_analyzer_dump(x8[2]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x8[3]); // expected-warning{{8 U8b}}
  clang_analyzer_dump(x8[4]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x8[i1]); // expected-warning{{Unknown}}
  x8[i1] = 6;
  // Symbolic write unvalidated entire region once again.
  clang_analyzer_dump(x8[0]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x8[1]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x8[2]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x8[3]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x8[4]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x8[i1]); // expected-warning{{6 U8b}}
}

void symbolic_write_then_symbolic_read() {
  uint64_t x64;
  uint8_t *x8 = (uint8_t *)&x64;
  int i1 = mk();
  int i2 = mk();
  x8[i1] = 1;
  clang_analyzer_dump(x8[i1]); // expected-warning{{1 U8b}}
  clang_analyzer_dump(x8[i2]); // expected-warning{{Unknown}}
  x8[i2] = 3;
  clang_analyzer_dump(x8[i1]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x8[i2]); // expected-warning{{3 U8b}}
  x8[i2] = 3; // Let's do the same, and check if the results remain the same.
  clang_analyzer_dump(x8[i1]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x8[i2]); // expected-warning{{3 U8b}}
  x8[0] = 18;
  // Concrete write resets any symbolic binding
  clang_analyzer_dump(x8[i1]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x8[i2]); // expected-warning{{Unknown}}
  clang_analyzer_dump(x8[0]); // expected-warning{{18 U8b}}
  clang_analyzer_dump(x8[1]); // expected-warning{{Unknown}}
}

void symbolic_top_level_params(int x, int y) {
  int sum = x + y;
  clang_analyzer_dump(x); // expected-warning-re {{reg_${{[0-9]+}}<int x>}}
  clang_analyzer_dump(y); // expected-warning-re {{reg_${{[0-9]+}}<int y>}}
  clang_analyzer_dump(sum); // expected-warning-re {{(reg_${{[0-9]+}}<int x>) + (reg_${{[0-9]+}}<int y>)}}
}

void lambda_variables() {
  // expected-warning@+1{{Assigned value is uninitialized}} FIXME FP
  auto one = []{ return 1; };
  int v = one();
  clang_analyzer_dump(v); // FIXME: This should be 1
}


void *malloc(size_t size);
void *calloc(size_t nmemb, size_t size);
void *free(void *ptr);

void malloc_uninit() {
  uint8_t *p = (uint8_t *)malloc(10);
  if (!p)
    return;
  clang_analyzer_dump(p[1]); // expected-warning{{1st function call argument is an uninitialized value}}
  free(p);
}

void calloc_zero_init() {
  uint8_t *p = (uint8_t *)calloc(2, sizeof(uint8_t));
  if (!p)
    return;
  clang_analyzer_dump(p[0]); // expected-warning{{0 U8b}}
  clang_analyzer_dump(p[1]); // expected-warning{{0 U8b}}
  clang_analyzer_dump(p[2]); // expected-warning{{1st function call argument is an uninitialized value}}
  free(p);
}

void malloc_init_pieces() {
  uint8_t *p = (uint8_t *)malloc(10);
  if (!p)
    return;
  p[1] = 1;
  clang_analyzer_dump(p[1]); // expected-warning{{1 U8b}}
  uint16_t *p16 = (uint16_t *)p;
  p16[1] = 6;
  clang_analyzer_dump(((uint16_t *)p)[1]); // expected-warning{{6 U16b}}
  clang_analyzer_dump(p[1]); // expected-warning{{1 U8b}}
  // These are two halves of u16
  clang_analyzer_dump(p[2]); // expected-warning{{Unknown}}
  clang_analyzer_dump(p[3]); // expected-warning{{Unknown}}
  clang_analyzer_dump(p16[2]); // expected-warning{{1st function call argument is an uninitialized value}}
  free(p);
}

void no_leak() {
  void *p = malloc(10);
  if (!p)
    return;
  // FIXME: implement iterBindings
  free(p); // expected-warning{{Potential leak of memory pointed to by 'p'}}
}

void *memset(void *s, int c, size_t n);

void memset0_read_0_1_offsets() {
  uint8_t *p = (uint8_t *)malloc(10);
  if (!p)
    return;
  memset(p, 0, 10); // expected-warning{{Potential leak of memory pointed to by 'p'}} FIXME
  clang_analyzer_dump(p[0]); // expected-warning{{0 U8b}}
  clang_analyzer_dump(p[1]); // expected-warning{{0 U8b}}
  free(p);
}

void memset0_read_different_subregions() {
  uint8_t *p = (uint8_t *)malloc(10);
  if (!p)
    return;
  memset(p, 0, 10); // expected-warning{{Potential leak of memory pointed to by 'p'}} FIXME
  clang_analyzer_dump(p[0]); // expected-warning{{0 U8b}}
  clang_analyzer_dump(p[1]); // expected-warning{{0 U8b}}
  uint16_t *p16 = (uint16_t *)p;
  clang_analyzer_dump(p16[0]); // expected-warning{{0 U16b}}
  clang_analyzer_dump(p16[1]); // expected-warning{{0 U16b}}
  uint64_t *p64 = (uint64_t *)p;
  clang_analyzer_dump(p64[0]); // expected-warning{{0 U64b}}
  free(p);
}

void memset0_with_direct_binding() {
  uint8_t *p = (uint8_t *)malloc(10);
  if (!p)
    return;
  memset(p, 0, 10); // expected-warning{{Potential leak of memory pointed to by 'p'}} FIXME
  uint16_t *p16 = (uint16_t *)p;
  p16[1] = 18;
  clang_analyzer_dump(p[0]); // expected-warning{{0 U8b}}
  clang_analyzer_dump(p[1]); // expected-warning{{0 U8b}}
  clang_analyzer_dump(p[2]); // expected-warning{{Unknown}} Correct
  clang_analyzer_dump(p[3]); // expected-warning{{Unknown}} Correct
  clang_analyzer_dump(p[4]); // expected-warning{{0 U8b}}
  clang_analyzer_dump(p16[0]); // expected-warning{{0 U16b}}
  clang_analyzer_dump(p16[1]); // expected-warning{{18 U16b}}
  signed short int *p16s = (signed short int *)p;
  clang_analyzer_dump(p16s[0]); // expected-warning{{0 S16b}}
  // FIXME: should be 18 signed, or unknown, but not 18 unsigned.
  clang_analyzer_dump(p16s[1]); // expected-warning{{18 U16b}} FIXME
  uint64_t *p64 = (uint64_t *)p;
  clang_analyzer_dump(p64[0]); // expected-warning{{Unknown}}
  memset(p, 0, 10);
  // Now everything is overwritten again
  clang_analyzer_dump(p[0]); // expected-warning{{0 U8b}}
  clang_analyzer_dump(p[1]); // expected-warning{{0 U8b}}
  clang_analyzer_dump(p[2]); // expected-warning{{0 U8b}}
  clang_analyzer_dump(p[3]); // expected-warning{{0 U8b}}
  clang_analyzer_dump(p[4]); // expected-warning{{0 U8b}}
  clang_analyzer_dump(p16[0]); // expected-warning{{0 U16b}}
  clang_analyzer_dump(p16[1]); // expected-warning{{0 U16b}}
  clang_analyzer_dump(p64[0]); // expected-warning{{0 U64b}}
  free(p);
}

void memset_non_0() {
  uint8_t *p = (uint8_t *)malloc(10);
  if (!p)
    return;
  // TODO: implement invalidation (?) Here memset is handled as opaque function
  // call and not translated into BindDefaultZero invocation because the value
  // passed is 1 and not 0.
  memset(p, 1, 10);
  clang_analyzer_dump(p[0]); // expected-warning{{1st function call argument is an uninitialized value}}
  free(p);
}

void param_dereference(uint8_t *p) {
  // TODO: create a symbolic value for a param ptr dereference
  clang_analyzer_dump(p[0]); // expected-warning{{1st function call argument is an uninitialized value}}
}

void param_dereference_after_memset(uint8_t *p) {
  // TODO: implement invalidation (?) Here memset is handled as opaque function
  // call and not translated into BindDefaultZero invocation because extent of p
  // is not known
  //
  // memset behaves as a regular opaque fcall here because CStringChecker cannot
  // prove that the provided extent matches the entire range of p (which is
  // unknown).
  memset(p, 0, 10);
  clang_analyzer_dump(p[0]); // expected-warning{{1st function call argument is an uninitialized value}}
}

struct TwoArrs {
  uint8_t arr1[5];
  uint8_t arr2[5];
};

void memset_struct() {
  TwoArrs obj;
  memset(&obj, 0, sizeof(obj));
  clang_analyzer_dump(obj.arr1[0]); // expected-warning{{0 U8b}}
  clang_analyzer_dump(obj.arr2[0]); // expected-warning{{0 U8b}}
  obj.arr1[0] = 1;
  clang_analyzer_dump(obj.arr1[0]); // expected-warning{{1 U8b}}
  clang_analyzer_dump(obj.arr2[0]); // expected-warning{{0 U8b}}
}

void memset_struct_field() {
  TwoArrs obj;
  memset(&obj.arr1, 0, sizeof(obj.arr1));
  // memset of a subregion is not handled by cstring checker
  clang_analyzer_dump(obj.arr1[0]); // expected-warning-re{{derived_${{[0-9]+}}{conj_${{[0-9]+}}{TwoArrs, LC{{[0-9]+}}, S{{[0-9]+}}, #1},Element{obj.arr1,0 S64b,unsigned char}}}}
}

struct Inner1 {
  int x;
};

struct Medium1 {
  int m1;
  Inner1 inner;
  int m2;
};

struct Outer1 {
  int o1;
  Medium1 med;
  int o2;
};

void triple_nested_structs() {
  Outer1 str = {1, {2, {3}, 4}, 5};
  clang_analyzer_dump(str.o1); // expected-warning{{1 S32b}}
  clang_analyzer_dump(str.med.m1); // expected-warning{{2 S32b}}
  clang_analyzer_dump(str.med.inner.x); // expected-warning{{3 S32b}}
  clang_analyzer_dump(str.med.m2); // expected-warning{{4 S32b}}
  clang_analyzer_dump(str.o2); // expected-warning{{5 S32b}}
}

void triple_nested_structs_symbolic_vals(int x, int *p) {
  Outer1 str = {x, {p[1], {p[2] + 1}, 4}, 5};
  clang_analyzer_dump(str.o1); // expected-warning-re {{reg_${{[0-9]+}}<int x>}}
  clang_analyzer_dump(str.med.m1); // expected-warning-re {{reg_${{[0-9]+}}<int Element{SymRegion{reg_${{[0-9]+}}<int * p>},1 S64b,int}>}}
  clang_analyzer_dump(str.med.inner.x); // expected-warning-re {{reg_${{[0-9]+}}<int Element{SymRegion{reg_${{[0-9]+}}<int * p>},2 S64b,int}>) + 1}}
  clang_analyzer_dump(str.med.m2); // expected-warning{{4 S32b}}
  clang_analyzer_dump(str.o2); // expected-warning{{5 S32b}}
}

struct Inner2 {
  int x;
};

struct Medium2 {
  int m1[2];
  Inner2 inner[2];
  int m2[2];
};

struct Outer2 {
  int o1[2];
  Medium2 med[2];
  int o2[2];
};

void nested_arrays() {
  Outer2 str = {{1, 2}, // o1
                {{{3, 4}, // med[0].m1
                  {{5}, {6}}, // med[0].inner
                  {7, 8}}, // med[0].m2
                 {{9, 10}, // med[1].m1
                  {{11}, {12}}, // med[1].inner
                  {13, 14}}}, // med[1].m2
                {15, 16}}; // o2
  clang_analyzer_dump(str.o1[0]);             // expected-warning{{1}}
  clang_analyzer_dump(str.o1[1]);             // expected-warning{{2}}
  clang_analyzer_dump(str.med[0].m1[0]);      // expected-warning{{3}}
  clang_analyzer_dump(str.med[0].m1[1]);      // expected-warning{{4}}
  clang_analyzer_dump(str.med[0].inner[0].x); // expected-warning{{5}}
  clang_analyzer_dump(str.med[0].inner[1].x); // expected-warning{{6}}
  clang_analyzer_dump(str.med[0].m2[0]);      // expected-warning{{7}}
  clang_analyzer_dump(str.med[0].m2[1]);      // expected-warning{{8}}
  clang_analyzer_dump(str.med[1].m1[0]);      // expected-warning{{9}}
  clang_analyzer_dump(str.med[1].m1[1]);      // expected-warning{{10}}
  clang_analyzer_dump(str.med[1].inner[0].x); // expected-warning{{11}}
  clang_analyzer_dump(str.med[1].inner[1].x); // expected-warning{{12}}
  clang_analyzer_dump(str.med[1].m2[0]);      // expected-warning{{13}}
  clang_analyzer_dump(str.med[1].m2[1]);      // expected-warning{{14}}
  clang_analyzer_dump(str.o2[0]);             // expected-warning{{15}}
  clang_analyzer_dump(str.o2[1]);             // expected-warning{{16}}
  str.med[0] = str.med[1]; // LazyCompoundVal referencing a subreg of the same base reg
  clang_analyzer_dump(str.med[0]); // expected-warning-re{{lazyCompoundVal{0x{{[0-9a-f]+}},Element{str.med,1 S64b,struct Medium2}}}}
  clang_analyzer_dump(str.med[1]); // expected-warning-re{{lazyCompoundVal{0x{{[0-9a-f]+}},Element{str.med,1 S64b,struct Medium2}}}}
  clang_analyzer_dump(str.med[0].m1[0]);      // expected-warning{{9 S32b}}
  clang_analyzer_dump(str.med[0].m1[1]);      // expected-warning{{10 S32b}}
  clang_analyzer_dump(str.med[0].inner[0].x); // expected-warning{{11 S32b}}
  clang_analyzer_dump(str.med[0].inner[1].x); // expected-warning{{12 S32b}}
  clang_analyzer_dump(str.med[0].m2[0]);      // expected-warning{{13 S32b}}
  clang_analyzer_dump(str.med[0].m2[1]);      // expected-warning{{14 S32b}}

  // Old values are still there and not erased
  clang_analyzer_dump(str.med[1].m1[0]);      // expected-warning{{9}}
  clang_analyzer_dump(str.med[1].m1[1]);      // expected-warning{{10}}
  clang_analyzer_dump(str.med[1].inner[0].x); // expected-warning{{11}}
  clang_analyzer_dump(str.med[1].inner[1].x); // expected-warning{{12}}
  clang_analyzer_dump(str.med[1].m2[0]);      // expected-warning{{13}}
  clang_analyzer_dump(str.med[1].m2[1]);      // expected-warning{{14}}
}

struct L0 {
  int l0f0;
  struct L1 {
    int l1f0;
    struct L2 {
      int l2f0;
    } l1f1;
    int l1f2;
  } l0f1;
  int l0f2;
};

void nested_struct_lcv() {
  L0 x = {1, {2, {3}, 4}, 5};
  x.l0f1 = L0::L1{7, {8}, 9};
  clang_analyzer_dump(x.l0f0);           // expected-warning{{1 S32b}}
  clang_analyzer_dump(x.l0f1.l1f0);      // expected-warning{{7 S32b}}
  clang_analyzer_dump(x.l0f1.l1f1.l2f0); // expected-warning{{8 S32b}}
  clang_analyzer_dump(x.l0f1.l1f1);      // expected-warning-re{{lazyCompoundVal{0x{{[0-9a-f]+}},temp_object{L0::L1, S{{[0-9]+}}}.l1f1}}}
  clang_analyzer_dump(x.l0f1.l1f2);      // expected-warning{{9 S32b}}
  clang_analyzer_dump(x.l0f1);           // expected-warning-re{{lazyCompoundVal{0x{{[0-9a-f]+}},temp_object{L0::L1, S{{[0-9]+}}}}}
  clang_analyzer_dump(x.l0f2);           // expected-warning{{5 S32b}}
}

struct Base1 { int x; };
struct Derived1 : Base1 { int y; };

void inheritance_simple() {
  Derived1 der = {{1}, 2};
  clang_analyzer_dump(der.x); // expected-warning{{1}}
  clang_analyzer_dump(der.y); // expected-warning{{2}}
}

struct Base2 {
  int x;
  int barr[2];
};

struct Derived2 : Base2 {
  int y;
  int arr[2];
};

void inheritance_with_arr() {
  Derived2 der = {{1, {2, 3}}, 4, {5, 6}};
  clang_analyzer_dump(der.x); // expected-warning{{1}}
  clang_analyzer_dump(der.barr[0]); // expected-warning{{2}}
  clang_analyzer_dump(der.barr[1]); // expected-warning{{3}}
  clang_analyzer_dump(der.y); // expected-warning{{4}}
  clang_analyzer_dump(der.arr[0]); // expected-warning{{5}}
  clang_analyzer_dump(der.arr[1]); // expected-warning{{6}}
}

struct Base3 {
  int barr[2];
  int x;
};

struct Med3 : Base3 {
  int m2[2];
  int m1;
};

struct Derived3 : Med3 {
  int y;
  Med3 nestedMed[2];
};

void extended_inheritance() {
  Derived3 der = {
    { // Med3 base
      {{1, 2}, 3}, // Base2 base
      {4, 5}, // m2
      6 // m1
    },
    7, // y
    { // nestedMed
      {  // [0]
        {{8, 9}, 10}, // Base2 base
        {11, 12}, // m2
        13 // m1
      },
      {  // [1]
        {{14, 15}, 16}, // Base2 base
        {17, 18}, // m2
        19 // m1
      }
    }
  };
  clang_analyzer_dump(der.barr[0]);              // expected-warning{{1}}
  clang_analyzer_dump(der.barr[1]);              // expected-warning{{2}}
  clang_analyzer_dump(der.x);                    // expected-warning{{3}}
  clang_analyzer_dump(der.m2[0]);                // expected-warning{{4}}
  clang_analyzer_dump(der.m2[1]);                // expected-warning{{5}}
  clang_analyzer_dump(der.m1);                   // expected-warning{{6}}
  clang_analyzer_dump(der.y);                    // expected-warning{{7}}
  clang_analyzer_dump(der.nestedMed[0].barr[0]); // expected-warning{{8}}
  clang_analyzer_dump(der.nestedMed[0].barr[1]); // expected-warning{{9}}
  clang_analyzer_dump(der.nestedMed[0].x);       // expected-warning{{10}}
  clang_analyzer_dump(der.nestedMed[0].m2[0]);   // expected-warning{{11}}
  clang_analyzer_dump(der.nestedMed[0].m2[1]);   // expected-warning{{12}}
  clang_analyzer_dump(der.nestedMed[0].m1);      // expected-warning{{13}}
  clang_analyzer_dump(der.nestedMed[1].barr[0]); // expected-warning{{14}}
  clang_analyzer_dump(der.nestedMed[1].barr[1]); // expected-warning{{15}}
  clang_analyzer_dump(der.nestedMed[1].x);       // expected-warning{{16}}
  clang_analyzer_dump(der.nestedMed[1].m2[0]);   // expected-warning{{17}}
  clang_analyzer_dump(der.nestedMed[1].m2[1]);   // expected-warning{{18}}
  clang_analyzer_dump(der.nestedMed[1].m1);      // expected-warning{{19}}
}

struct Base41 { int bs41[2]; };
struct Base42 { int bs42; };
struct Base43 { uint64_t bs43; };
struct Base44 { uint8_t bs44; };


struct Med41 : Base41, Base42 { int m41; };
struct Med42 : Base43, Base44 { uint8_t m42[2]; };
struct Derived4 : Med41, Med42 { int y; };

void multiple_inheritance() {
  Derived4 der = {
    { // Med41 base
      {1, 2}, // Base41
      {3}, // Base42
      4 // m41
    },
    { // Med42 base
      {5}, // Base43
      {6}, // Base44
      {7, 8} // m42
    },
    9 // y
  };
  clang_analyzer_dump(der.bs41[0]); // expected-warning{{1 S32b}}
  clang_analyzer_dump(der.bs41[1]); // expected-warning{{2 S32b}}
  clang_analyzer_dump(der.bs42);    // expected-warning{{3 S32b}}
  clang_analyzer_dump(der.m41);     // expected-warning{{4 S32b}}
  clang_analyzer_dump(der.bs43);    // expected-warning{{5 U64b}}
  clang_analyzer_dump(der.bs44);    // expected-warning{{6 U8b}}
  clang_analyzer_dump(der.m42[0]);  // expected-warning{{7 U8b}}
  clang_analyzer_dump(der.m42[1]);  // expected-warning{{8 U8b}}
  clang_analyzer_dump(der.y);       // expected-warning{{9 S32b}}
}

struct Base5 { int x; };
struct Med51 : Base5 {};
struct Med52 : Base5 {};
struct Derived5 : Med51, Med52 { int y; };

void multiple_inheritance_same_base() {
  Derived5 der = {
    {1}, // Med51::x
    {2}, // Med52::x
    3 // y
  };
  clang_analyzer_dump(der.Med51::x); // expected-warning{{1}}
  clang_analyzer_dump(der.Med52::x); // expected-warning{{2}}
  clang_analyzer_dump(der.y); // expected-warning{{3}}
}

void virtual_inheritance_diamond_default_init() {
  struct Base6 { int base = 1; };
  struct Med61 : virtual Base6 { int med61 = 2; };
  struct Med62 : virtual Base6 { int med62 = 3; };
  struct Derived6 : Med61, Med62 { int y = 4; };
  Derived6 der;
  // Here, each field is initialized separately,
  // so store contains 4 disjoint bindings.
  clang_analyzer_dump(der.base); // expected-warning{{1}}
  clang_analyzer_dump(der.med61); // expected-warning{{2}}
  clang_analyzer_dump(der.med62); // expected-warning{{3}}
  clang_analyzer_dump(der.y); // expected-warning{{4}}
}

struct Toaster { int capacity; };
struct TootbrushHolder { int handle; };

struct FancyToaster : Toaster, TootbrushHolder {
  int price;
  Toaster &asToaster() { return *this; }
  TootbrushHolder &asTootbrushHolder() { return *this; }
};

void lcv_multiple_inheriance_1st_base() {
  FancyToaster x;
  x.price = 3;
  x.asToaster() = Toaster{/*capacity=*/1};
  clang_analyzer_dump(x.capacity); // expected-warning{{1 S32b}}
  clang_analyzer_dump(x.asToaster()); // expected-warning-re{{lazyCompoundVal{0x{{[0-9a-f]+}},temp_object{Toaster, S{{[0-9]+}}}}}}
  clang_analyzer_dump(x.price); // expected-warning{{3 S32b}}
  clang_analyzer_dump(x); // expected-warning-re{{lazyCompoundVal{0x{{[0-9a-f]+}},x}}} FIXME This should trigger a partial undef copy warning.
}

void lcv_multiple_inheriance_2nd_base() {
  FancyToaster x;
  x.price = 3;
  x.asTootbrushHolder() = TootbrushHolder{/*handle*/};

  clang_analyzer_dump(x.handle); // expected-warning{{0 S32b}}}
  clang_analyzer_dump(x.asTootbrushHolder()); // expected-warning-re{{lazyCompoundVal{0x{{[0-9a-f]+}},temp_object{TootbrushHolder, S{{[0-9]+}}}}}}
  clang_analyzer_dump(x.price); // expected-warning{{3 S32b}}
  clang_analyzer_dump(x); // expected-warning-re{{lazyCompoundVal{0x{{[0-9a-f]+}},x}}} FIXME This should trigger a partial undef copy warning.
}

void lcv_multiple_inheriance_all_bases(int rng) {
  FancyToaster x;
  x.asToaster() = Toaster{/*capacity=*/1};
  x.asTootbrushHolder() = TootbrushHolder{/*handle*/};

  clang_analyzer_dump(x.capacity); // expected-warning{{1 S32b}}
  clang_analyzer_dump(x.handle); // expected-warning{{0 S32b}}
  clang_analyzer_dump(x.asToaster()); // expected-warning-re{{lazyCompoundVal{0x{{[0-9a-f]+}},temp_object{Toaster, S{{[0-9]+}}}}}}
  clang_analyzer_dump(x.asTootbrushHolder()); // expected-warning-re{{lazyCompoundVal{0x{{[0-9a-f]+}},temp_object{TootbrushHolder, S{{[0-9]+}}}}}}

  switch (rng) {
    case 10:
      clang_analyzer_dump(x); // expected-warning{{Passed-by-value struct argument contains uninitialized data (e.g., field: 'price')}}
      break;
    case 20:
      clang_analyzer_dump(x.price); // expected-warning{{1st function call argument is an uninitialized value}}
      break;
  }
}

void opaque(int* p);

void scalar_symbol_invalidation_on_opaque_call() {
  int x = 1;
  opaque(&x);
  clang_analyzer_dump(x); // expected-warning-re{{conj_${{[0-9]+}}{int, LC{{[0-9]+}}, S{{[0-9]+}}, #1}}}
}

struct Compound {
  int x;
};

void opaque(Compound* p);

void compound_symbol_invalidation_on_opaque_call() {
  Compound cpd = {1};
  opaque(&cpd);
  clang_analyzer_dump(cpd.x); // expected-warning-re{{derived_${{[0-9]+}}{conj_${{[0-9]+}}{Compound, LC{{[0-9]+}}, S{{[0-9]+}}, #1},cpd.x}}}
}

void array_symbol_invalidation_on_opaque_call() {
  int arr[2] = {1, 2};
  opaque(arr);
  // TODO: It is not as easy to conjure a symbol for default binding of an array,
  // SVal builder cannot symbolicate int[2].
  // Once properly invalidated, these should refer to derived_ conj_... symbols
  clang_analyzer_dump(arr[0]); // expected-warning{{Unknown}}
  clang_analyzer_dump(arr[1]); // expected-warning{{Unknown}}
}

struct CompoundNested1 {
  struct CompoundNested2 {
    struct CompoundNested3 {
      int x;
    } nested3;
  } nested2;
};

void opaque(CompoundNested1* p);

void compound_nested_symbol_invalidation_on_opaque_call() {
  CompoundNested1 s = {{{1}}};
  opaque(&s);
  clang_analyzer_dump(s.nested2.nested3.x); // expected-warning-re{{derived_${{[0-9]+}}{conj_${{[0-9]+}}{CompoundNested1, LC{{[0-9]+}}, S{{[0-9]+}}, #1},s.nested2.nested3.x}}}
}

void opaque(CompoundNested1::CompoundNested2* p);

void compound_nested_symbol_nested_invalidation_on_opaque_call() {
  CompoundNested1 s = {{{1}}};
  opaque(&s.nested2);
  clang_analyzer_dump(s.nested2.nested3.x); // expected-warning-re{{derived_${{[0-9]+}}{conj_${{[0-9]+}}{CompoundNested1, LC{{[0-9]+}}, S{{[0-9]+}}, #1},s.nested2.nested3.x}}}
}

struct CompoundOuter {
  struct CompoundInner {
    struct CompoundMostInner {
      int x[2];
    } mostInner[2];
  } nested;
};

void opaque(CompoundOuter* p);

void compound_nested_arr_symbol_invalidation_on_opaque_call() {
  CompoundOuter s = {{{{{1, 2}}, {{3, 4}}}}};
  opaque(&s);
  clang_analyzer_dump(s.nested); // expected-warning-re{{lazyCompoundVal{0x{{[0-9a-f]+}},s.nested}}}
  clang_analyzer_dump(s.nested.mostInner[0].x[0]); // expected-warning-re{{derived_${{[0-9]}}{conj_${{[0-9]+}}{CompoundOuter, LC{{[0-9]+}}, S{{[0-9]+}}, #1},Element{Element{s.nested.mostInner,0 S64b,struct CompoundOuter::CompoundInner::CompoundMostInner}.x,0 S64b,int}}}}
}

struct Base6 { int x; };
struct Middle6 : Base6 { int y; };
struct Derived6 : Middle6 { int z; };

void opaque(Derived6* p);

void inherited_struct_invalidation_on_opaque_call() {
  Derived6 s = {{{1}, 2}, 3};
  opaque(&s);
  clang_analyzer_dump(s.x); // expected-warning-re{{derived_${{[0-9]+}}{conj_${{[0-9]+}}{Derived6, LC{{[0-9]+}}, S{{[0-9]+}}, #1},Base{Base{s,Middle6},Base6}.x}}}
  clang_analyzer_dump(s.y); // expected-warning-re{{derived_${{[0-9]+}}{conj_${{[0-9]+}}{Derived6, LC{{[0-9]+}}, S{{[0-9]+}}, #1},Base{s,Middle6}.y}}}
}

void bind_address_of_locals_to_globals() {
  // This test needs iterBindings.
  int x = 1;
  global_x64 = (uint64_t)&x;
  // expected-warning@-1 {{Address of stack memory associated with local variable 'x' is still referred to by the global variable 'global_x64' upon returning to the caller.  This will be a dangling reference}}
}

struct PtrHolder { int *ptr; };

void opaque(PtrHolder p);

PtrHolder returning_ptr_inside_struct_no_leak() {
  PtrHolder result;
  result.ptr = (int*)malloc(4);
  return result; // no-warning
}

struct PtrArr { int *ptrs[2]; };

PtrArr returning_ptr_symbolic_inside_struct_no_leak(int idx) {
  PtrArr result;
  result.ptrs[idx] = (int*)malloc(4);
  return result; // no-warning
}

void opaque();

PtrArr returning_ptr_symbolic_inside_struct_opaque_no_leak(int idx) {
  PtrArr result;
  result.ptrs[idx] = (int*)malloc(4);
  opaque(); // expected-warning{{Potential memory leak}} FIXME why??
  return result;
}

void ptr_through_lcv_invalidation_on_opaque_call() {
  int x = 11;
  PtrHolder s = {&x};
  opaque(s);
  clang_analyzer_dump(x); // expected-warning-re{{conj_${{[0-9]+}}{int, LC{{[0-9]+}}, S{{[0-9]+}}, #1}}}
}

struct PtrPtrHolder {
  uint8_t **ptrptr;
  PtrHolder ph;
  struct Nested {
    uint8_t *ptr;
  } nested;
};

void opaque(PtrPtrHolder p);

void complex_lcv_invalidation_on_opaque_call() {
  uint8_t x = 11;
  uint8_t *y = &x;
  int z = 12;
  uint8_t w = 13;
  PtrPtrHolder s = {&y, {nullptr}, {&w}};
  s.ph = {&z}; // Create LCV
  opaque(s); // s.ph LCV is now nested in the LCV representing copy of s
  clang_analyzer_dump(y); // expected-warning-re{{&SymRegion{conj_${{[0-9]}}{uint8_t *, LC{{[0-9]}}, S{{[0-9]+}}, #1}}}}
  clang_analyzer_dump(x); // expected-warning-re{{conj_${{[0-9]+}}{uint8_t, LC{{[0-9]+}}, S{{[0-9]+}}, #1}}}
  clang_analyzer_dump(z); // expected-warning-re{{conj_${{[0-9]+}}{int, LC{{[0-9]+}}, S{{[0-9]+}}, #1}}}
  clang_analyzer_dump(w); // expected-warning-re{{conj_${{[0-9]+}}{uint8_t, LC{{[0-9]+}}, S{{[0-9]+}}, #1}}}
}
