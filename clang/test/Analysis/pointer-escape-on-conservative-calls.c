// RUN: %clang_analyze_cc1 -analyzer-checker=debug.AnalysisOrder \
// RUN:   -analyzer-config debug.AnalysisOrder:PointerEscape=true,debug.AnalysisOrder:PostCall=true %s 2>&1 | FileCheck %s


void f(int *);
int *getMem();

int main() {
    f(getMem());
    return 0;
}

// CHECK: PostCall (f)
// CHECK-NEXT: PointerEscape
