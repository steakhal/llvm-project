// REQUIRES: plugins

// rm -rf %t.{ast,usrs,overriders}

// Load the plugin and generate the 3 outputs:
// Note: The -o is required so that the plugin could create their outputs with the file extension replaced.
//   RUN: %clang_cc1 %s \
//   RUN:   -o %t.output \
//   RUN:   -load %llvmshlibdir/AnalysisPlugin%pluginext \
//   RUN:   -add-plugin dump-the-ast \
//   RUN:   -add-plugin dump-the-usrs \
//   RUN:   -add-plugin dump-the-overriders

// Check if the 3 plugin actions produced the expected files:
//   RUN: %clang_cc1 -ast-dump -ast-dump-filter Derived -ast-dump-filter Derived::virtualmethod %t.ast 2>&1 | FileCheck %s --check-prefix=AST
//     AST:   CXXMethodDecl {{.*}} imported virtualmethod 'int ()'
//   RUN: FileCheck %s --check-prefix=USRS --input-file=%t.usrs
//     USRS:  c:@S@Derived@F@virtualmethod#
//   RUN: FileCheck %s --check-prefix=OVERRIDERS --input-file=%t.overriders
//     OVERRIDERS:      1 c:@S@Base@F@virtualmethod#
//     OVERRIDERS-NEXT: c:@S@Derived@F@virtualmethod#

// Check if the plugins report the missing '-o' flag.
//   RUN: %clang_cc1 %s \
//   RUN:   -load %llvmshlibdir/AnalysisPlugin%pluginext \
//   RUN:   -add-plugin dump-the-ast \
//   RUN:   -add-plugin dump-the-usrs \
//   RUN:   -add-plugin dump-the-overriders \
//   RUN:  2>&1 | FileCheck --check-prefix=MISSING-OUT-FLAG %s
//   MISSING-OUT-FLAG:       Plugin 'dump-the-ast' requires the -o flag.
//   MISSING-OUT-FLAG-NEXT:  Plugin 'dump-the-usrs' requires the -o flag.
//   MISSING-OUT-FLAG-NEXT:  Plugin 'dump-the-overriders' requires the -o flag.

class Base {
public:
  virtual ~Base() = default;
  virtual int virtualmethod() { return 1; }
};

class Derived : public Base {
public:
  int virtualmethod() override { return 2; }
};
