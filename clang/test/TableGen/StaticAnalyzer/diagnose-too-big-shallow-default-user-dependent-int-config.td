// RUN: not clang-tblgen -gen-clang-sa-configs -I%S %s -o /dev/null 2>%t
// RUN: FileCheck --input-file=%t %s

include "ConfigValueBase.inc"

def Foo : UserModeDependentIntConfigValue {
  // CHECK: [[@LINE-1]]:1: error: The field named `ShallowMin' must be smaller or equal to the field named `ShallowDefaultValue'!
  // CHECK: [[@LINE+2]]:7: note: `ShallowMin' is 2
  // CHECK: [[@LINE+3]]:7: note: `ShallowDefaultValue' is 0
  let ShallowMin = 2;
  let DeepMin = 2;
  let ShallowDefaultValue = 0;
  let DeepDefaultValue = 2;
  let FlagName = "foo";
  let ShortDescription = "Description.";
  let Category = DummyCategory;
}
