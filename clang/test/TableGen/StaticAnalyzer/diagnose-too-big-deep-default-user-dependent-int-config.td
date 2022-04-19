// RUN: not clang-tblgen -gen-clang-sa-configs -I%S %s -o /dev/null 2>%t
// RUN: FileCheck --input-file=%t %s

include "ConfigValueBase.inc"

def Foo : UserModeDependentIntConfigValue {
  // CHECK: [[@LINE-1]]:1: error: The field named `DeepMin' must be smaller or equal to the field named `DeepDefaultValue'!
  // CHECK: [[@LINE+3]]:7: note: `DeepMin' is 2
  // CHECK: [[@LINE+4]]:7: note: `DeepDefaultValue' is 0
  let ShallowMin = 2;
  let DeepMin = 2;
  let ShallowDefaultValue = 2;
  let DeepDefaultValue = 0;
  let FlagName = "foo";
  let ShortDescription = "Description.";
  let Category = DummyCategory;
}
