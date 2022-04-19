// RUN: not clang-tblgen -gen-clang-sa-configs -I%S %s -o /dev/null 2>%t
// RUN: FileCheck --input-file=%t %s

include "ConfigValueBase.inc"

def Foo : IntConfigValue {
  // CHECK: [[@LINE-1]]:1: error: The field named `DefaultValue' must be smaller or equal to the field named `Max'!
  // CHECK: [[@LINE+3]]:7: note: `DefaultValue' is 3
  // CHECK: [[@LINE+1]]:7: note: `Max' is 2
  let Max = 2;
  let DefaultValue = 3;
  let FlagName = "foo";
  let ShortDescription = "Description.";
  let Category = DummyCategory;
}
