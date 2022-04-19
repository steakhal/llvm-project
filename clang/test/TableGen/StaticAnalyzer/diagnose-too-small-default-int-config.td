// RUN: not clang-tblgen -gen-clang-sa-configs -I%S %s -o /dev/null 2>%t
// RUN: FileCheck --input-file=%t %s

include "ConfigValueBase.inc"

def Foo : IntConfigValue {
  // CHECK: [[@LINE-1]]:1: error: The field named `Min' must be smaller or equal to the field named `DefaultValue'!
  // CHECK: [[@LINE+2]]:7: note: `Min' is 2
  // CHECK: [[@LINE+2]]:7: note: `DefaultValue' is 0
  let Min = 2;
  let DefaultValue = 0;
  let FlagName = "foo";
  let ShortDescription = "Description.";
  let Category = DummyCategory;
}
