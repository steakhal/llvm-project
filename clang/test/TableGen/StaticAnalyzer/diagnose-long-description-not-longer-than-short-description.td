// RUN: not clang-tblgen -gen-clang-sa-configs -I%S %s -o /dev/null 2>%t
// RUN: FileCheck --input-file=%t %s

include "ConfigValueBase.inc"

def Foo : BooleanConfigValue {
  let DefaultValue = 0;
  let FlagName = "foo";
  let ShortDescription = "Foo description.";
  let LongDescription  = "Foo description.";
  // CHECK: [[@LINE-1]]:7: error: The `LongDescription' field must be longer than the `ShortDescription' field!
  let Category = DummyCategory;
}
