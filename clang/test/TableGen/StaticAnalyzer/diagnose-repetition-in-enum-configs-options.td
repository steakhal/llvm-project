// RUN: not clang-tblgen -gen-clang-sa-configs -I%S %s -o /dev/null 2>%t
// RUN: FileCheck --input-file=%t %s

include "ConfigValueBase.inc"

def Foo : EnumConfigValue {
  let FlagName = "foo";
  let ShortDescription = "Foo description.";
  let Category = DummyCategory;
  let DefaultValue = "small";
  let Options = ["small", "medium", "big", "small"];
  // CHECK: [[@LINE-1]]:7: error: "small" appears more than once in the list!
}
