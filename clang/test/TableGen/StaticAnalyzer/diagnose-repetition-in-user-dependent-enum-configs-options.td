// RUN: not clang-tblgen -gen-clang-sa-configs -I%S %s -o /dev/null 2>%t
// RUN: FileCheck --input-file=%t %s

include "ConfigValueBase.inc"

def Foo : UserModeDependentEnumConfigValue {
  let FlagName = "foo";
  let ShortDescription = "Foo description.";
  let Category = DummyCategory;
  let ShallowDefaultValue = "small";
  let DeepDefaultValue = "big";
  let Options = ["something", "small", "big", "something"];
  // CHECK: [[@LINE-1]]:7: error: "something" appears more than once in the list!
}
