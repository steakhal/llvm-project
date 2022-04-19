// RUN: not clang-tblgen -gen-clang-sa-configs -I%S %s -o /dev/null 2>%t
// RUN: FileCheck --input-file=%t %s

include "ConfigValueBase.inc"

def Foo : UserModeDependentEnumConfigValue {
  let FlagName = "foo";
  let ShortDescription = "Foo description.";
  let Category = DummyCategory;
  let ShallowDefaultValue = "small";
  let DeepDefaultValue = "big";
  let Options = ["something", "small"];
  // CHECK: [[@LINE-1]]:7: error: The field named `Options' must contain the value of `DeepDefaultValue'!
  // CHECK: [[@LINE-3]]:7: note: `DeepDefaultValue' is "big"
}
