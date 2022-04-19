// RUN: not clang-tblgen -gen-clang-sa-configs -I%S %s -o /dev/null 2>%t
// RUN: FileCheck --input-file=%t %s

include "ConfigValueBase.inc"

def Foo : BooleanConfigValue {
  let DefaultValue = 0;
  let FlagName = "foo";
  let ShortDescription = "Foo description.";
  let Category = DummyCategory;
}

def EmptyFlagName : BooleanConfigValue {
  let DefaultValue = 0;
  let FlagName = "empty";
  let ShortDescription = "Description.";
  let Category = DummyCategory;
  // CHECK: [[@LINE+1]]:7: error: "Foo" appears more than once in the list!
  let RelatedConfigs = ["Foo", "Foo"];
}
