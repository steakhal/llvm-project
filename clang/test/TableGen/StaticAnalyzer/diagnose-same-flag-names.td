// RUN: not clang-tblgen -gen-clang-sa-configs -I%S %s -o /dev/null 2>%t
// RUN: FileCheck --input-file=%t %s

include "ConfigValueBase.inc"

def Foo : BooleanConfigValue {
  let DefaultValue = 0;
  let FlagName = "foo";
  let ShortDescription = "Foo description.";
  let Category = DummyCategory;
}

def Bar : BooleanConfigValue {
  let DefaultValue = 0;
  let FlagName = "foo";
  // CHECK: [[@LINE-1]]:7: error: The value of `FlagName' has been already used!
  // CHECK: [[@LINE-9]]:7: note: Previously used by the `Foo' record.
  let ShortDescription = "Bar description.";
  let Category = DummyCategory;
}
