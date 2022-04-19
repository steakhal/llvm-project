// RUN: not clang-tblgen -gen-clang-sa-configs -I%S %s -o /dev/null 2>%t
// RUN: FileCheck --input-file=%t %s

include "ConfigValueBase.inc"

def Foo : BooleanConfigValue {
  let DefaultValue = 0;
  let FlagName = "foo";
  let ShortDescription = "Foo description.";
  let LongDescription = "Foo long description.";
  let Category = DummyCategory;
}

def Bar : BooleanConfigValue {
  let DefaultValue = 0;
  let FlagName = "bar";
  let ShortDescription = "Bar description.";
  let LongDescription = "Foo long description.";
  // CHECK: [[@LINE-1]]:7: error: The value of `LongDescription' has been already used!
  // CHECK: [[@LINE-10]]:7: note: Previously used by the `Foo' record.
  let Category = DummyCategory;
}
