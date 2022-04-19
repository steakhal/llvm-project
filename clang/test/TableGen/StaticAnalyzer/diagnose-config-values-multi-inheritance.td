// RUN: not clang-tblgen -gen-clang-sa-configs -I%S %s -o /dev/null 2>%t
// RUN: FileCheck --input-file=%t %s

include "ConfigValueBase.inc"

class MyFancyOtherClass {
  int Height;
}

// CHECK: [[@LINE+1]]:1: error: Record `DummyFlag' should inherit from only a single Record!
def DummyFlag : BooleanConfigValue, MyFancyOtherClass {
  let FlagName = "dummy-flag";
  let ShortDescription = "Dummy flag.";
  let DefaultValue = 0;
  let Category = DummyCategory;
  let Height = 42;
}
