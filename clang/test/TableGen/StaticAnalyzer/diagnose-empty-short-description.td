// RUN: not clang-tblgen -gen-clang-sa-configs -I%S %s -o /dev/null 2>%t
// RUN: FileCheck --input-file=%t %s

include "ConfigValueBase.inc"

def EmptyFlagName : BooleanConfigValue {
  let DefaultValue = 0;
  let FlagName = "empty";
  // CHECK: [[@LINE+1]]:7: error: `ShortDescription' should not be empty.
  let ShortDescription = "";
  let Category = DummyCategory;
}
