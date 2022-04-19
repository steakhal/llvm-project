// RUN: not clang-tblgen -gen-clang-sa-configs -I%S %s -o /dev/null 2>%t
// RUN: FileCheck --input-file=%t %s

include "ConfigValueBase.inc"

def EmptyFlagName : BooleanConfigValue {
  // CHECK: [[@LINE+1]]:7: error: `FlagName' should not be empty.
  let FlagName = "";
  let DefaultValue = 0;
  let ShortDescription = "Description.";
  let Category = DummyCategory;
}
