// RUN: not clang-tblgen -gen-clang-sa-configs -I%S %s -o /dev/null 2>%t
// RUN: FileCheck --input-file=%t %s

include "ConfigValueBase.inc"

def EmptyFlagName : BooleanConfigValue {
  let DefaultValue = 0;
  let FlagName = "empty";
  let ShortDescription = "Description.";
  let Category = DummyCategory;
  // CHECK: [[@LINE+1]]:7: error: Should not refer to itself.
  let RelatedConfigs = ["EmptyFlagName"];
}
