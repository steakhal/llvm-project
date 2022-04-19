// RUN: not clang-tblgen -gen-clang-sa-configs -I%S %s -o /dev/null 2>%t
// RUN: FileCheck --input-file=%t %s

include "ConfigValueBase.inc"

class NewConfigType : ConfigValue {
  int Member;
}

// CHECK: [[@LINE+1]]:1: error: Record `NewConfigType' is unhandled by the "gen-clang-sa-configs" tablegen backend!
def NewConfig : NewConfigType {
  let Member = 44;
  let Category = DummyCategory;
}
