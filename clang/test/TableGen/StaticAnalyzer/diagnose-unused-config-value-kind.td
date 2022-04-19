// RUN: clang-tblgen -gen-clang-sa-configs -I%S %s -o /dev/null 2>%t
// RUN: FileCheck --input-file=%t %s

include "ConfigValueBase.inc"

// CHECK: warning: Record `IntConfigValue' is unused; consider removing it.
