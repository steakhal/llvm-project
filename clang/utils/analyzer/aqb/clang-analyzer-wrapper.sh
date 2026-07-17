#!/usr/bin/env bash
# AQB analyzer-clang wrapper. Stands in as $CC / scan-build --use-analyzer so
# each per-TU clang *analysis* process writes entry-point stats to a UNIQUE
# (PID-named) CSV under $AQB_EP_CSV_DIR — avoiding the single-shared-path clobber
# where every TU truncates one file (EntryPointStats.cpp uses OF_Text).
#
# scan-build's ccc-analyzer invokes the analyzer clang ($ENV{CLANG}, i.e. this
# wrapper) TWICE per TU: first `clang -### --analyze …` to expand the frontend
# command, then `clang -cc1 … -analyze …` to actually analyze. We inject only on
# the second (the real `-cc1 … -analyze`), using the cc1-native form
# `-analyzer-config key=val` (NOT `-Xclang …`, which is a driver-only flag and
# is rejected by `-cc1`). Every other invocation — the `-### --analyze` probe,
# plain compiles, `--version` — passes through untouched so scan-build's own
# probing still sees a real clang.
set -euo pipefail

real="${AQB_REAL_CLANG:?AQB_REAL_CLANG is required}"

has_cc1=0
has_analyze=0
for arg in "$@"; do
    case "$arg" in
    -cc1) has_cc1=1 ;;
    -analyze) has_analyze=1 ;;
    esac
done

if [ "$has_cc1" = "1" ] && [ "$has_analyze" = "1" ] && [ -n "${AQB_EP_CSV_DIR:-}" ]; then
    mkdir -p "$AQB_EP_CSV_DIR"
    exec "$real" "$@" \
        -analyzer-config "dump-entry-point-stats-to-csv=$AQB_EP_CSV_DIR/$$.csv"
fi

exec "$real" "$@"
