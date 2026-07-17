#!/usr/bin/env bash
# AQB analyzer-clang wrapper. Stands in as $CC / scan-build --use-analyzer so
# each per-TU clang *analysis* process writes entry-point stats to a UNIQUE
# (PID-named) CSV under $AQB_EP_CSV_DIR — avoiding the single-shared-path clobber
# where every TU truncates one file (EntryPointStats.cpp uses OF_Text). Non-
# analysis invocations (plain compiles) pass through untouched.
set -euo pipefail

real="${AQB_REAL_CLANG:?AQB_REAL_CLANG is required}"

is_analysis=0
for arg in "$@"; do
    if [ "$arg" = "--analyze" ] || [ "$arg" = "-analyze" ]; then
        is_analysis=1
        break
    fi
done

if [ "$is_analysis" = "1" ] && [ -n "${AQB_EP_CSV_DIR:-}" ]; then
    mkdir -p "$AQB_EP_CSV_DIR"
    exec "$real" "$@" \
        -Xclang -analyzer-config \
        -Xclang "dump-entry-point-stats-to-csv=$AQB_EP_CSV_DIR/$$.csv"
fi

exec "$real" "$@"
