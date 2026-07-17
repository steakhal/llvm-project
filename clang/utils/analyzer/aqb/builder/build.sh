#!/usr/bin/env bash
# AQB Clang Volume builder. Reads the AQB_* env contract (see aqb/volume.py):
# builds clang from AQB_COMMIT (fetched from AQB_SOURCE) using the CMake preset
# AQB_PRESET, and installs it into the mounted clang volume. AQB_USER_PRESETS_JSON
# is the assembled CMakeUserPresets.json (aqb-base + any user overlay). The mount
# paths are fixed contract constants shared with volume.py. A completion marker
# is written last so an interrupted build is never mistaken for a valid cache.
set -euo pipefail

: "${AQB_COMMIT:?AQB_COMMIT is required}"
: "${AQB_SOURCE:?AQB_SOURCE is required}"
: "${AQB_PRESET:?AQB_PRESET is required}"
: "${AQB_USER_PRESETS_JSON:?AQB_USER_PRESETS_JSON is required}"

# Contract constants (must match volume.py CLANG_INSTALL_MOUNT / CCACHE_MOUNT).
INSTALL_DIR=/opt/aqb/clang
export CCACHE_DIR=/ccache

SRC=/tmp/llvm-project
git clone "$AQB_SOURCE" "$SRC"
git -C "$SRC" checkout --detach "$AQB_COMMIT"

# LLVM's top-level CMakeLists.txt (and its tracked CMakePresets.json) live in
# llvm/. Drop the assembled user presets alongside them; a fresh clone has no
# CMakeUserPresets.json, so this never clobbers LLVM's tracked file.
printf '%s' "$AQB_USER_PRESETS_JSON" > "$SRC/llvm/CMakeUserPresets.json"

cd "$SRC/llvm"
# Force AQB's build dir and install prefix regardless of preset content
# (command-line -B / -D override preset values).
cmake --preset "$AQB_PRESET" -B /tmp/build \
    -D CMAKE_INSTALL_PREFIX="$INSTALL_DIR" \
    -D LLVM_CCACHE_BUILD=ON
# Install clang, its resource headers, and scan-build (the analyze driver AQB's
# `run` invokes via PATH=/analyzer/bin). `install-clang` alone does NOT install
# scan-build, so it must be requested explicitly.
ninja -C /tmp/build \
    install-clang \
    install-clang-resource-headers \
    install-scan-build \
    install-scan-build-py

# Mark the install tree complete only after a fully successful install.
touch "$INSTALL_DIR/.aqb-complete"
