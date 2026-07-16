from __future__ import annotations

import json
from typing import Optional

AQB_PRESET_VERSION = 3
AQB_BASE_PRESET_NAME = "aqb-base"

# Self-contained so it works at any pinned commit (does not depend on LLVM's own
# presets existing). AQB always overrides binaryDir (-B) and CMAKE_INSTALL_PREFIX
# (-D) on the command line, but they are set here too as sane defaults. The cache
# variables mirror SATest's build recipe (clang/utils/analyzer/entrypoint.py):
# host target only, lld linker, and the static analyzer explicitly enabled --
# plus LLVM_ENABLE_ASSERTIONS, which AQB keeps ON so analyzer invariant
# violations surface as assertion failures (a qualification robustness signal).
AQB_BASE_PRESET = {
    "name": AQB_BASE_PRESET_NAME,
    "displayName": "AQB base clang build",
    "generator": "Ninja",
    "binaryDir": "/tmp/build",
    "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Release",
        "LLVM_ENABLE_PROJECTS": "clang",
        "LLVM_TARGETS_TO_BUILD": "host",
        "CLANG_ENABLE_STATIC_ANALYZER": "ON",
        "LLVM_ENABLE_LLD": "ON",
        "LLVM_ENABLE_ASSERTIONS": "ON",
        "LLVM_CCACHE_BUILD": "ON",
        "CMAKE_INSTALL_PREFIX": "/opt/aqb/clang",
    },
}


def assemble_user_presets(user_overlay_json: Optional[str]) -> str:
    """Return the ``CMakeUserPresets.json`` content = ``aqb-base`` + the user's
    presets, as canonical JSON (sorted keys) so identical inputs yield identical
    bytes (this is the volume-digest input).

    The user's configure presets are appended after ``aqb-base`` and may
    ``inherits: aqb-base``. LLVM's own tracked ``CMakePresets.json`` is left
    untouched, so a user preset may also inherit ``llvm-*`` presets.
    """
    version = AQB_PRESET_VERSION
    configure_presets = [dict(AQB_BASE_PRESET)]
    if user_overlay_json:
        overlay = json.loads(user_overlay_json)
        version = max(version, int(overlay.get("version", version)))
        configure_presets.extend(overlay.get("configurePresets", []))
    doc = {"version": version, "configurePresets": configure_presets}
    return json.dumps(doc, sort_keys=True, indent=2)
