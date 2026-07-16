from __future__ import annotations

import json
import unittest

from aqb.presets import AQB_BASE_PRESET_NAME, assemble_user_presets


class AssembleUserPresetsTest(unittest.TestCase):
    def test_base_only_when_no_overlay(self):
        doc = json.loads(assemble_user_presets(None))
        names = [p["name"] for p in doc["configurePresets"]]
        self.assertEqual(names, [AQB_BASE_PRESET_NAME])
        base = doc["configurePresets"][0]
        self.assertEqual(base["cacheVariables"]["CMAKE_BUILD_TYPE"], "Release")
        self.assertEqual(base["cacheVariables"]["LLVM_ENABLE_PROJECTS"], "clang")

    def test_overlay_presets_appended_after_base(self):
        overlay = json.dumps(
            {
                "version": 6,
                "configurePresets": [
                    {
                        "name": "mine",
                        "inherits": "aqb-base",
                        "cacheVariables": {"LLVM_ENABLE_ASSERTIONS": "OFF"},
                    }
                ],
            }
        )
        doc = json.loads(assemble_user_presets(overlay))
        names = [p["name"] for p in doc["configurePresets"]]
        self.assertEqual(names, ["aqb-base", "mine"])
        self.assertGreaterEqual(doc["version"], 6)

    def test_output_is_canonical_and_stable(self):
        a = assemble_user_presets(None)
        b = assemble_user_presets(None)
        self.assertEqual(a, b)
        self.assertEqual(a, json.dumps(json.loads(a), sort_keys=True, indent=2))
