from __future__ import annotations

import os
import subprocess
import unittest

BUILDER_DIR = os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "builder"
)


class BuilderImageTest(unittest.TestCase):
    def test_build_script_has_valid_bash_syntax(self):
        result = subprocess.run(
            ["bash", "-n", os.path.join(BUILDER_DIR, "build.sh")],
            capture_output=True,
            text=True,
        )
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_build_script_contract(self):
        with open(os.path.join(BUILDER_DIR, "build.sh")) as handle:
            body = handle.read()
        self.assertIn("/opt/aqb/clang", body)
        self.assertIn("/ccache", body)
        self.assertIn("CMakeUserPresets.json", body)
        self.assertIn("AQB_USER_PRESETS_JSON", body)
        self.assertIn("cmake --preset", body)
        self.assertIn(".aqb-complete", body)
        # scan-build is the analyze driver AQB runs later; install-clang alone
        # doesn't install it.
        self.assertIn("install-scan-build", body)

    def test_dockerfile_wires_the_build_script(self):
        with open(os.path.join(BUILDER_DIR, "Dockerfile")) as handle:
            body = handle.read()
        self.assertIn("build.sh", body)
        self.assertIn("ENTRYPOINT", body)
