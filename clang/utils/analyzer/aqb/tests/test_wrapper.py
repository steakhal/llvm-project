from __future__ import annotations

import os
import subprocess
import tempfile
import unittest

WRAPPER = os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
    "clang-analyzer-wrapper.sh",
)


def _run(args, ep_dir=None):
    env = dict(os.environ, AQB_REAL_CLANG="/bin/echo")
    if ep_dir is not None:
        env["AQB_EP_CSV_DIR"] = ep_dir
    else:
        env.pop("AQB_EP_CSV_DIR", None)
    return subprocess.run(
        [WRAPPER, *args], env=env, capture_output=True, text=True, check=True
    ).stdout


class WrapperTest(unittest.TestCase):
    def test_injects_cc1_native_config_on_real_analysis(self):
        # scan-build's real analysis is `clang -cc1 … -analyze …`. Inject there,
        # cc1-native (no -Xclang), with a PID-unique CSV path.
        with tempfile.TemporaryDirectory() as d:
            out = _run(["-cc1", "-analyze", "foo.c"], ep_dir=d)
            self.assertIn("-analyzer-config", out)
            self.assertIn("dump-entry-point-stats-to-csv=", out)
            self.assertIn(d, out)
            self.assertIn(".csv", out)
            self.assertNotIn("-Xclang", out)  # -Xclang is invalid under -cc1

    def test_passthrough_on_driver_analyze_probe(self):
        # The `-### --analyze` probe (driver mode, no -cc1) must pass through so
        # scan-build captures a clean frontend command; we only inject on -cc1.
        with tempfile.TemporaryDirectory() as d:
            out = _run(["-###", "--analyze", "foo.c"], ep_dir=d)
            self.assertNotIn("dump-entry-point-stats-to-csv", out)

    def test_passthrough_on_compile(self):
        with tempfile.TemporaryDirectory() as d:
            out = _run(["-c", "foo.c", "-o", "foo.o"], ep_dir=d)
            self.assertNotIn("dump-entry-point-stats-to-csv", out)

    def test_no_injection_without_ep_dir(self):
        out = _run(["-cc1", "-analyze", "foo.c"], ep_dir=None)
        self.assertNotIn("dump-entry-point-stats-to-csv", out)


if __name__ == "__main__":
    unittest.main()
