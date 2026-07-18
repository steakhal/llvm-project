from __future__ import annotations

import os
import tempfile
import unittest

from aqb.analyze import (
    EP_CSV_DIR_NAME,
    WRAPPER,
    analyze_run_argv,
    collect_entry_point_csvs,
    collect_plists,
    merge_entry_point_csvs,
)


class CollectTest(unittest.TestCase):
    def test_collect_plists_finds_ref_scan_build_results(self):
        with tempfile.TemporaryDirectory() as root:
            plist = os.path.join(root, "curl", "RefScanBuildResults", "r1", "a.plist")
            os.makedirs(os.path.dirname(plist))
            open(plist, "w").close()
            # A non-plist and a wrong-dir file should be ignored.
            open(
                os.path.join(root, "curl", "RefScanBuildResults", "r1", "b.txt"), "w"
            ).close()
            self.assertEqual(collect_plists(root), [plist])

    def test_collect_entry_point_csvs_finds_pid_files(self):
        with tempfile.TemporaryDirectory() as root:
            ep_dir = os.path.join(root, EP_CSV_DIR_NAME)
            os.makedirs(ep_dir)
            a = os.path.join(ep_dir, "111.csv")
            b = os.path.join(ep_dir, "222.csv")
            open(a, "w").close()
            open(b, "w").close()
            # A non-csv sibling should be ignored.
            open(os.path.join(ep_dir, "notes.txt"), "w").close()
            self.assertEqual(collect_entry_point_csvs(root), [a, b])

    def test_merge_entry_point_csvs(self):
        with tempfile.TemporaryDirectory() as d:
            a = os.path.join(d, "1.csv")
            b = os.path.join(d, "2.csv")
            with open(a, "w") as f:
                f.write("USR,File,DebugName\nu1,f1,d1\n")
            with open(b, "w") as f:
                f.write("USR,File,DebugName\nu2,f2,d2\n")
            merged = merge_entry_point_csvs([a, b])
            self.assertEqual(merged[0], "USR,File,DebugName")
            self.assertIn("u1,f1,d1", merged)
            self.assertIn("u2,f2,d2", merged)
            self.assertEqual(len(merged), 3)  # header + 2 rows


class AnalyzeRunArgvTest(unittest.TestCase):
    def _argv(self, **overrides):
        kwargs = dict(
            clang_volume="aqb-clang-x-y",
            projects_dir="/host/projects",
            scripts_dir="/host/scripts",
            ccache_volume="aqb-ccache",
            image="aqb-clang-builder:latest",
            projects=["curl", "redis"],
        )
        kwargs.update(overrides)
        return analyze_run_argv(**kwargs)

    def test_mounts_clang_volume_readonly_at_analyzer(self):
        self.assertIn("aqb-clang-x-y:/analyzer:ro", self._argv())

    def test_invokes_analyze_driver_with_comma_projects(self):
        argv = self._argv()
        # AQB drives its own analyze_driver.py (reference build, no compare),
        # NOT `SATest.py build`.
        self.assertTrue(any(a.endswith("analyze_driver.py") for a in argv))
        self.assertNotIn("build", argv)
        self.assertNotIn("-r", argv)
        self.assertEqual(argv[argv.index("--projects") + 1], "curl,redis")

    def test_wrapper_env_wires_cc_real_clang_and_ep_dir(self):
        argv = self._argv()
        # CC is the wrapper (SATest uses it as --use-analyzer); the wrapper
        # execs the real clang and writes a per-TU CSV into AQB_EP_CSV_DIR.
        self.assertIn(f"CC=/scripts/{WRAPPER}", argv)
        self.assertIn("AQB_REAL_CLANG=/analyzer/bin/clang", argv)
        self.assertIn(f"AQB_EP_CSV_DIR=/projects/{EP_CSV_DIR_NAME}", argv)

    def test_custom_ep_csv_dir_overrides_default(self):
        argv = self._argv(ep_csv_dir="/projects/aqb-entry-point-stats/iter-3")
        self.assertIn("AQB_EP_CSV_DIR=/projects/aqb-entry-point-stats/iter-3", argv)

    def test_extra_config_passed_through(self):
        argv = self._argv(extra_config="max-nodes=0")
        self.assertEqual(argv[argv.index("--extra-analyzer-config") + 1], "max-nodes=0")

    def test_resource_limits_and_workdir(self):
        argv = self._argv()
        self.assertEqual(argv[argv.index("-m") + 1], "20G")
        self.assertEqual(argv[argv.index("--cpus") + 1], "8")
        self.assertEqual(argv[argv.index("-w") + 1], "/projects")


if __name__ == "__main__":
    unittest.main()
