from __future__ import annotations

import os
import tempfile
import unittest

from aqb.analyze import (
    EP_CSV_NAME,
    analyze_run_argv,
    analyzer_config,
    collect_entry_point_csvs,
    collect_plists,
)


class AnalyzerConfigTest(unittest.TestCase):
    def test_includes_entry_point_csv(self):
        cfg = analyzer_config("/out/ep.csv")
        self.assertIn("dump-entry-point-stats-to-csv=/out/ep.csv", cfg)

    def test_appends_extra(self):
        cfg = analyzer_config("/out/ep.csv", extra="max-nodes=0")
        parts = cfg.split(",")
        self.assertIn("dump-entry-point-stats-to-csv=/out/ep.csv", parts)
        self.assertIn("max-nodes=0", parts)

    def test_ep_csv_name_is_a_plain_filename(self):
        self.assertNotIn("/", EP_CSV_NAME)


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

    def test_collect_entry_point_csvs_recursive(self):
        with tempfile.TemporaryDirectory() as root:
            csv = os.path.join(root, "curl", EP_CSV_NAME)
            os.makedirs(os.path.dirname(csv))
            open(csv, "w").close()
            self.assertEqual(collect_entry_point_csvs(root), [csv])


class AnalyzeRunArgvTest(unittest.TestCase):
    def _argv(self):
        return analyze_run_argv(
            clang_volume="aqb-clang-x-y",
            projects_dir="/host/projects",
            scripts_dir="/host/scripts",
            ccache_volume="aqb-ccache",
            image="aqb-clang-builder:latest",
            projects=["curl", "redis"],
            ep_csv_path="/projects/ep.csv",
        )

    def test_mounts_clang_volume_readonly_at_analyzer(self):
        self.assertIn("aqb-clang-x-y:/analyzer:ro", self._argv())

    def test_invokes_satest_regenerate_with_comma_projects_and_config(self):
        argv = self._argv()
        self.assertIn("build", argv)
        self.assertIn("-r", argv)  # regenerate (analyze-only, no compare)
        self.assertEqual(argv[argv.index("--projects") + 1], "curl,redis")
        cfg = argv[argv.index("--extra-analyzer-config") + 1]
        self.assertIn("dump-entry-point-stats-to-csv=/projects/ep.csv", cfg)

    def test_resource_limits_and_workdir(self):
        argv = self._argv()
        self.assertEqual(argv[argv.index("-m") + 1], "24G")
        self.assertEqual(argv[argv.index("--cpus") + 1], "8")
        self.assertEqual(argv[argv.index("-w") + 1], "/projects")
