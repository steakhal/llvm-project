from __future__ import annotations

import contextlib
import io
import tempfile
import unittest
from unittest import mock

from aqb.cli import main
from aqb.metadata import (
    AnalyzerProvenance,
    ContainerProvenance,
    ExecutionProvenance,
    Metadata,
)
from aqb.store import RunStore


class CliTest(unittest.TestCase):
    def test_no_command_prints_help_and_exits_zero(self):
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            code = main([])
        self.assertEqual(code, 0)
        self.assertIn("usage: aqb", out.getvalue())

    def test_stub_command_reports_not_implemented(self):
        err = io.StringIO()
        with contextlib.redirect_stderr(err):
            code = main(["run"])
        self.assertEqual(code, 2)
        self.assertIn("not yet implemented", err.getvalue())

    def test_list_prints_stored_run_ids(self):
        with tempfile.TemporaryDirectory() as root:
            RunStore(root).create_run(
                Metadata(
                    run_id="r-xyz",
                    kind="functional",
                    created="2026-07-16T00:00:00+00:00",
                    analyzer=AnalyzerProvenance(commit="c"),
                    container=ContainerProvenance(),
                    execution=ExecutionProvenance(),
                )
            )
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                code = main(["--home", root, "list"])
            self.assertEqual(code, 0)
            self.assertIn("r-xyz", out.getvalue())


class BuildClangCliTest(unittest.TestCase):
    def test_invokes_orchestration_with_preset(self):
        from aqb.volume import ClangVolume

        captured = {}

        def fake_build(runtime, **kwargs):
            captured.update(kwargs)
            return ClangVolume(name="aqb-clang-x-y", config_digest="y", built=True)

        out = io.StringIO()
        with mock.patch(
            "aqb.cli.build_clang_volume", fake_build
        ), contextlib.redirect_stdout(out):
            code = main(
                ["build-clang", "--commit", "abc", "--source", "/s", "--preset", "mine"]
            )
        self.assertEqual(code, 0)
        self.assertIn("aqb-clang-x-y", out.getvalue())
        self.assertEqual(captured["commit"], "abc")
        self.assertEqual(captured["preset"], "mine")
        self.assertIsNone(captured["user_overlay_json"])

    def test_reads_preset_file(self):
        import tempfile
        from aqb.volume import ClangVolume

        captured = {}

        def fake_build(runtime, **kwargs):
            captured.update(kwargs)
            return ClangVolume(name="v", config_digest="d", built=False)

        with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False) as handle:
            handle.write('{"version": 6, "configurePresets": []}')
            preset_path = handle.name
        with mock.patch("aqb.cli.build_clang_volume", fake_build):
            code = main(
                [
                    "build-clang",
                    "--commit",
                    "c",
                    "--source",
                    "/s",
                    "--preset",
                    "mine",
                    "--preset-file",
                    preset_path,
                ]
            )
        self.assertEqual(code, 0)
        self.assertIn("configurePresets", captured["user_overlay_json"])

    def test_reports_build_error(self):
        from aqb.errors import ClangBuildError

        def fake_build(runtime, **kwargs):
            raise ClangBuildError("boom")

        err = io.StringIO()
        with mock.patch(
            "aqb.cli.build_clang_volume", fake_build
        ), contextlib.redirect_stderr(err):
            code = main(["build-clang", "--commit", "c", "--source", "/s"])
        self.assertEqual(code, 1)
        self.assertIn("boom", err.getvalue())
