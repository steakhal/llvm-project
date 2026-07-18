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
            code = main(["report"])
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
        # Default resource limits are forwarded.
        self.assertEqual(captured["memory"], "20G")
        self.assertEqual(captured["cpus"], "8")

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


class RunCliTest(unittest.TestCase):
    def test_invokes_perform_run_and_prints_run_id(self):
        captured = {}

        def fake_perform_run(**kwargs):
            captured.update(kwargs)
            return "/some/home/runs/r-123"

        out = io.StringIO()
        with mock.patch(
            "aqb.cli.perform_run", fake_perform_run
        ), contextlib.redirect_stdout(out):
            code = main(
                [
                    "run",
                    "--commit",
                    "abc",
                    "--source",
                    "/s",
                    "--projects",
                    "zstd,fmt",
                    "--extra-analyzer-config",
                    "max-nodes=0",
                ]
            )
        self.assertEqual(code, 0)
        self.assertIn("r-123", out.getvalue())
        self.assertEqual(captured["commit"], "abc")
        self.assertEqual(list(captured["project_names"]), ["zstd", "fmt"])
        self.assertEqual(captured["extra_config"], "max-nodes=0")
        # The in-tree projects dir and analyzer scripts dir are wired.
        self.assertTrue(captured["projects_src"].endswith("projects"))
        self.assertEqual(captured["memory"], "20G")
        self.assertEqual(captured["cpus"], "8")

    def test_reports_run_error(self):
        from aqb.errors import RuntimeCommandError

        def fake_perform_run(**kwargs):
            raise RuntimeCommandError("kaboom")

        err = io.StringIO()
        with mock.patch(
            "aqb.cli.perform_run", fake_perform_run
        ), contextlib.redirect_stderr(err):
            code = main(["run", "--commit", "c", "--source", "/s"])
        self.assertEqual(code, 1)
        self.assertIn("kaboom", err.getvalue())

    def test_bench_passes_benchmark_kind_and_iterations(self):
        captured = {}

        def fake_perform_run(**kwargs):
            captured.update(kwargs)
            return "/h/runs/b-1"

        out = io.StringIO()
        with mock.patch(
            "aqb.cli.perform_run", fake_perform_run
        ), contextlib.redirect_stdout(out):
            code = main(
                ["run", "--commit", "c", "--source", "/s", "--bench", "-n", "3"]
            )
        self.assertEqual(code, 0)
        self.assertEqual(captured["kind"], "benchmark")
        self.assertEqual(captured["iterations"], 3)

    def test_bench_requires_at_least_two_iterations(self):
        err = io.StringIO()
        with contextlib.redirect_stderr(err):
            code = main(
                ["run", "--commit", "c", "--source", "/s", "--bench", "-n", "1"]
            )
        self.assertEqual(code, 2)


class DiffCliTest(unittest.TestCase):
    _ROW = dict(
        issue_id="i1",
        file="a.c",
        line=1,
        column=1,
        checker="core.X",
        category="Logic",
        description="d",
        path_length=1,
    )

    def _make_run(self, store, run_id, findings_by_project):
        import json
        import os

        store.create_run(
            Metadata(
                run_id=run_id,
                kind="functional",
                created="2026-07-17T00:00:00+00:00",
                analyzer=AnalyzerProvenance(commit="c"),
                container=ContainerProvenance(),
                execution=ExecutionProvenance(),
            )
        )
        path = os.path.join(store.runs_dir, run_id, "reports", "findings.json")
        with open(path, "w") as f:
            json.dump(findings_by_project, f)

    def test_diff_reports_added_and_default_passes(self):
        with tempfile.TemporaryDirectory() as root:
            store = RunStore(root)
            row2 = dict(self._ROW, issue_id="i2", line=2)
            self._make_run(store, "r-base", {"p": [self._ROW]})
            self._make_run(store, "r-new", {"p": [self._ROW, row2]})
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                code = main(
                    ["--home", root, "diff", "--base", "r-base", "--new", "r-new"]
                )
            self.assertEqual(code, 0)  # default no-crashes passes
            self.assertIn("added", out.getvalue().lower())

    def test_diff_expect_same_reports_fails_on_drift(self):
        with tempfile.TemporaryDirectory() as root:
            store = RunStore(root)
            row2 = dict(self._ROW, issue_id="i2", line=2)
            self._make_run(store, "r-base", {"p": [self._ROW]})
            self._make_run(store, "r-new", {"p": [self._ROW, row2]})
            with contextlib.redirect_stdout(io.StringIO()):
                code = main(
                    [
                        "--home",
                        root,
                        "diff",
                        "--base",
                        "r-base",
                        "--new",
                        "r-new",
                        "--expect",
                        "same-reports",
                    ]
                )
            self.assertEqual(code, 1)


class PlotCliTest(unittest.TestCase):
    def _make_benchmark_run(self, store, run_id, created="2026-07-18T00:00:00+00:00"):
        import json
        import os

        store.create_run(
            Metadata(
                run_id=run_id,
                kind="benchmark",
                created=created,
                analyzer=AnalyzerProvenance(commit="c"),
                container=ContainerProvenance(),
                execution=ExecutionProvenance(n=2),
            )
        )
        samples = {
            "iterations": 2,
            "metrics": ["NumSteps"],
            "samples": [
                [
                    {
                        "usr": "u1",
                        "file": "a.c",
                        "debug_name": "fn",
                        "stats": {"NumSteps": 10},
                    }
                ],
                [
                    {
                        "usr": "u1",
                        "file": "a.c",
                        "debug_name": "fn",
                        "stats": {"NumSteps": 12},
                    }
                ],
            ],
        }
        path = os.path.join(store.runs_dir, run_id, "metrics", "samples.json")
        with open(path, "w") as f:
            json.dump(samples, f)

    def _make_run_with_eps(self, store, run_id, created, eps):
        """eps: list of (usr, file) for a single-iteration benchmark run."""
        import json
        import os

        store.create_run(
            Metadata(
                run_id=run_id,
                kind="benchmark",
                created=created,
                analyzer=AnalyzerProvenance(commit="c"),
                container=ContainerProvenance(),
                execution=ExecutionProvenance(n=1),
            )
        )
        rows = [
            {"usr": usr, "file": file, "debug_name": usr, "stats": {"NumSteps": 1}}
            for usr, file in eps
        ]
        samples = {"iterations": 1, "metrics": ["NumSteps"], "samples": [rows]}
        path = os.path.join(store.runs_dir, run_id, "metrics", "samples.json")
        with open(path, "w") as f:
            json.dump(samples, f)

    def test_plot_inner_joins_and_logs_dropped(self):
        import os

        with tempfile.TemporaryDirectory() as root:
            store = RunStore(root)
            # Shared (real.c, foo) in both; each has a build-unique probe path.
            self._make_run_with_eps(
                store,
                "b-old",
                "2026-07-18T01:00:00+00:00",
                [("foo", "real.c"), ("main", "TryCompile-AAA/src.c")],
            )
            self._make_run_with_eps(
                store,
                "b-new",
                "2026-07-18T09:00:00+00:00",
                [("foo", "real.c"), ("main", "TryCompile-BBB/src.c")],
            )
            out_path = os.path.join(root, "plot.html")
            err = io.StringIO()
            with contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(
                err
            ):
                code = main(["--home", root, "plot", "b-old", "b-new", "-o", out_path])
            self.assertEqual(code, 0)
            # Dropped probe paths are logged...
            stderr = err.getvalue()
            self.assertIn("dropped", stderr)
            self.assertIn("TryCompile-AAA/src.c", stderr)
            self.assertIn("TryCompile-BBB/src.c", stderr)
            # ...and excluded from the chart, while the shared entry stays.
            with open(out_path) as f:
                html_out = f.read()
            self.assertNotIn("TryCompile", html_out)
            self.assertIn("real.c", html_out)

    def test_plot_writes_html_with_svg(self):
        import os

        with tempfile.TemporaryDirectory() as root:
            store = RunStore(root)
            self._make_benchmark_run(store, "b-1")
            out_path = os.path.join(root, "plot.html")
            with contextlib.redirect_stdout(io.StringIO()):
                code = main(["--home", root, "plot", "b-1", "-o", out_path])
            self.assertEqual(code, 0)
            self.assertTrue(os.path.isfile(out_path))
            with open(out_path) as f:
                html_out = f.read()
            self.assertIn("<svg", html_out)
            self.assertIn("NumSteps", html_out)

    def test_plot_accepts_a_run_path(self):
        import os

        with tempfile.TemporaryDirectory() as root:
            store = RunStore(root)
            self._make_benchmark_run(store, "b-1")
            run_path = os.path.join(store.runs_dir, "b-1")  # full path, not the id
            out_path = os.path.join(root, "plot.html")
            with contextlib.redirect_stdout(io.StringIO()):
                code = main(["--home", root, "plot", run_path, "-o", out_path])
            self.assertEqual(code, 0)
            self.assertTrue(os.path.isfile(out_path))

    def test_plot_orders_runs_by_created(self):
        import os

        with tempfile.TemporaryDirectory() as root:
            store = RunStore(root)
            self._make_benchmark_run(
                store, "b-new", created="2026-07-18T09:00:00+00:00"
            )
            self._make_benchmark_run(
                store, "b-old", created="2026-07-18T01:00:00+00:00"
            )
            out_path = os.path.join(root, "plot.html")
            with contextlib.redirect_stdout(io.StringIO()):
                # Pass newest first on the CLI; output must still be oldest-first.
                code = main(["--home", root, "plot", "b-new", "b-old", "-o", out_path])
            self.assertEqual(code, 0)
            with open(out_path) as f:
                html_out = f.read()
            self.assertIn("Runs: b-old, b-new", html_out)

    def test_plot_errors_on_non_benchmark_run(self):
        with tempfile.TemporaryDirectory() as root:
            store = RunStore(root)
            store.create_run(
                Metadata(
                    run_id="f-1",
                    kind="functional",
                    created="2026-07-18T00:00:00+00:00",
                    analyzer=AnalyzerProvenance(commit="c"),
                    container=ContainerProvenance(),
                    execution=ExecutionProvenance(),
                )
            )
            err = io.StringIO()
            with contextlib.redirect_stderr(err):
                code = main(["--home", root, "plot", "f-1"])
            self.assertEqual(code, 1)
            self.assertIn("samples.json", err.getvalue())
