from __future__ import annotations

import contextlib
import io
import tempfile
import unittest

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
