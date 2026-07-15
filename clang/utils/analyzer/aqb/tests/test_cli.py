from __future__ import annotations

import contextlib
import io
import unittest

from aqb.cli import main


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
