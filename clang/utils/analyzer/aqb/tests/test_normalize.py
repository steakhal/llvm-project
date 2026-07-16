from __future__ import annotations

import os
import plistlib
import tempfile
import unittest

from aqb.normalize import load_findings


def _diag(file_idx, line, col, checker, category, desc, ctx, issue_hash):
    return {
        "location": {"file": file_idx, "line": line, "col": col},
        "path": [{"kind": "event"}],  # path length 1
        "check_name": checker,
        "category": category,
        "description": desc,
        "issue_context": ctx,
        "issue_hash_content_of_line_in_context": issue_hash,
        "HTMLDiagnostics_files": ["report.html"],
    }


def _write_plist(path, files, diagnostics):
    with open(path, "wb") as handle:
        plistlib.dump(
            {"files": files, "diagnostics": diagnostics, "clang_version": "x"}, handle
        )


class LoadFindingsTest(unittest.TestCase):
    def test_dedups_by_issue_identity_and_relativizes(self):
        with tempfile.TemporaryDirectory() as root:
            files = [os.path.join(root, "src", "a.c")]
            diags = [
                _diag(
                    0,
                    10,
                    4,
                    "core.NullDeref",
                    "Logic error",
                    "deref of null",
                    "func_f",
                    111,
                ),
                _diag(
                    0,
                    20,
                    2,
                    "core.DivZero",
                    "Logic error",
                    "divide by zero",
                    "func_g",
                    222,
                ),
                # Duplicate of the first (same file+context+hash) -> same issue.
                _diag(
                    0,
                    10,
                    4,
                    "core.NullDeref",
                    "Logic error",
                    "deref of null",
                    "func_f",
                    111,
                ),
            ]
            _write_plist(os.path.join(root, "report.plist"), files, diags)

            findings = load_findings(root, project_root=root)

            # Three diagnostics, two distinct issues.
            self.assertEqual(len(findings), 2)
            ids = {f.issue_id for f in findings}
            self.assertEqual(len(ids), 2)
            # A finding carries its attributes.
            null = next(f for f in findings if f.checker == "core.NullDeref")
            self.assertEqual(null.line, 10)
            self.assertEqual(null.column, 4)
            self.assertEqual(null.category, "Logic error")
            self.assertEqual(null.path_length, 1)
            # Path is relative to project_root (root prefix stripped).
            self.assertNotIn(root, null.file)
            self.assertTrue(null.file.endswith("a.c"))
