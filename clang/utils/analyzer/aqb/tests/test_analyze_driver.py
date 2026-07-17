from __future__ import annotations

import os
import sys
import unittest

# analyze_driver imports SATest siblings (ProjectMap) from the analyzer dir;
# make both that dir and the aqb package dir importable for these unit tests.
_AQB_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
_ANALYZER_DIR = os.path.dirname(_AQB_DIR)
for _p in (_ANALYZER_DIR, _AQB_DIR):
    if _p not in sys.path:
        sys.path.insert(0, _p)

from analyze_driver import _select_projects  # noqa: E402
from ProjectMap import ProjectInfo  # noqa: E402


class _Map:
    def __init__(self, projects):
        self.projects = projects


class SelectProjectsTest(unittest.TestCase):
    def _map(self):
        return _Map(
            [
                ProjectInfo("zstd", 1, enabled=True),
                ProjectInfo("curl", 1, enabled=False),
            ]
        )

    def test_empty_selects_only_enabled(self):
        names = [p.name for p in _select_projects(self._map(), [])]
        self.assertEqual(names, ["zstd"])

    def test_explicit_force_enables_disabled(self):
        sel = _select_projects(self._map(), ["curl"])
        self.assertEqual([(p.name, p.enabled) for p in sel], [("curl", True)])

    def test_unknown_project_raises(self):
        with self.assertRaises(SystemExit):
            _select_projects(self._map(), ["does-not-exist"])


if __name__ == "__main__":
    unittest.main()
