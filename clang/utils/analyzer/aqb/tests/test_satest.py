from __future__ import annotations

import unittest

from aqb import _satest


class SatestReuseTest(unittest.TestCase):
    def test_cmpruns_diff_api_is_importable(self):
        # The two-tier diff engine we reuse in Phase 4.
        self.assertTrue(hasattr(_satest.CmpRuns, "compare_results"))
        self.assertTrue(hasattr(_satest.CmpRuns, "load_results_from_single_run"))

    def test_projectmap_corpus_api_is_importable(self):
        # The corpus registry we reuse in Phase 3.
        self.assertTrue(hasattr(_satest.ProjectMap, "ProjectInfo"))
        self.assertTrue(hasattr(_satest.ProjectMap, "ProjectMap"))
