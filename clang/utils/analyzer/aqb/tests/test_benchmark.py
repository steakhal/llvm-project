from __future__ import annotations

import unittest

from aqb.benchmark import aggregate_samples, candlestick
from aqb.metrics import EntryPointMetrics


class CandlestickTest(unittest.TestCase):
    def test_five_number_summary(self):
        c = candlestick([1, 2, 3, 4, 5])
        self.assertEqual(c.min, 1)
        self.assertEqual(c.max, 5)
        self.assertEqual(c.median, 3)
        self.assertEqual(c.n, 5)
        self.assertLessEqual(c.q1, c.median)
        self.assertLessEqual(c.median, c.q3)

    def test_single_sample_collapses(self):
        c = candlestick([7])
        self.assertEqual((c.min, c.q1, c.median, c.q3, c.max), (7, 7, 7, 7, 7))
        self.assertEqual(c.n, 1)

    def test_empty_is_none(self):
        self.assertIsNone(candlestick([]))


class AggregateTest(unittest.TestCase):
    def _ep(self, usr, file, **stats):
        return EntryPointMetrics(usr=usr, file=file, debug_name=usr, stats=stats)

    def test_three_levels(self):
        it0 = [self._ep("a", "f1.c", NumSteps=10), self._ep("b", "f2.c", NumSteps=5)]
        it1 = [self._ep("a", "f1.c", NumSteps=20), self._ep("b", "f2.c", NumSteps=5)]
        agg = aggregate_samples([it0, it1])

        # per-run: sum over all EPs per iteration -> [15, 25]
        self.assertEqual(agg["run"]["(all)"]["NumSteps"], [15, 25])
        # per-TU: file f1.c -> [10, 20]
        self.assertEqual(agg["tu"]["f1.c"]["NumSteps"], [10, 20])
        # per-entry-point: USR a -> [10, 20]
        self.assertEqual(agg["entry-point"]["a"]["NumSteps"], [10, 20])


if __name__ == "__main__":
    unittest.main()
