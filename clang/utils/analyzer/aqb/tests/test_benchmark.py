from __future__ import annotations

import unittest

from aqb.benchmark import aggregate_samples, candlestick, inner_join_runs
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


class InnerJoinTest(unittest.TestCase):
    def _ep(self, usr, file):
        return EntryPointMetrics(
            usr=usr, file=file, debug_name=usr, stats={"NumSteps": 1}
        )

    def test_keeps_common_drops_the_rest(self):
        # (real.c, foo) is in both runs; each run also has a probe with a
        # build-unique path that the other run doesn't share.
        runs = {
            "A": [
                [self._ep("foo", "real.c"), self._ep("main", "TryCompile-AAA/src.c")]
            ],
            "B": [
                [self._ep("foo", "real.c"), self._ep("main", "TryCompile-BBB/src.c")]
            ],
        }
        filtered, dropped = inner_join_runs(runs)
        # Only the shared (real.c, foo) survives in each run.
        self.assertEqual(
            [(ep.file, ep.usr) for ep in filtered["A"][0]], [("real.c", "foo")]
        )
        self.assertEqual(
            [(ep.file, ep.usr) for ep in filtered["B"][0]], [("real.c", "foo")]
        )
        # The two probe keys are reported as dropped (sorted).
        self.assertEqual(
            dropped,
            [("TryCompile-AAA/src.c", "main"), ("TryCompile-BBB/src.c", "main")],
        )

    def test_single_run_drops_nothing(self):
        runs = {
            "A": [[self._ep("foo", "real.c"), self._ep("main", "TryCompile-X/s.c")]]
        }
        filtered, dropped = inner_join_runs(runs)
        self.assertEqual(dropped, [])
        self.assertEqual(len(filtered["A"][0]), 2)


if __name__ == "__main__":
    unittest.main()
