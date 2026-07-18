from __future__ import annotations

import unittest

from aqb.benchmark import Candle
from aqb.plot import _ordered_entities, render_html, svg_chart


class SvgChartTest(unittest.TestCase):
    def test_chart_has_one_candle_per_run_per_entity(self):
        series = {
            "run-A": {"f1.c": Candle(1, 2, 3, 4, 5, 4)},
            "run-B": {"f1.c": Candle(2, 3, 4, 5, 6, 4)},
        }
        svg = svg_chart(["f1.c"], series, title="NumSteps")
        self.assertIn("<svg", svg)
        self.assertIn("NumSteps", svg)
        self.assertGreaterEqual(svg.count("<rect"), 2)  # a box per run
        self.assertIn("run-A", svg)
        self.assertIn("run-B", svg)

    def test_missing_entity_for_a_run_is_skipped(self):
        series = {"run-A": {"f1.c": Candle(1, 2, 3, 4, 5, 4)}, "run-B": {}}
        svg = svg_chart(["f1.c"], series, title="m")
        self.assertIn("<svg", svg)  # does not crash on the absent candle


class RenderHtmlTest(unittest.TestCase):
    def test_bundles_all_levels_and_metrics(self):
        runs = {
            "run-A": {
                "run": {"(all)": {"NumSteps": [10, 12], "PathRunningTime": [1, 2]}},
                "tu": {"f1.c": {"NumSteps": [10, 12]}},
                "entry-point": {"usrA": {"NumSteps": [10, 12]}},
            }
        }
        html_out = render_html(runs)
        self.assertIn("<html", html_out)
        self.assertIn("NumSteps", html_out)
        self.assertIn("PathRunningTime", html_out)
        self.assertIn("Per-run", html_out)
        self.assertIn("Per-TU", html_out)
        self.assertIn("Per-entry-point", html_out)
        # self-contained: no external assets
        self.assertNotIn("<script src", html_out)
        self.assertNotIn("http://", html_out.replace("http://www.w3.org/2000/svg", ""))

    def test_overlays_multiple_runs(self):
        runs = {
            "A": {"run": {"(all)": {"NumSteps": [10]}}, "tu": {}, "entry-point": {}},
            "B": {"run": {"(all)": {"NumSteps": [20]}}, "tu": {}, "entry-point": {}},
        }
        html_out = render_html(runs)
        self.assertIn("Runs: A, B", html_out)


class OrderingTest(unittest.TestCase):
    def _runs(self):
        # "old" is the first (oldest) run; its PathRunningTime drives ordering.
        return {
            "old": {
                "run": {},
                "tu": {
                    "lo.c": {"PathRunningTime": [5], "NumSteps": [1]},
                    "hi.c": {"PathRunningTime": [100], "NumSteps": [1]},
                    "none.c": {"NumSteps": [7]},  # no PathRunningTime -> last
                },
                "entry-point": {},
            },
            "new": {
                "run": {},
                "tu": {
                    "lo.c": {"PathRunningTime": [9]},
                    "hi.c": {"PathRunningTime": [90]},
                },
                "entry-point": {},
            },
        }

    def test_entities_ordered_by_oldest_path_running_time_desc(self):
        # Busiest (hi.c) first; the entity lacking PathRunningTime sorts last.
        self.assertEqual(
            _ordered_entities(self._runs(), "tu"), ["hi.c", "lo.c", "none.c"]
        )

    def test_ordering_is_shared_across_metric_charts(self):
        # Even the NumSteps chart lays entities in PathRunningTime order:
        # hi.c must appear before lo.c in the rendered SVG.
        html_out = render_html(self._runs())
        self.assertLess(html_out.index("hi.c"), html_out.index("lo.c"))


if __name__ == "__main__":
    unittest.main()
