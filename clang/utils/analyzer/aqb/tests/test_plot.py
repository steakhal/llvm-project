from __future__ import annotations

import unittest

from aqb.benchmark import Candle, candle_frames, frame_from_samples
from aqb.plot import _ordered_entities, render_html, svg_chart


def _ep(usr, file, **stats):
    return {"usr": usr, "file": file, "debug_name": usr, "stats": stats}


def _frames(samples_by_run):
    return candle_frames(frame_from_samples(samples_by_run))


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
        sbr = {
            "run-A": [
                [_ep("usrA", "f1.c", NumSteps=10, PathRunningTime=1)],
                [_ep("usrA", "f1.c", NumSteps=12, PathRunningTime=2)],
            ]
        }
        html_out = render_html(_frames(sbr), run_order=["run-A"])
        self.assertIn("<html", html_out)
        self.assertIn("NumSteps", html_out)
        self.assertIn("PathRunningTime", html_out)
        self.assertIn("Per-run", html_out)
        self.assertIn("Per-TU", html_out)
        self.assertIn("Per-entry-point", html_out)
        # self-contained: no external assets
        self.assertNotIn("<script src", html_out)
        self.assertNotIn("http://", html_out.replace("http://www.w3.org/2000/svg", ""))

    def test_overlays_multiple_runs_in_order(self):
        sbr = {
            "A": [[_ep("u", "f.c", NumSteps=10)]],
            "B": [[_ep("u", "f.c", NumSteps=20)]],
        }
        html_out = render_html(_frames(sbr), run_order=["A", "B"])
        self.assertIn("Runs: A, B", html_out)


class OrderingTest(unittest.TestCase):
    def _frames_and_order(self):
        # "old" is the oldest run; its PathRunningTime drives entity order.
        sbr = {
            "old": [
                [
                    _ep("x", "lo.c", PathRunningTime=5, NumSteps=1),
                    _ep("y", "hi.c", PathRunningTime=100, NumSteps=1),
                    _ep("z", "none.c", NumSteps=7),  # no PathRunningTime -> last
                ]
            ],
            "new": [
                [
                    _ep("x", "lo.c", PathRunningTime=9),
                    _ep("y", "hi.c", PathRunningTime=90),
                ]
            ],
        }
        return _frames(sbr), ["old", "new"]

    def test_entities_ordered_by_oldest_path_running_time_desc(self):
        frames, order = self._frames_and_order()
        self.assertEqual(
            _ordered_entities(frames["tu"], order[0]), ["hi.c", "lo.c", "none.c"]
        )

    def test_ordering_is_shared_across_metric_charts(self):
        frames, order = self._frames_and_order()
        html_out = render_html(frames, run_order=order)
        self.assertLess(html_out.index("hi.c"), html_out.index("lo.c"))


class ScrollSyncTest(unittest.TestCase):
    def test_all_charts_at_a_level_share_the_full_axis(self):
        # metric NumSteps only on a.c, CFGSize only on b.c; both TU charts must
        # still render BOTH entities (empty slots) so columns line up.
        sbr = {"r": [[_ep("ua", "a.c", NumSteps=1), _ep("ub", "b.c", CFGSize=2)]]}
        html_out = render_html(_frames(sbr), run_order=["r"])
        self.assertGreaterEqual(html_out.count("a.c"), 2)
        self.assertGreaterEqual(html_out.count("b.c"), 2)

    def test_sync_groups_and_script_present(self):
        sbr = {"r": [[_ep("u", "a.c", NumSteps=1)]]}
        html_out = render_html(_frames(sbr), run_order=["r"])
        self.assertIn('data-sync="tu"', html_out)
        self.assertIn('data-sync="entry-point"', html_out)
        self.assertIn("<script>", html_out)
        self.assertNotIn("<script src", html_out)
        self.assertIn("scrollLeft", html_out)
        # Smooth: propagation coalesced into an animation frame, passive listener.
        self.assertIn("requestAnimationFrame", html_out)
        self.assertIn("passive", html_out)


class HoverTooltipTest(unittest.TestCase):
    def test_candle_tooltip_table_ordered_max_to_min_no_n(self):
        series = {"run-A": {"c:@F@zc": Candle(100, 110, 115, 120, 130, 5)}}
        svg = svg_chart(
            ["c:@F@zc"], series, title="NumSteps", labels={"c:@F@zc": "ZSTD_compress"}
        )
        self.assertIn("data-tip=", svg)
        self.assertNotIn("<title>", svg)  # replaced by the JS tooltip
        self.assertIn("ZSTD_compress", svg)
        self.assertIn("<table>", svg)
        # ordered max at top, min at bottom
        self.assertLess(svg.index(">max<"), svg.index(">min<"))
        self.assertLess(svg.index(">q3<"), svg.index(">q1<"))
        # the sample count "n" is gone from the tooltip
        self.assertNotIn(">n<", svg)
        self.assertNotIn("<td>n</td>", svg)
        # values present; a copyable plain-text payload too
        self.assertIn(">130<", svg)
        self.assertIn(">100<", svg)
        self.assertIn("data-copy=", svg)

    def test_render_includes_click_to_copy_tooltip(self):
        sbr = {"r": [[_ep("c:@F@zc", "z.c", NumSteps=1)]]}
        sbr["r"][0][0]["debug_name"] = "ZSTD_compress"
        html_out = render_html(
            _frames(sbr), run_order=["r"], names={"c:@F@zc": "ZSTD_compress"}
        )
        self.assertIn("data-tip=", html_out)
        self.assertIn("data-copy=", html_out)
        self.assertIn("ZSTD_compress", html_out)
        self.assertIn('id="aqb-tip"', html_out)
        self.assertIn("mousemove", html_out)
        # click-to-copy wiring, with a fallback for non-secure contexts
        self.assertIn("addEventListener('click'", html_out)
        self.assertIn("clipboard", html_out)
        self.assertIn("execCommand", html_out)
        self.assertNotIn("<script src", html_out)


class LogToggleTest(unittest.TestCase):
    def test_candles_carry_raw_values_and_chart_carries_geometry(self):
        series = {"run-A": {"f1.c": Candle(1, 2, 3, 4, 5, 4)}}
        svg = svg_chart(["f1.c"], series, title="NumSteps")
        self.assertIn("data-ph=", svg)
        self.assertIn("data-ymax=", svg)
        self.assertIn('class="candle"', svg)
        self.assertIn('data-md="3"', svg)
        self.assertIn('data-hi="5"', svg)
        self.assertIn('class="wick"', svg)
        self.assertIn('class="box"', svg)
        self.assertIn('class="median"', svg)

    def test_render_has_per_chart_log_checkbox_and_script(self):
        sbr = {"r": [[_ep("u", "f.c", NumSteps=1)], [_ep("u", "f.c", NumSteps=2)]]}
        html_out = render_html(_frames(sbr), run_order=["r"])
        self.assertIn('class="logtoggle"', html_out)
        self.assertIn('type="checkbox"', html_out)
        self.assertIn("Math.log", html_out)
        self.assertNotIn("<script src", html_out)


if __name__ == "__main__":
    unittest.main()
