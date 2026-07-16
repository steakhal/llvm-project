from __future__ import annotations

import unittest

from aqb.metrics import dedup_entry_points, parse_entry_point_csv, parse_tu_stats_json

CSV = (
    "USR,File,DebugName,CFGSize,PathRunningTime,NumSteps\n"
    '"c:@F@fib#i#","/src/fib.c","fib",5,12,120\n'
    '"c:@F@main#","/src/main.c","main",,,240\n'
)


class EntryPointCsvTest(unittest.TestCase):
    def test_parses_rows_and_stats(self):
        rows = parse_entry_point_csv(CSV)
        self.assertEqual([r.usr for r in rows], ["c:@F@fib#i#", "c:@F@main#"])
        fib = rows[0]
        self.assertEqual(fib.file, "/src/fib.c")
        self.assertEqual(fib.debug_name, "fib")
        self.assertEqual(fib.stats["CFGSize"], 5)
        self.assertEqual(fib.stats["PathRunningTime"], 12)
        self.assertEqual(fib.stats["NumSteps"], 120)

    def test_empty_cells_are_omitted_from_stats(self):
        rows = parse_entry_point_csv(CSV)
        main = rows[1]
        self.assertNotIn("CFGSize", main.stats)
        self.assertNotIn("PathRunningTime", main.stats)
        self.assertEqual(main.stats["NumSteps"], 240)

    def test_empty_input_yields_no_rows(self):
        self.assertEqual(parse_entry_point_csv(""), [])

    def test_dedup_keeps_first_per_usr(self):
        rows = parse_entry_point_csv(CSV)
        deduped = dedup_entry_points(rows + [rows[0]])
        self.assertEqual([r.usr for r in deduped], ["c:@F@fib#i#", "c:@F@main#"])


TU_STATS_JSON = (
    "{\n"
    '\t"Analysis.NumFunctionsAnalyzed": 3,\n'
    '\t"CoreEngine.NumSteps": 240\n'
    "}\n"
)


class TuStatsJsonTest(unittest.TestCase):
    def test_parses_object_of_int_values(self):
        stats = parse_tu_stats_json(TU_STATS_JSON)
        self.assertEqual(stats["Analysis.NumFunctionsAnalyzed"], 3)
        self.assertEqual(stats["CoreEngine.NumSteps"], 240)

    def test_empty_object(self):
        self.assertEqual(parse_tu_stats_json("{}"), {})

    def test_preserves_float_timer_values(self):
        # PrintStatisticsJSON appends TimerGroup float values into the same
        # object; they must not be truncated to int.
        text = (
            "{\n"
            '\t"CoreEngine.NumSteps": 240,\n'
            '\t"time.analyzer.exprengine.wall": 1.25e-01\n'
            "}\n"
        )
        stats = parse_tu_stats_json(text)
        self.assertEqual(stats["CoreEngine.NumSteps"], 240)
        self.assertAlmostEqual(stats["time.analyzer.exprengine.wall"], 0.125)
