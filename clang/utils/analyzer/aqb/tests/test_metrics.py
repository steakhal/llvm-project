from __future__ import annotations

import unittest

import pandas as pd

from aqb.metrics import (
    dedup_entry_points,
    parse_entry_point_csv,
    parse_tu_stats_json,
    to_sample_records,
)

CSV = (
    "USR,File,DebugName,CFGSize,PathRunningTime,NumSteps\n"
    '"c:@F@fib#i#","/src/fib.c","fib",5,12,120\n'
    '"c:@F@main#","/src/main.c","main",,,240\n'
)


class EntryPointCsvTest(unittest.TestCase):
    def test_parses_wide_frame(self):
        df = parse_entry_point_csv(CSV)
        self.assertEqual(list(df["USR"]), ["c:@F@fib#i#", "c:@F@main#"])
        self.assertEqual(list(df.columns)[:3], ["USR", "File", "DebugName"])
        fib = df.iloc[0]
        self.assertEqual(fib["File"], "/src/fib.c")
        self.assertEqual(fib["DebugName"], "fib")
        self.assertEqual(int(fib["CFGSize"]), 5)
        self.assertEqual(int(fib["NumSteps"]), 120)

    def test_empty_cells_are_nan(self):
        df = parse_entry_point_csv(CSV)
        main = df.iloc[1]
        self.assertTrue(pd.isna(main["CFGSize"]))
        self.assertTrue(pd.isna(main["PathRunningTime"]))
        self.assertEqual(int(main["NumSteps"]), 240)

    def test_empty_input_yields_empty_frame(self):
        df = parse_entry_point_csv("")
        self.assertTrue(df.empty)
        self.assertEqual(list(df.columns), ["USR", "File", "DebugName"])

    def test_dedup_keeps_first_per_usr(self):
        df = parse_entry_point_csv(CSV)
        dup = pd.concat([df, df.iloc[[0]]], ignore_index=True)
        deduped = dedup_entry_points(dup)
        self.assertEqual(list(deduped["USR"]), ["c:@F@fib#i#", "c:@F@main#"])

    def test_to_sample_records_omits_nan_and_casts_int(self):
        recs = to_sample_records(parse_entry_point_csv(CSV))
        self.assertEqual(recs[0]["usr"], "c:@F@fib#i#")
        self.assertEqual(recs[0]["file"], "/src/fib.c")
        self.assertEqual(recs[0]["stats"]["CFGSize"], 5)
        self.assertEqual(recs[0]["stats"]["NumSteps"], 120)
        # main's empty cells are omitted; present ones are ints.
        self.assertNotIn("CFGSize", recs[1]["stats"])
        self.assertNotIn("PathRunningTime", recs[1]["stats"])
        self.assertEqual(recs[1]["stats"]["NumSteps"], 240)
        self.assertIsInstance(recs[1]["stats"]["NumSteps"], int)


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
        text = (
            "{\n"
            '\t"CoreEngine.NumSteps": 240,\n'
            '\t"time.analyzer.exprengine.wall": 1.25e-01\n'
            "}\n"
        )
        stats = parse_tu_stats_json(text)
        self.assertEqual(stats["CoreEngine.NumSteps"], 240)
        self.assertAlmostEqual(stats["time.analyzer.exprengine.wall"], 0.125)


if __name__ == "__main__":
    unittest.main()
