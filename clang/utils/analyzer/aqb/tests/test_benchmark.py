from __future__ import annotations

import unittest

from aqb.benchmark import candle_frames, frame_from_samples, inner_join_runs


def _ep(usr, file, **stats):
    return {"usr": usr, "file": file, "debug_name": usr, "stats": stats}


class FrameFromSamplesTest(unittest.TestCase):
    def test_long_tidy_shape(self):
        sbr = {"A": [[_ep("a", "f1.c", NumSteps=10, CFGSize=3)]]}
        df = frame_from_samples(sbr)
        self.assertEqual(len(df), 2)  # one row per (metric)
        row = df[df["metric"] == "NumSteps"].iloc[0]
        self.assertEqual(row["run"], "A")
        self.assertEqual(row["iteration"], 0)
        self.assertEqual(row["usr"], "a")
        self.assertEqual(row["file"], "f1.c")
        self.assertEqual(row["value"], 10)


class CandleFramesTest(unittest.TestCase):
    def test_three_levels(self):
        # 2 iterations, 2 entry points across 2 files, one run.
        sbr = {
            "A": [
                [_ep("a", "f1.c", NumSteps=10), _ep("b", "f2.c", NumSteps=5)],
                [_ep("a", "f1.c", NumSteps=20), _ep("b", "f2.c", NumSteps=5)],
            ]
        }
        frames = candle_frames(frame_from_samples(sbr))

        # per-run: sum over all EPs per iteration -> [15, 25]
        run = frames["run"]
        r = run[run["metric"] == "NumSteps"].iloc[0]
        self.assertEqual((r["min"], r["median"], r["max"], r["n"]), (15, 20, 25, 2))

        # per-TU: f1.c -> [10, 20]
        tu = frames["tu"]
        f1 = tu[(tu["entity"] == "f1.c") & (tu["metric"] == "NumSteps")].iloc[0]
        self.assertEqual((f1["min"], f1["max"]), (10, 20))

        # per-entry-point: USR a -> [10, 20]
        ep = frames["entry-point"]
        a = ep[(ep["entity"] == "a") & (ep["metric"] == "NumSteps")].iloc[0]
        self.assertEqual((a["min"], a["max"]), (10, 20))


class InnerJoinTest(unittest.TestCase):
    def test_keeps_common_drops_the_rest(self):
        sbr = {
            "A": [
                [
                    _ep("foo", "real.c", NumSteps=1),
                    _ep("main", "TryCompile-AAA/src.c", NumSteps=1),
                ]
            ],
            "B": [
                [
                    _ep("foo", "real.c", NumSteps=1),
                    _ep("main", "TryCompile-BBB/src.c", NumSteps=1),
                ]
            ],
        }
        df, dropped = inner_join_runs(frame_from_samples(sbr))
        self.assertEqual(set(zip(df["file"], df["usr"])), {("real.c", "foo")})
        self.assertEqual(
            dropped,
            [("TryCompile-AAA/src.c", "main"), ("TryCompile-BBB/src.c", "main")],
        )

    def test_single_run_drops_nothing(self):
        sbr = {
            "A": [[_ep("foo", "real.c", NumSteps=1), _ep("main", "T/s.c", NumSteps=1)]]
        }
        df, dropped = inner_join_runs(frame_from_samples(sbr))
        self.assertEqual(dropped, [])
        self.assertEqual(len(set(zip(df["file"], df["usr"]))), 2)


if __name__ == "__main__":
    unittest.main()
