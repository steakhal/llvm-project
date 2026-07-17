from __future__ import annotations

import unittest

from aqb.diff import compare_findings, diff_runs, summarize, verdict
from aqb.normalize import Finding


def _f(
    issue_id, file="a.c", line=1, col=1, checker="core.X", cat="Logic", desc="d", pl=1
):
    return Finding(
        issue_id=issue_id,
        file=file,
        line=line,
        column=col,
        checker=checker,
        category=cat,
        description=desc,
        path_length=pl,
    )


class CompareFindingsTest(unittest.TestCase):
    def test_identical_are_common(self):
        d = compare_findings([_f("id1")], [_f("id1")])
        self.assertEqual(len(d.common), 1)
        self.assertEqual(d.added, [])
        self.assertEqual(d.removed, [])

    def test_added_and_removed_at_distinct_locations(self):
        d = compare_findings([_f("old", line=1)], [_f("new", line=2)])
        self.assertEqual(len(d.removed), 1)
        self.assertEqual(len(d.added), 1)
        self.assertEqual(d.changed, [])

    def test_same_location_similar_is_changed(self):
        # Same location, different identity, same checker/category/desc => tier-2.
        d = compare_findings([_f("id-a", line=5)], [_f("id-b", line=5)])
        self.assertEqual(len(d.changed), 1)
        self.assertEqual(d.added, [])
        self.assertEqual(d.removed, [])

    def test_same_location_dissimilar_is_add_remove(self):
        d = compare_findings(
            [_f("id-a", line=5, checker="core.A", desc="da")],
            [_f("id-b", line=5, checker="core.B", desc="db")],
        )
        self.assertEqual(len(d.added), 1)
        self.assertEqual(len(d.removed), 1)
        self.assertEqual(d.changed, [])


class DiffRunsTest(unittest.TestCase):
    def test_diff_runs_union_of_projects_and_summary(self):
        base = {"p": [_f("id1", line=1)], "gone": [_f("g", line=9)]}
        new = {"p": [_f("id1", line=1), _f("id2", line=2)], "fresh": [_f("z", line=3)]}
        deltas = diff_runs(base, new)
        self.assertEqual(set(deltas), {"p", "gone", "fresh"})
        self.assertEqual(len(deltas["p"].common), 1)
        self.assertEqual(len(deltas["p"].added), 1)
        self.assertEqual(len(deltas["gone"].removed), 1)
        self.assertEqual(len(deltas["fresh"].added), 1)

        s = summarize(deltas)
        self.assertEqual(s["added"], 2)
        self.assertEqual(s["removed"], 1)
        self.assertEqual(s["common"], 1)
        self.assertEqual(s["changed"], 0)


class VerdictTest(unittest.TestCase):
    def test_verdict_policies(self):
        clean = {"common": 5, "added": 0, "removed": 0, "changed": 0}
        drift = {"common": 5, "added": 2, "removed": 1, "changed": 0}

        self.assertTrue(verdict(clean, "same-reports")[0])
        self.assertFalse(verdict(drift, "same-reports")[0])
        self.assertTrue(
            verdict(
                {"common": 4, "added": 1, "removed": 1, "changed": 0}, "same-count"
            )[0]
        )
        self.assertFalse(verdict(drift, "same-count")[0])  # 6 vs 7
        self.assertTrue(verdict(drift, "no-crashes")[0])

    def test_unknown_policy_raises(self):
        with self.assertRaises(ValueError):
            verdict({"common": 0, "added": 0, "removed": 0, "changed": 0}, "bogus")


if __name__ == "__main__":
    unittest.main()
