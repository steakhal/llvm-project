from __future__ import annotations

import datetime
import unittest

from aqb.runid import new_run_id


class RunIdTest(unittest.TestCase):
    def test_format_is_deterministic_when_injected(self):
        now = datetime.datetime(2026, 7, 16, 13, 15, 0)
        self.assertEqual(new_run_id(now=now, suffix="a1b2"), "r-20260716-131500-a1b2")

    def test_two_default_ids_differ(self):
        self.assertNotEqual(new_run_id(), new_run_id())
