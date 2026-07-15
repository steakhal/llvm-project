from __future__ import annotations

import os
import tempfile
import unittest

from aqb.metadata import (
    AnalyzerProvenance,
    ContainerProvenance,
    ExecutionProvenance,
    Metadata,
)
from aqb.store import RunNotFoundError, RunStore


def _meta(run_id: str) -> Metadata:
    return Metadata(
        run_id=run_id,
        kind="functional",
        created="2026-07-16T13:15:00+00:00",
        analyzer=AnalyzerProvenance(commit="349146da"),
        container=ContainerProvenance(),
        execution=ExecutionProvenance(),
    )


class RunStoreTest(unittest.TestCase):
    def test_create_then_get_round_trips_metadata(self):
        with tempfile.TemporaryDirectory() as root:
            store = RunStore(root)
            path = store.create_run(_meta("r-1"))
            for sub in ("reports", "metrics", "logs"):
                self.assertTrue(os.path.isdir(os.path.join(path, sub)))
            self.assertEqual(store.get("r-1").analyzer.commit, "349146da")

    def test_list_runs_sorted(self):
        with tempfile.TemporaryDirectory() as root:
            store = RunStore(root)
            store.create_run(_meta("r-2"))
            store.create_run(_meta("r-1"))
            self.assertEqual(store.list_runs(), ["r-1", "r-2"])

    def test_resolve_unique_prefix(self):
        with tempfile.TemporaryDirectory() as root:
            store = RunStore(root)
            store.create_run(_meta("r-20260716-aaaa"))
            self.assertEqual(store.resolve("r-2026"), "r-20260716-aaaa")

    def test_resolve_missing_raises(self):
        with tempfile.TemporaryDirectory() as root:
            with self.assertRaises(RunNotFoundError):
                RunStore(root).resolve("nope")

    def test_resolve_ambiguous_raises(self):
        with tempfile.TemporaryDirectory() as root:
            store = RunStore(root)
            store.create_run(_meta("r-aa"))
            store.create_run(_meta("r-ab"))
            with self.assertRaises(RunNotFoundError):
                store.resolve("r-a")
