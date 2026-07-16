from __future__ import annotations

import contextlib
import io
import tempfile
import unittest

from aqb.cli import main
from aqb.metadata import (
    AnalyzerProvenance,
    ContainerProvenance,
    ExecutionProvenance,
    Metadata,
    ProjectProvenance,
)
from aqb.store import RunStore

RUN_ID = "r-20260716-131500-abcd"


def _full_metadata() -> Metadata:
    return Metadata(
        run_id=RUN_ID,
        kind="functional",
        created="2026-07-16T13:15:00+00:00",
        analyzer=AnalyzerProvenance(
            commit="349146dabe4b07651d02",
            commit_title="do the thing",
            config_digest="cfg0a1b2c3d4",
            volume="aqb-clang-349146dabe4b-cfg0a1b2c3d4",
        ),
        container=ContainerProvenance(runtime="podman", image_digest="sha256:deadbeef"),
        execution=ExecutionProvenance(
            n=1,
            analyzer_args=["-Xclang", "-analyzer-stats"],
            note="with workaround X",
        ),
        corpus=[
            ProjectProvenance(
                name="curl",
                source="https://github.com/curl/curl",
                commit="aaaa1111",
                commit_title="curl 8.9",
            ),
            ProjectProvenance(
                name="redis",
                source="https://github.com/redis/redis",
                commit="bbbb2222",
                commit_title="7.4.0",
            ),
        ],
    )


class GoldenMetadataTest(unittest.TestCase):
    def test_full_metadata_survives_store_round_trip_and_is_listed(self):
        with tempfile.TemporaryDirectory() as root:
            store = RunStore(root)
            store.create_run(_full_metadata())

            # Round-trips losslessly (all nested provenance + collections).
            self.assertEqual(store.get(RUN_ID), _full_metadata())

            # And is visible through the CLI list command against the same home.
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                code = main(["--home", root, "list"])
            self.assertEqual(code, 0)
            self.assertIn(RUN_ID, out.getvalue())
