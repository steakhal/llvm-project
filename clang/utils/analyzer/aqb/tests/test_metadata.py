from __future__ import annotations

import json
import unittest

from aqb.metadata import (
    AnalyzerProvenance,
    ContainerProvenance,
    ExecutionProvenance,
    Metadata,
    ProjectProvenance,
)


def _sample() -> Metadata:
    return Metadata(
        run_id="r-20260716-131500-a1b2",
        kind="functional",
        created="2026-07-16T13:15:00+00:00",
        analyzer=AnalyzerProvenance(
            commit="349146da",
            commit_title="do the thing",
            config_digest="cfg123",
            volume="aqb-clang-349146da-cfg123",
        ),
        container=ContainerProvenance(runtime="podman", image_digest="sha256:abc"),
        execution=ExecutionProvenance(
            n=1, analyzer_args=["-Xclang", "-analyzer-stats"], note="with workaround X"
        ),
        corpus=[
            ProjectProvenance(
                name="curl",
                source="https://github.com/curl/curl",
                commit="deadbeef",
                commit_title="curl 8.9",
            )
        ],
    )


class MetadataTest(unittest.TestCase):
    def test_json_round_trip_is_lossless(self):
        meta = _sample()
        restored = Metadata.from_json(meta.to_json())
        self.assertEqual(restored, meta)

    def test_to_json_is_stable_sorted_text(self):
        meta = _sample()
        # Same object serializes identically twice (sorted keys, no ordering churn).
        self.assertEqual(meta.to_json(), meta.to_json())
        # And is valid JSON with the note under execution.
        parsed = json.loads(meta.to_json())
        self.assertEqual(parsed["execution"]["note"], "with workaround X")
