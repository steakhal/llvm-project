from __future__ import annotations

import os
from typing import List

from aqb.errors import RunNotFoundError
from aqb.metadata import Metadata

RUN_SUBDIRS = ("reports", "metrics", "logs")


class RunStore:
    """Filesystem-backed store of run artifacts under ``<root>/runs/<run_id>/``."""

    def __init__(self, root: str):
        self.root = root
        self.runs_dir = os.path.join(root, "runs")

    def _run_path(self, run_id: str) -> str:
        return os.path.join(self.runs_dir, run_id)

    def create_run(self, metadata: Metadata) -> str:
        path = self._run_path(metadata.run_id)
        os.makedirs(path, exist_ok=False)
        for sub in RUN_SUBDIRS:
            os.makedirs(os.path.join(path, sub), exist_ok=True)
        with open(os.path.join(path, "metadata.json"), "w") as handle:
            handle.write(metadata.to_json())
        return path

    def get(self, run_id: str) -> Metadata:
        meta_path = os.path.join(self._run_path(run_id), "metadata.json")
        if not os.path.isfile(meta_path):
            raise RunNotFoundError(run_id)
        with open(meta_path) as handle:
            return Metadata.from_json(handle.read())

    def list_runs(self) -> List[str]:
        if not os.path.isdir(self.runs_dir):
            return []
        return sorted(
            name
            for name in os.listdir(self.runs_dir)
            if os.path.isfile(os.path.join(self.runs_dir, name, "metadata.json"))
        )

    def resolve(self, prefix: str) -> str:
        # Accept a run path (e.g. what `aqb run` prints to stderr) or a bare id/
        # prefix: run ids never contain a path separator, so reducing to the
        # final path component is always safe and lets users paste either.
        prefix = os.path.basename(prefix.rstrip("/")) or prefix
        matches = [r for r in self.list_runs() if r.startswith(prefix)]
        if len(matches) == 1:
            return matches[0]
        if not matches:
            raise RunNotFoundError(f"no run matching {prefix!r} (see 'aqb list')")
        raise RunNotFoundError(f"ambiguous run id prefix: {prefix} -> {matches}")
