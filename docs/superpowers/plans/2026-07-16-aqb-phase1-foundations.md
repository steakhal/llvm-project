# AQB (Analysis Qualification Bench) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build `aqb`, a Clang Static Analyzer qualification tool that runs a chosen analyzer build over a pinned corpus, stores runs as durable artifacts, and diffs / benchmarks / plots them — reusing SATest's proven cores.

**Architecture:** A new Python package `clang/utils/analyzer/aqb/` (approach "B": new orchestration, reused cores). It imports the existing sibling SATest modules (`CmpRuns`, `ProjectMap`, `SATestBuild`, `SATestBenchmark`) rather than rewriting them. The design source of truth is `clang/docs/analyzer/developer-docs/AQB-design.rst`.

**Tech Stack:** Python ≥3.8 (stdlib only for new code — `argparse`, `dataclasses`, `json`, `subprocess`; `unittest` for tests), formatted with `black`, type-checked with `pyright` (per repo `pyproject.toml`). Docker/Podman for isolation and volumes. Reuses SATest's plist-based report model.

---

## Scope & Decomposition

This system is too large for a single plan. It is split into **five sequenced plans**, each producing working, testable software. This document details **Phase 1 in full**; Phases 2–5 are scoped stubs to be expanded into their own plan docs after the preceding phase is built and reviewed.

| Phase | Deliverable | Depends on |
|---|---|---|
| **1. Foundations** *(this doc)* | `aqb` package, CLI dispatch, Run Store, `metadata.json`, run IDs, SATest reuse shim | — |
| **2. Runtime & Volumes** | docker/podman word-substitution; Clang Volume (name/digest/labels/resolve-or-build); shared ccache volume | 1 |
| **3. Analyze pipeline** | `aqb run --commit`: materialize → analyze (reuse scan-build) → observe → normalize → dedup; reports + metrics in a run | 1, 2 |
| **4. Compare & verdict** | `aqb diff` (reuse `CmpRuns`); Reference abstraction; `--expect`; `aqb promote` | 1, 3 |
| **5. Benchmark & present** | `--bench -n`; metric distributions; `aqb plot` candlesticks; `aqb report` | 1, 3 |

## Conventions (apply to every task)

- **Package location:** `clang/utils/analyzer/aqb/`. The parent dir `clang/utils/analyzer/` holds the SATest modules we reuse.
- **Run tests from `clang/utils/analyzer/`** so that both `aqb.*` and the sibling modules (`CmpRuns`, `ProjectMap`, …) are importable. Every test command below assumes that working directory.
- **Test framework:** stdlib `unittest`. Run a module with `python -m unittest aqb.tests.<name> -v`.
- **Formatting:** run `black aqb/` before each commit (the repo configures black in `pyproject.toml`).
- **Python 3.8 compatibility:** every new module starts with `from __future__ import annotations` so `X | None` annotations are legal.

## File Structure (Phase 1)

- Create `clang/utils/analyzer/aqb/__init__.py` — package marker + version.
- Create `clang/utils/analyzer/aqb/__main__.py` — `python -m aqb` entry point.
- Create `clang/utils/analyzer/aqb/cli.py` — argparse dispatcher; owns the `list` command and stubs for `run`/`diff`/`plot`/`report`/`promote`.
- Create `clang/utils/analyzer/aqb/runid.py` — run-ID generation.
- Create `clang/utils/analyzer/aqb/metadata.py` — `Metadata` dataclasses + JSON (de)serialization.
- Create `clang/utils/analyzer/aqb/store.py` — `RunStore` (create/get/list/resolve runs on disk).
- Create `clang/utils/analyzer/aqb/_satest.py` — import shim that puts the SATest dir on `sys.path` and re-exports `CmpRuns`, `ProjectMap`.
- Create `clang/utils/analyzer/aqb/tests/__init__.py` — test package marker.
- Create `clang/utils/analyzer/aqb/tests/test_cli.py`, `test_runid.py`, `test_metadata.py`, `test_store.py`, `test_satest.py`.

---

## Phase 1: Foundations

### Task 1: Package scaffold + CLI skeleton

**Files:**
- Create: `clang/utils/analyzer/aqb/__init__.py`
- Create: `clang/utils/analyzer/aqb/cli.py`
- Create: `clang/utils/analyzer/aqb/__main__.py`
- Create: `clang/utils/analyzer/aqb/tests/__init__.py`
- Test: `clang/utils/analyzer/aqb/tests/test_cli.py`

- [ ] **Step 1: Write the failing test**

Create `clang/utils/analyzer/aqb/tests/__init__.py` as an empty file, and `clang/utils/analyzer/aqb/tests/test_cli.py`:

```python
from __future__ import annotations

import contextlib
import io
import unittest

from aqb.cli import main


class CliTest(unittest.TestCase):
    def test_no_command_prints_help_and_exits_zero(self):
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            code = main([])
        self.assertEqual(code, 0)
        self.assertIn("usage: aqb", out.getvalue())

    def test_stub_command_reports_not_implemented(self):
        err = io.StringIO()
        with contextlib.redirect_stderr(err):
            code = main(["run"])
        self.assertEqual(code, 2)
        self.assertIn("not yet implemented", err.getvalue())
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python -m unittest aqb.tests.test_cli -v`
Expected: FAIL — `ModuleNotFoundError: No module named 'aqb'`.

- [ ] **Step 3: Write minimal implementation**

Create `clang/utils/analyzer/aqb/__init__.py`:

```python
"""Analysis Qualification Bench (AQB)."""

__version__ = "0.0.1"
```

Create `clang/utils/analyzer/aqb/cli.py`:

```python
from __future__ import annotations

import argparse
import sys
from typing import List, Optional

STUB_COMMANDS = ("run", "diff", "plot", "report", "promote")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="aqb", description="Analysis Qualification Bench"
    )
    parser.add_argument(
        "--home",
        default=None,
        help="AQB data directory (default: $AQB_HOME or ./.aqb)",
    )
    sub = parser.add_subparsers(dest="command")

    for name in STUB_COMMANDS:
        stub = sub.add_parser(name, help=f"{name} (not yet implemented)")
        stub.set_defaults(func=cmd_not_implemented, command_name=name)

    return parser


def cmd_not_implemented(args: argparse.Namespace) -> int:
    print(f"aqb {args.command_name}: not yet implemented", file=sys.stderr)
    return 2


def main(argv: Optional[List[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    if not getattr(args, "command", None):
        parser.print_help()
        return 0
    return args.func(args)
```

Create `clang/utils/analyzer/aqb/__main__.py`:

```python
from __future__ import annotations

import sys

from aqb.cli import main

if __name__ == "__main__":
    sys.exit(main())
```

- [ ] **Step 4: Run test to verify it passes**

Run: `python -m unittest aqb.tests.test_cli -v`
Expected: PASS (2 tests).

- [ ] **Step 5: Commit**

```bash
black aqb/
git add clang/utils/analyzer/aqb/__init__.py clang/utils/analyzer/aqb/cli.py clang/utils/analyzer/aqb/__main__.py clang/utils/analyzer/aqb/tests/__init__.py clang/utils/analyzer/aqb/tests/test_cli.py
git commit -m "feat(aqb): package scaffold + CLI skeleton"
```

---

### Task 2: Run IDs

**Files:**
- Create: `clang/utils/analyzer/aqb/runid.py`
- Test: `clang/utils/analyzer/aqb/tests/test_runid.py`

- [ ] **Step 1: Write the failing test**

Create `clang/utils/analyzer/aqb/tests/test_runid.py`:

```python
from __future__ import annotations

import datetime
import unittest

from aqb.runid import new_run_id


class RunIdTest(unittest.TestCase):
    def test_format_is_deterministic_when_injected(self):
        now = datetime.datetime(2026, 7, 16, 13, 15, 0)
        self.assertEqual(
            new_run_id(now=now, suffix="a1b2"), "r-20260716-131500-a1b2"
        )

    def test_two_default_ids_differ(self):
        self.assertNotEqual(new_run_id(), new_run_id())
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python -m unittest aqb.tests.test_runid -v`
Expected: FAIL — `ModuleNotFoundError: No module named 'aqb.runid'`.

- [ ] **Step 3: Write minimal implementation**

Create `clang/utils/analyzer/aqb/runid.py`:

```python
from __future__ import annotations

import datetime
import secrets
from typing import Optional


def new_run_id(
    prefix: str = "r",
    now: Optional[datetime.datetime] = None,
    suffix: Optional[str] = None,
) -> str:
    """Return a sortable, unique run id like ``r-20260716-131500-a1b2``.

    ``now`` and ``suffix`` may be injected to make the value deterministic in
    tests; otherwise the current UTC time and a random 2-byte suffix are used.
    """
    if now is None:
        now = datetime.datetime.now(datetime.timezone.utc)
    if suffix is None:
        suffix = secrets.token_hex(2)
    stamp = now.strftime("%Y%m%d-%H%M%S")
    return f"{prefix}-{stamp}-{suffix}"
```

- [ ] **Step 4: Run test to verify it passes**

Run: `python -m unittest aqb.tests.test_runid -v`
Expected: PASS (2 tests).

- [ ] **Step 5: Commit**

```bash
black aqb/
git add clang/utils/analyzer/aqb/runid.py clang/utils/analyzer/aqb/tests/test_runid.py
git commit -m "feat(aqb): sortable unique run ids"
```

---

### Task 3: Metadata dataclasses + JSON round-trip

**Files:**
- Create: `clang/utils/analyzer/aqb/metadata.py`
- Test: `clang/utils/analyzer/aqb/tests/test_metadata.py`

The `Metadata` shape mirrors the design spec's `metadata.json` (`AQB-design.rst`, "The metadata.json File"): analyzer provenance (commit, commit_title, config_digest, volume), container (runtime, image_digest), execution (n, analyzer_args, note), corpus (per-project provenance), plus `run_id`, `kind`, and `created`.

- [ ] **Step 1: Write the failing test**

Create `clang/utils/analyzer/aqb/tests/test_metadata.py`:

```python
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
                name="curl", source="https://github.com/curl/curl",
                commit="deadbeef", commit_title="curl 8.9",
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
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python -m unittest aqb.tests.test_metadata -v`
Expected: FAIL — `ModuleNotFoundError: No module named 'aqb.metadata'`.

- [ ] **Step 3: Write minimal implementation**

Create `clang/utils/analyzer/aqb/metadata.py`:

```python
from __future__ import annotations

import dataclasses
import json
from dataclasses import dataclass, field
from typing import List


@dataclass
class AnalyzerProvenance:
    commit: str
    commit_title: str = ""
    config_digest: str = ""
    volume: str = ""


@dataclass
class ProjectProvenance:
    name: str
    source: str = ""
    commit: str = ""
    commit_title: str = ""


@dataclass
class ContainerProvenance:
    runtime: str = "docker"
    image_digest: str = ""


@dataclass
class ExecutionProvenance:
    n: int = 1
    analyzer_args: List[str] = field(default_factory=list)
    note: str = ""


@dataclass
class Metadata:
    run_id: str
    kind: str  # "functional" | "benchmark"
    created: str  # ISO-8601
    analyzer: AnalyzerProvenance
    container: ContainerProvenance
    execution: ExecutionProvenance
    corpus: List[ProjectProvenance] = field(default_factory=list)

    def to_json(self) -> str:
        return json.dumps(dataclasses.asdict(self), indent=2, sort_keys=True)

    @classmethod
    def from_json(cls, text: str) -> "Metadata":
        raw = json.loads(text)
        return cls(
            run_id=raw["run_id"],
            kind=raw["kind"],
            created=raw["created"],
            analyzer=AnalyzerProvenance(**raw["analyzer"]),
            container=ContainerProvenance(**raw["container"]),
            execution=ExecutionProvenance(**raw["execution"]),
            corpus=[ProjectProvenance(**p) for p in raw.get("corpus", [])],
        )
```

- [ ] **Step 4: Run test to verify it passes**

Run: `python -m unittest aqb.tests.test_metadata -v`
Expected: PASS (2 tests).

- [ ] **Step 5: Commit**

```bash
black aqb/
git add clang/utils/analyzer/aqb/metadata.py clang/utils/analyzer/aqb/tests/test_metadata.py
git commit -m "feat(aqb): metadata dataclasses with json round-trip"
```

---

### Task 4: Run Store (create / get / list / resolve)

**Files:**
- Create: `clang/utils/analyzer/aqb/store.py`
- Test: `clang/utils/analyzer/aqb/tests/test_store.py`

- [ ] **Step 1: Write the failing test**

Create `clang/utils/analyzer/aqb/tests/test_store.py`:

```python
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
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python -m unittest aqb.tests.test_store -v`
Expected: FAIL — `ModuleNotFoundError: No module named 'aqb.store'`.

- [ ] **Step 3: Write minimal implementation**

Create `clang/utils/analyzer/aqb/store.py`:

```python
from __future__ import annotations

import os
from typing import List

from aqb.metadata import Metadata

RUN_SUBDIRS = ("reports", "metrics", "logs")


class RunNotFoundError(Exception):
    pass


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
        matches = [r for r in self.list_runs() if r.startswith(prefix)]
        if len(matches) == 1:
            return matches[0]
        if not matches:
            raise RunNotFoundError(prefix)
        raise RunNotFoundError(f"ambiguous run id prefix: {prefix} -> {matches}")
```

- [ ] **Step 4: Run test to verify it passes**

Run: `python -m unittest aqb.tests.test_store -v`
Expected: PASS (5 tests).

- [ ] **Step 5: Commit**

```bash
black aqb/
git add clang/utils/analyzer/aqb/store.py clang/utils/analyzer/aqb/tests/test_store.py
git commit -m "feat(aqb): filesystem run store"
```

---

### Task 5: Wire the `list` command to the Run Store

**Files:**
- Modify: `clang/utils/analyzer/aqb/cli.py`
- Test: `clang/utils/analyzer/aqb/tests/test_cli.py` (add a case)

- [ ] **Step 1: Write the failing test**

Append to `clang/utils/analyzer/aqb/tests/test_cli.py` (add these imports at the top of the file: `import os`, `import tempfile`, and `from aqb.metadata import AnalyzerProvenance, ContainerProvenance, ExecutionProvenance, Metadata`, `from aqb.store import RunStore`):

```python
    def test_list_prints_stored_run_ids(self):
        with tempfile.TemporaryDirectory() as root:
            RunStore(root).create_run(
                Metadata(
                    run_id="r-xyz",
                    kind="functional",
                    created="2026-07-16T00:00:00+00:00",
                    analyzer=AnalyzerProvenance(commit="c"),
                    container=ContainerProvenance(),
                    execution=ExecutionProvenance(),
                )
            )
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                code = main(["--home", root, "list"])
            self.assertEqual(code, 0)
            self.assertIn("r-xyz", out.getvalue())
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python -m unittest aqb.tests.test_cli -v`
Expected: FAIL — `list` is not a registered command (argparse error / SystemExit).

- [ ] **Step 3: Write minimal implementation**

In `clang/utils/analyzer/aqb/cli.py`, add imports and a home resolver, register the `list` command, and add its handler. Replace the file's contents with:

```python
from __future__ import annotations

import argparse
import os
import sys
from typing import List, Optional

from aqb.store import RunStore

STUB_COMMANDS = ("run", "diff", "plot", "report", "promote")


def default_home() -> str:
    return os.environ.get("AQB_HOME", os.path.join(os.getcwd(), ".aqb"))


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="aqb", description="Analysis Qualification Bench"
    )
    parser.add_argument(
        "--home",
        default=None,
        help="AQB data directory (default: $AQB_HOME or ./.aqb)",
    )
    sub = parser.add_subparsers(dest="command")

    list_parser = sub.add_parser("list", help="list stored runs")
    list_parser.set_defaults(func=cmd_list)

    for name in STUB_COMMANDS:
        stub = sub.add_parser(name, help=f"{name} (not yet implemented)")
        stub.set_defaults(func=cmd_not_implemented, command_name=name)

    return parser


def cmd_list(args: argparse.Namespace) -> int:
    store = RunStore(args.home or default_home())
    for run_id in store.list_runs():
        print(run_id)
    return 0


def cmd_not_implemented(args: argparse.Namespace) -> int:
    print(f"aqb {args.command_name}: not yet implemented", file=sys.stderr)
    return 2


def main(argv: Optional[List[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    if not getattr(args, "command", None):
        parser.print_help()
        return 0
    return args.func(args)
```

- [ ] **Step 4: Run test to verify it passes**

Run: `python -m unittest aqb.tests.test_cli -v`
Expected: PASS (3 tests).

- [ ] **Step 5: Commit**

```bash
black aqb/
git add clang/utils/analyzer/aqb/cli.py clang/utils/analyzer/aqb/tests/test_cli.py
git commit -m "feat(aqb): list command backed by the run store"
```

---

### Task 6: SATest reuse shim

Proves the approach-B reuse path works: `aqb` can import the sibling SATest modules it will build on in later phases.

**Files:**
- Create: `clang/utils/analyzer/aqb/_satest.py`
- Test: `clang/utils/analyzer/aqb/tests/test_satest.py`

- [ ] **Step 1: Write the failing test**

Create `clang/utils/analyzer/aqb/tests/test_satest.py`:

```python
from __future__ import annotations

import unittest

from aqb import _satest


class SatestReuseTest(unittest.TestCase):
    def test_cmpruns_diff_api_is_importable(self):
        # The two-tier diff engine we reuse in Phase 4.
        self.assertTrue(hasattr(_satest.CmpRuns, "compare_results"))
        self.assertTrue(hasattr(_satest.CmpRuns, "load_results_from_single_run"))

    def test_projectmap_corpus_api_is_importable(self):
        # The corpus registry we reuse in Phase 3.
        self.assertTrue(hasattr(_satest.ProjectMap, "ProjectInfo"))
        self.assertTrue(hasattr(_satest.ProjectMap, "ProjectMap"))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python -m unittest aqb.tests.test_satest -v`
Expected: FAIL — `ModuleNotFoundError: No module named 'aqb._satest'`.

- [ ] **Step 3: Write minimal implementation**

Create `clang/utils/analyzer/aqb/_satest.py`:

```python
"""Bridge to the sibling SATest modules AQB reuses.

The SATest scripts live in the parent directory (``clang/utils/analyzer/``) as
top-level modules, not a package. Put that directory on ``sys.path`` so they can
be imported, then re-export the ones AQB builds on.
"""

from __future__ import annotations

import os
import sys

_SATEST_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
if _SATEST_DIR not in sys.path:
    sys.path.insert(0, _SATEST_DIR)

import CmpRuns  # noqa: E402  (import after sys.path tweak)
import ProjectMap  # noqa: E402

__all__ = ["CmpRuns", "ProjectMap"]
```

- [ ] **Step 4: Run test to verify it passes**

Run: `python -m unittest aqb.tests.test_satest -v`
Expected: PASS (2 tests).

> If import fails because `CmpRuns`/`ProjectMap` pull in a heavy optional dependency (e.g. `matplotlib`/`pandas` via a transitive import), narrow the shim to import only what's needed, or install the dep from `clang/utils/analyzer/requirements.txt`. As of the ground-truth review, `CmpRuns` and `ProjectMap` import only stdlib (`plistlib`, `typing`, `enum`).

- [ ] **Step 5: Commit**

```bash
black aqb/
git add clang/utils/analyzer/aqb/_satest.py clang/utils/analyzer/aqb/tests/test_satest.py
git commit -m "feat(aqb): reuse shim for sibling SATest modules"
```

---

### Task 7: Phase 1 green check

- [ ] **Step 1: Run the whole suite**

Run: `python -m unittest discover -s aqb/tests -t . -v`
Expected: PASS — all Phase 1 tests (test_cli, test_runid, test_metadata, test_store, test_satest).

- [ ] **Step 2: Confirm formatting is clean**

Run: `black --check aqb/`
Expected: "All done!" with no files reformatted.

- [ ] **Step 3: Commit any formatting fixes (if `black --check` failed)**

```bash
black aqb/
git add clang/utils/analyzer/aqb/
git commit -m "style(aqb): black formatting"
```

---

## Phase 2–5: Sequenced Stubs

> These are scope stubs, **not executable tasks**. Expand each into its own full plan doc (`docs/superpowers/plans/YYYY-MM-DD-aqb-phaseN-*.md`) after the previous phase is built and reviewed. Each references `AQB-design.rst` as the source of truth.

### Phase 2: Runtime & Clang Volumes

**Goal:** `aqb` can select a docker-compatible runtime and resolve-or-build a content-addressed Clang Volume for a `(commit, build config)`, plus manage the shared ccache volume.

**Key files:** `aqb/runtime.py` (runtime name resolution: `--runtime` flag > `AQB_RUNTIME` > `docker`; thin wrappers over `volume inspect/create`, `run`, `image inspect --format {{.Id}}` for the digest), `aqb/volume.py` (config-digest computation, `aqb-clang-<shortcommit>-<configdigest>` naming, label set incl. non-digested `aqb.created`, resolve-or-build flow, `aqb-ccache` handling).

**Representative work:** digest is a stable hash of the normalized build config (cmake flags + assertions + builder image), **excluding** the timestamp and the run-level note; runtime commands built by substituting the resolved program name; build failure removes the partial volume.

**Testable without a daemon:** inject a fake "runtime runner" (a callable capturing argv) so digest/name/label/flow logic is unit-tested without docker; gate any real-daemon test behind an env check so CI without docker still passes.

**Depends on:** Phase 1.

### Phase 3: Analyze Pipeline (`aqb run`)

**Goal:** `aqb run --commit <sha> [--source <path>] [--note ...] [--projects ...]` produces a functional run: materialize corpus → analyze with the built clang → observe → normalize → dedup; reports (plist) + per-TU/entry-point metrics land in the run artifact; `metadata.json` is fully populated.

**Key files:** `aqb/corpus.py` (wrap `ProjectMap`/`projects.json`, selection by name/size), `aqb/analyze.py` (reuse `SATestBuild` scan-build mechanics inside the analyze container; enable stats channels: `-analyzer-stats` / `serialize-stats=true` / `dump-entry-point-stats-to-csv`), `aqb/normalize.py` (relativize paths, strip transient fields — reuse `CmpRuns` loading), `aqb/metrics.py` (parse TU + per-entry-point CSV keyed by USR, dedup).

**Depends on:** Phases 1, 2. **Reuses:** `SATestBuild.py`, `ProjectMap.py`, `CmpRuns.load_results_from_single_run`.

### Phase 4: Compare & Verdict (`aqb diff`, `aqb promote`)

**Goal:** `aqb diff --base <run> --new <run>` and `aqb diff --baseline --new <run>` classify reports (unchanged/added/removed/modified) + metric deltas; `--expect {no-crashes,same-count,same-reports}` sets the exit code; `aqb promote <run>` writes the committed baseline.

**Key files:** `aqb/reference.py` (the Reference abstraction: committed-baseline | another-run | none), `aqb/diff.py` (adapt AQB runs into `CmpRuns.ResultsDirectory`/`AnalysisRun`, call `compare_results`, render classification), `aqb/verdict.py` (`--expect` policy → exit code; `same-reports` bypasses Tier 2), `aqb/baseline.py` (promote lifecycle).

**Depends on:** Phases 1, 3. **Reuses:** `CmpRuns.compare_results`, `ComparisonResult`, `dump_scan_build_results_diff`.

### Phase 5: Benchmark & Present (`aqb run --bench`, `aqb plot`, `aqb report`)

**Goal:** `aqb run --bench -n N` runs the same `(config, project)` N times, capturing per-(TU, metric) samples (reports ignored, robustness still checked); `aqb plot <run>...` renders a full report of overlaid candlesticks joined on `(project, TU/entry-point, metric)` and labeled by provenance; `aqb report <run>` renders the self-contained HTML/JSON.

**Key files:** `aqb/benchmark.py` (N-iteration loop; reuse `SATestBenchmark` cost sampling; per-iteration metrics into `metrics/`), `aqb/candlestick.py` (min/max/quartiles/median per series), `aqb/plot.py` (multi-run overlay), `aqb/report.py` (HTML/JSON rendering).

**Depends on:** Phases 1, 3. **Reuses:** `SATestBenchmark.py`.

---

## Self-Review

**Spec coverage (Phase 1 scope):** Run Store + layout → Task 4; `metadata.json` provenance shape (analyzer/container/execution/corpus, note on runs) → Task 3; run IDs → Task 2; CLI verb surface (`run`/`diff`/`plot`/`report`/`promote` present as stubs, `list` live) → Tasks 1, 5; approach-B reuse of `CmpRuns`/`ProjectMap` → Task 6. Runtime, Clang Volumes, ccache, analyze, diff/`--expect`/promote, benchmark/plot/report are **intentionally deferred** to Phases 2–5 (scoped above), each producing working software.

**Placeholder scan:** No "TBD"/"handle edge cases"/"write tests for the above" in Phase 1 — every task ships full test + implementation code. Phases 2–5 are explicitly labeled non-executable stubs.

**Type consistency:** `Metadata`/`AnalyzerProvenance`/`ContainerProvenance`/`ExecutionProvenance`/`ProjectProvenance` field names are identical across Task 3 (definition), Task 4 (`_meta` helper), and Task 5 (CLI test). `RunStore` methods (`create_run`, `get`, `list_runs`, `resolve`) and `RunNotFoundError` are used consistently in Tasks 4–5. `new_run_id` signature matches its Task 2 test. `main(argv)` and `build_parser()` are stable across Tasks 1 and 5.
