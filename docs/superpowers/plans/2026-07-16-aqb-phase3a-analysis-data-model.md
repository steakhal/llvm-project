# AQB Phase 3a — Analysis Data Model Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the pure, daemon-free **analysis data model** that later sub-phases feed and consume: corpus selection from `projects.json`, parsing of the analyzer's statistics outputs (per-entry-point CSV + TU-level JSON) with deduplication, and normalization+dedup of analyzer findings by reusing SATest's `CmpRuns`.

**Architecture:** Three focused modules under `clang/utils/analyzer/aqb/` — `corpus.py`, `metrics.py`, `normalize.py`. Everything here is pure Python operating on in-memory strings, temp `projects.json` files, or synthetic `.plist` fixtures; there is **no** container/daemon/clang dependency, so the whole layer is unit-tested with `unittest`. `normalize.py` reuses `CmpRuns` (via the Phase-1 `aqb._satest` shim) for report loading and the two-tier issue identity. Design source of truth: `clang/docs/analyzer/developer-docs/AQB-design.rst` (*Two Signal Streams*, *Measurements and Metrics*, *Report Identity*).

**Tech Stack:** Python ≥3.8 (stdlib only — `csv`, `io`, `json`, `dataclasses`, `plistlib` in tests; `unittest`), formatted with `python3 -m black`.

---

## Phase 3 decomposition

Phase 3 (`aqb run --commit` end-to-end) is split into three sequenced sub-plans; this document is **3a**. 3b/3c are stubbed at the end and expanded into their own plan docs after 3a is built and reviewed.

- **3a — Analysis data model** *(this doc)*: `corpus.py`, `metrics.py`, `normalize.py`. Pure/TDD.
- **3b — Builder image + volume hardening**: builder Dockerfile + build script (honors the `AQB_*` env contract documented in `volume.py`), build-completion marker so an interrupted build isn't treated as a valid cache, `resolve_or_build_clang` returns `(volume_name, config_digest, built)`, lossless cmake-arg transport, `.Id`-vs-repo-digest decision.
- **3c — Analyze + run orchestration**: `analyze.py` (scan-build invocation reusing `SATestBuild` mechanics + injection of `dump-entry-point-stats-to-csv` / `serialize-stats=true` / `-analyzer-stats`, `:ro` clang mount), the `aqb run --commit` CLI orchestration (materialize corpus → resolve/build clang → analyze → observe → normalize + metrics → populate `Metadata` → persist run), and distinguishing daemon-down from volume-absent.

## Conventions (apply to every task)

- **Package location:** `clang/utils/analyzer/aqb/`. Repo root `/Users/benics/git/upstream-llvm-ssaf`.
- **Run all test/black commands from `clang/utils/analyzer/`** so `aqb` and the sibling SATest modules import. Whole suite: `python3 -m unittest discover -s aqb/tests -t . -v`.
- **`black` is `python3 -m black`** (25.11.0). Run `python3 -m black aqb/` before each commit; confirm `python3 -m black --check aqb/` is clean.
- **Python 3.8 compat:** each new module starts with `from __future__ import annotations`; use `typing.List`/`Optional`/`Dict`/`Sequence`/`Iterable`, never PEP 604 `X | None`.
- **Commit to `bb/aqb-design`.** Run git from repo root or with `git -C <root>`; never `cd` inside a compound git command. Stage only each task's named files.
- **Reuse, don't reinvent:** `normalize.py` uses `from aqb._satest import CmpRuns`; `corpus.py` uses `from aqb._satest import ProjectMap`. If a reused `CmpRuns`/`ProjectMap` attribute name differs from what this plan assumes (these are the reuse seams), STOP and report it — do not silently reshape the module. As of ground-truth review the assumed API is: `CmpRuns.load_results(CmpRuns.ResultsDirectory(path, root), delete_empty=...) -> AnalysisRun` with `.diagnostics: List[AnalysisDiagnostic]`; `AnalysisDiagnostic` has `get_issue_identifier()`, `get_file_name()`, `get_line()`, `get_column()`, `get_category()`, `get_description()`, `get_path_length()`, `get_raw_data()`; `ProjectMap.ProjectMap(path=...)` exposes `.projects: List[ProjectInfo]`; `ProjectMap.Size` is an enum with `TINY/SMALL/BIG/HUGE/UNSPECIFIED`.

## File Structure (Phase 3a)

- Create `clang/utils/analyzer/aqb/corpus.py` — `select_projects(project_map, names, sizes, include_disabled)`.
- Create `clang/utils/analyzer/aqb/metrics.py` — `EntryPointMetrics`, `parse_entry_point_csv`, `dedup_entry_points`, `parse_tu_stats_json`.
- Create `clang/utils/analyzer/aqb/normalize.py` — `Finding`, `load_findings`.
- Create tests: `test_corpus.py`, `test_metrics.py`, `test_normalize.py`.

---

## Task 1: Corpus selection

**Files:**
- Create: `clang/utils/analyzer/aqb/corpus.py`
- Test: `clang/utils/analyzer/aqb/tests/test_corpus.py`

- [ ] **Step 1: Write the failing test**

Create `clang/utils/analyzer/aqb/tests/test_corpus.py`:

```python
from __future__ import annotations

import json
import os
import tempfile
import unittest

from aqb._satest import ProjectMap
from aqb.corpus import select_projects

PROJECTS = [
    {"name": "curl", "mode": 1, "source": "git",
     "origin": "https://x/curl.git", "commit": "aaaa", "size": "small"},
    {"name": "redis", "mode": 1, "source": "git",
     "origin": "https://x/redis.git", "commit": "bbbb", "size": "tiny",
     "enabled": False},
    {"name": "box2d", "mode": 1, "source": "git",
     "origin": "https://x/box2d.git", "commit": "cccc", "size": "tiny"},
]


def _load_map(root: str) -> "ProjectMap.ProjectMap":
    path = os.path.join(root, "projects.json")
    with open(path, "w") as handle:
        json.dump(PROJECTS, handle)
    return ProjectMap.ProjectMap(path=path)


class SelectProjectsTest(unittest.TestCase):
    def test_drops_disabled_by_default(self):
        with tempfile.TemporaryDirectory() as root:
            names = [p.name for p in select_projects(_load_map(root))]
            self.assertEqual(names, ["curl", "box2d"])

    def test_include_disabled(self):
        with tempfile.TemporaryDirectory() as root:
            names = [
                p.name
                for p in select_projects(_load_map(root), include_disabled=True)
            ]
            self.assertEqual(names, ["curl", "redis", "box2d"])

    def test_filter_by_name(self):
        with tempfile.TemporaryDirectory() as root:
            names = [p.name for p in select_projects(_load_map(root), names=["curl"])]
            self.assertEqual(names, ["curl"])

    def test_filter_by_size_respects_enabled(self):
        with tempfile.TemporaryDirectory() as root:
            # box2d is tiny+enabled; redis is tiny but disabled -> excluded.
            selected = select_projects(_load_map(root), sizes=[ProjectMap.Size.TINY])
            self.assertEqual([p.name for p in selected], ["box2d"])
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python3 -m unittest aqb.tests.test_corpus -v`
Expected: FAIL — `ModuleNotFoundError: No module named 'aqb.corpus'`.

- [ ] **Step 3: Write minimal implementation**

Create `clang/utils/analyzer/aqb/corpus.py`:

```python
from __future__ import annotations

from typing import List, Optional, Sequence

from aqb._satest import ProjectMap


def select_projects(
    project_map: "ProjectMap.ProjectMap",
    names: Optional[Sequence[str]] = None,
    sizes: Optional[Sequence["ProjectMap.Size"]] = None,
    include_disabled: bool = False,
) -> List["ProjectMap.ProjectInfo"]:
    """Select projects from a loaded ProjectMap, preserving map order.

    - ``names``: if given, keep only projects whose name is in this set.
    - ``sizes``: if given, keep only projects whose size is in this set.
    - ``include_disabled``: unless True, projects with ``enabled=False`` are
      dropped.
    """
    name_set = set(names) if names is not None else None
    size_set = set(sizes) if sizes is not None else None

    selected: List["ProjectMap.ProjectInfo"] = []
    for project in project_map.projects:
        if not include_disabled and not project.enabled:
            continue
        if name_set is not None and project.name not in name_set:
            continue
        if size_set is not None and project.size not in size_set:
            continue
        selected.append(project)
    return selected
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `python3 -m unittest aqb.tests.test_corpus -v`
Expected: PASS (4 tests).

- [ ] **Step 5: Format + commit**

```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb/corpus.py clang/utils/analyzer/aqb/tests/test_corpus.py
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "feat(aqb): corpus selection over projects.json"
```

---

## Task 2: Per-entry-point CSV parsing + dedup

Parses the `dump-entry-point-stats-to-csv` output. Ground truth: header is `USR,File,DebugName,<sorted stat names>`; USR/File/DebugName are quoted; unset `UnsignedEPStat` cells are empty; counter/max cells are always present.

**Files:**
- Create: `clang/utils/analyzer/aqb/metrics.py`
- Test: `clang/utils/analyzer/aqb/tests/test_metrics.py`

- [ ] **Step 1: Write the failing test**

Create `clang/utils/analyzer/aqb/tests/test_metrics.py`:

```python
from __future__ import annotations

import unittest

from aqb.metrics import dedup_entry_points, parse_entry_point_csv

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
        # UnsignedEPStat columns were unset -> not present at all.
        self.assertNotIn("CFGSize", main.stats)
        self.assertNotIn("PathRunningTime", main.stats)
        # Counter column is present.
        self.assertEqual(main.stats["NumSteps"], 240)

    def test_empty_input_yields_no_rows(self):
        self.assertEqual(parse_entry_point_csv(""), [])

    def test_dedup_keeps_first_per_usr(self):
        rows = parse_entry_point_csv(CSV)
        # Same USR appearing twice (e.g. a header analyzed via two TUs).
        deduped = dedup_entry_points(rows + [rows[0]])
        self.assertEqual([r.usr for r in deduped], ["c:@F@fib#i#", "c:@F@main#"])
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python3 -m unittest aqb.tests.test_metrics -v`
Expected: FAIL — `ModuleNotFoundError: No module named 'aqb.metrics'`.

- [ ] **Step 3: Write minimal implementation**

Create `clang/utils/analyzer/aqb/metrics.py`:

```python
from __future__ import annotations

import csv
import io
from dataclasses import dataclass
from typing import Dict, Iterable, List

# The fixed leading columns of the per-entry-point CSV (see AQB-design.rst /
# EntryPointStats.cpp dumpStatsAsCSV).
EP_CSV_FIXED_COLUMNS = ("USR", "File", "DebugName")


@dataclass
class EntryPointMetrics:
    """Per-entry-point statistics for one analyzed function.

    ``stats`` holds only the columns that had a value; unset ``UnsignedEPStat``
    columns (empty CSV cells) are omitted.
    """

    usr: str
    file: str
    debug_name: str
    stats: Dict[str, int]


def parse_entry_point_csv(text: str) -> List[EntryPointMetrics]:
    """Parse ``dump-entry-point-stats-to-csv`` output into a list of rows."""
    rows = list(csv.reader(io.StringIO(text)))
    if not rows:
        return []
    stat_names = rows[0][len(EP_CSV_FIXED_COLUMNS) :]
    result: List[EntryPointMetrics] = []
    for row in rows[1:]:
        if not row:
            continue
        usr, file, debug_name = row[0], row[1], row[2]
        stats: Dict[str, int] = {}
        for name, cell in zip(stat_names, row[len(EP_CSV_FIXED_COLUMNS) :]):
            if cell != "":
                stats[name] = int(cell)
        result.append(
            EntryPointMetrics(usr=usr, file=file, debug_name=debug_name, stats=stats)
        )
    return result


def dedup_entry_points(
    rows: Iterable[EntryPointMetrics],
) -> List[EntryPointMetrics]:
    """Keep one row per USR (first wins) so a header analyzed through many TUs
    is not counted multiple times."""
    seen: Dict[str, EntryPointMetrics] = {}
    for row in rows:
        if row.usr not in seen:
            seen[row.usr] = row
    return list(seen.values())
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `python3 -m unittest aqb.tests.test_metrics -v`
Expected: PASS (4 tests).

- [ ] **Step 5: Format + commit**

```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb/metrics.py clang/utils/analyzer/aqb/tests/test_metrics.py
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "feat(aqb): parse per-entry-point stats CSV with dedup"
```

---

## Task 3: TU-level statistics JSON parsing

Parses `serialize-stats=true` output (`llvm::PrintStatisticsJSON`), a JSON object mapping `"<debugtype>.<name>"` to an integer.

**Files:**
- Modify: `clang/utils/analyzer/aqb/metrics.py`
- Test: `clang/utils/analyzer/aqb/tests/test_metrics.py` (add a case)

- [ ] **Step 1: Write the failing test**

Append to `clang/utils/analyzer/aqb/tests/test_metrics.py` — add `from aqb.metrics import parse_tu_stats_json` to the existing import line's module (i.e. add `parse_tu_stats_json` to the `from aqb.metrics import ...` list), then add this test class:

```python
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
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python3 -m unittest aqb.tests.test_metrics -v`
Expected: FAIL — `ImportError: cannot import name 'parse_tu_stats_json'`.

- [ ] **Step 3: Extend `metrics.py`**

Add `import json` to the imports at the top of `clang/utils/analyzer/aqb/metrics.py` (alongside `import csv`, `import io`), then append this function:

```python
def parse_tu_stats_json(text: str) -> Dict[str, float]:
    """Parse ``serialize-stats=true`` / ``PrintStatisticsJSON`` output.

    The output is a JSON object mapping ``"<debugtype>.<name>"`` to a numeric
    value. Most are integer counters, but ``PrintStatisticsJSON`` also appends
    ``TimerGroup`` values (keys like ``"time.<group>.<name>.wall"``) whose
    values are floating-point. Values are therefore preserved with their JSON
    numeric type (``int`` for counters, ``float`` for timers) rather than
    coerced to ``int`` — coercing would silently truncate sub-second timings
    to zero.
    """
    raw = json.loads(text)
    return dict(raw)
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `python3 -m unittest aqb.tests.test_metrics -v`
Expected: PASS (6 tests total: 4 CSV + 2 TU-JSON).

- [ ] **Step 5: Format + commit**

```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb/metrics.py clang/utils/analyzer/aqb/tests/test_metrics.py
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "feat(aqb): parse TU-level statistics JSON"
```

---

## Task 4: Findings normalization + dedup (reusing CmpRuns)

Loads analyzer plist reports via SATest's `CmpRuns`, canonicalizes each into a `Finding`, relativizes paths against the project root, and deduplicates by the stable two-tier issue identifier. Uses `delete_empty=False` so input plists are never mutated.

**Files:**
- Create: `clang/utils/analyzer/aqb/normalize.py`
- Test: `clang/utils/analyzer/aqb/tests/test_normalize.py`

- [ ] **Step 1: Write the failing test**

Create `clang/utils/analyzer/aqb/tests/test_normalize.py`:

```python
from __future__ import annotations

import os
import plistlib
import tempfile
import unittest

from aqb.normalize import load_findings


def _diag(file_idx, line, col, checker, category, desc, ctx, issue_hash):
    return {
        "location": {"file": file_idx, "line": line, "col": col},
        "path": [{"kind": "event"}],  # path length 1
        "check_name": checker,
        "category": category,
        "description": desc,
        "issue_context": ctx,
        "issue_hash_content_of_line_in_context": issue_hash,
        "HTMLDiagnostics_files": [],
    }


def _write_plist(path, files, diagnostics):
    with open(path, "wb") as handle:
        plistlib.dump(
            {"files": files, "diagnostics": diagnostics, "clang_version": "x"}, handle
        )


class LoadFindingsTest(unittest.TestCase):
    def test_dedups_by_issue_identity_and_relativizes(self):
        with tempfile.TemporaryDirectory() as root:
            files = [os.path.join(root, "src", "a.c")]
            diags = [
                _diag(0, 10, 4, "core.NullDeref", "Logic error", "deref of null",
                      "func_f", 111),
                _diag(0, 20, 2, "core.DivZero", "Logic error", "divide by zero",
                      "func_g", 222),
                # Duplicate of the first (same file+context+hash) -> same issue.
                _diag(0, 10, 4, "core.NullDeref", "Logic error", "deref of null",
                      "func_f", 111),
            ]
            _write_plist(os.path.join(root, "report.plist"), files, diags)

            findings = load_findings(root, project_root=root)

            # Three diagnostics, two distinct issues.
            self.assertEqual(len(findings), 2)
            ids = {f.issue_id for f in findings}
            self.assertEqual(len(ids), 2)
            # A finding carries its attributes.
            null = next(f for f in findings if f.checker == "core.NullDeref")
            self.assertEqual(null.line, 10)
            self.assertEqual(null.column, 4)
            self.assertEqual(null.category, "Logic error")
            self.assertEqual(null.path_length, 1)
            # Path is relative to project_root (root prefix stripped).
            self.assertNotIn(root, null.file)
            self.assertTrue(null.file.endswith("a.c"))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python3 -m unittest aqb.tests.test_normalize -v`
Expected: FAIL — `ModuleNotFoundError: No module named 'aqb.normalize'`.

- [ ] **Step 3: Write minimal implementation**

Create `clang/utils/analyzer/aqb/normalize.py`:

```python
from __future__ import annotations

from dataclasses import dataclass
from typing import List, Set

from aqb._satest import CmpRuns


@dataclass(frozen=True)
class Finding:
    """A normalized, deduplicated analyzer report."""

    issue_id: str
    file: str
    line: int
    column: int
    checker: str
    category: str
    description: str
    path_length: int


def _to_finding(diag) -> Finding:
    raw = diag.get_raw_data()
    return Finding(
        issue_id=diag.get_issue_identifier(),
        file=diag.get_file_name(),
        line=diag.get_line(),
        column=diag.get_column(),
        checker=raw.get("check_name", ""),
        category=diag.get_category(),
        description=diag.get_description(),
        path_length=diag.get_path_length(),
    )


def load_findings(results_dir: str, project_root: str = "") -> List[Finding]:
    """Load, normalize, and deduplicate analyzer findings from a results dir.

    Reuses SATest's ``CmpRuns`` loader, which walks ``results_dir`` for
    ``*.plist`` files and canonicalizes each report. Paths are made relative to
    ``project_root``. Findings are deduplicated by their stable issue identifier
    (first wins), so a header analyzed through many TUs yields one finding per
    distinct issue. ``delete_empty=False`` ensures the input plists are never
    mutated.
    """
    run = CmpRuns.load_results(
        CmpRuns.ResultsDirectory(path=results_dir, root=project_root),
        delete_empty=False,
    )
    findings: List[Finding] = []
    seen: Set[str] = set()
    for diag in run.diagnostics:
        issue_id = diag.get_issue_identifier()
        if issue_id in seen:
            continue
        seen.add(issue_id)
        findings.append(_to_finding(diag))
    return findings
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `python3 -m unittest aqb.tests.test_normalize -v`
Expected: PASS (1 test).

> If `CmpRuns.load_results` needs a plist key the fixture omits (e.g. it unconditionally pops a field), add that key to `_diag`/`_write_plist` in the test — that is legitimate fixture completion, not gaming. If the `CmpRuns` API name/shape differs from the assumption (e.g. no `load_results` or a different `ResultsDirectory`), STOP and report it per the reuse-seam convention rather than reshaping `CmpRuns`.

- [ ] **Step 5: Format + commit**

```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb/normalize.py clang/utils/analyzer/aqb/tests/test_normalize.py
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "feat(aqb): normalize + dedup findings via CmpRuns"
```

---

## Task 5: Phase 3a green check

- [ ] **Step 1: Run the whole suite**

Run: `python3 -m unittest discover -s aqb/tests -t . -v`
Expected: PASS — all Phase 1 + 2 + 3a tests (adds test_corpus ×4, test_metrics ×6, test_normalize ×1).

- [ ] **Step 2: Confirm formatting is clean**

Run: `python3 -m black --check aqb/`
Expected: "All done!" — no files reformatted.

- [ ] **Step 3: Commit any formatting fixes (only if `black --check` reported changes)**

```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "style(aqb): black formatting"
```

---

## Phase 3b / 3c — Sequenced Stubs

> Non-executable scope stubs; expand each into its own full plan doc after 3a is built and reviewed. Each references `AQB-design.rst` as source of truth and folds in the noted Phase-2 backlog items.

### Phase 3b: Builder image + volume hardening

**Goal:** A real builder image + build script that, given the `AQB_*` env contract (`_builder_run_argv` in `volume.py`), fetches `AQB_COMMIT` from `AQB_SOURCE` (git remote or absolute local clone), configures/builds/`cmake --install`s clang into `AQB_INSTALL_DIR` using `AQB_CCACHE_DIR`; plus volume-layer hardening.

**Key work / files:** builder `Dockerfile` + entry script (infra; validated by a daemon-gated smoke test, not unit tests); in `volume.py`: a **build-completion marker** (e.g. an `aqb.complete` sentinel written into the install tree, or a post-build label) so `resolve_or_build_clang` treats "volume exists but incomplete" as absent-and-rebuild (Phase-2 backlog #1); change `resolve_or_build_clang` to return `(volume_name, config_digest, built: bool)` (Phase-2 backlog #2); lossless cmake-arg transport to the builder — NUL/JSON/repeated env instead of space-join (Phase-2 backlog #4); decide `.Id` vs `RepoDigests` for the recorded image digest and apply consistently (Phase-2 backlog #6).

**Depends on:** Phase 2, 3a.

### Phase 3c: Analyze + run orchestration (`aqb run --commit`)

**Goal:** `aqb run --commit <sha> [--source <path>] [--note ...] [--projects ...] [--runtime ...]` produces a functional run: materialize corpus → resolve/build clang → analyze each project → observe → normalize + metrics → populate `Metadata` → persist a run.

**Key work / files:** `aqb/analyze.py` — build the scan-build invocation reusing `SATestBuild` mechanics; inject the stats channels into the `-analyzer-config` string (`serialize-stats=true` is already there; add `dump-entry-point-stats-to-csv=<path>`) plus `-analyzer-stats`; mount the Clang Volume `:ro` and the ccache volume; unit-test the argv/config construction with a fake runtime. `aqb/run.py` + CLI wiring for `run` — orchestrate the pipeline, populate `AnalyzerProvenance` (commit/commit_title/config_digest/volume from 3b's return) + `ProjectProvenance` + `ExecutionProvenance` and persist via `RunStore`; distinguish daemon-down from volume-absent for a clear error (Phase-2 backlog #5). End-to-end validated by a daemon-gated smoke test.

**Depends on:** Phase 2, 3a, 3b. **Reuses:** `SATestBuild.py`, `corpus.py`, `normalize.py`, `metrics.py`, `volume.py`, `runtime.py`, `store.py`, `metadata.py`.

---

## Self-Review

**Spec coverage (Phase 3a scope):** corpus selection from `projects.json` by name/size/enabled → Task 1; per-entry-point CSV parsing (fixed columns, empty-cell omission) + USR dedup → Task 2; TU-level `PrintStatisticsJSON` parsing → Task 3; findings normalization (path relativization) + two-tier issue-id dedup via `CmpRuns` → Task 4. The report/metrics **two signal streams** and the **dedup keys** (issue id for reports; USR for entry-point metrics) from `AQB-design.rst` are realized. Deferred by design: analyze execution, builder image, run orchestration → 3b/3c (stubbed).

**Placeholder scan:** No "TBD"/"handle errors"/"write tests for the above" — every task ships full test + implementation code. The `CmpRuns`/`ProjectMap` reuse points are real, ground-truth-verified APIs, not placeholders; the one contingency (fixture completion if the loader needs an extra plist key) is called out explicitly with a bounded instruction.

**Type consistency:** `EntryPointMetrics(usr, file, debug_name, stats)` defined in Task 2 and reused (import extension) in Task 3's file; `parse_entry_point_csv`/`dedup_entry_points`/`parse_tu_stats_json` names match their tests. `Finding(issue_id, file, line, column, checker, category, description, path_length)` fields in Task 4 match the test's assertions. `select_projects(project_map, names, sizes, include_disabled)` signature matches Task 1's test calls. All reuse-seam calls (`CmpRuns.load_results`, `CmpRuns.ResultsDirectory`, `ProjectMap.ProjectMap`, `ProjectMap.Size`) match the ground-truth API stated in Conventions.
