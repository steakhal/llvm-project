# AQB Phase 4 — `diff` verb (Design & Decomposition)

> **For agentic workers:** REQUIRED SUB-SKILL: superpowers:subagent-driven-development to execute this plan task-by-task. Steps use checkbox (`- [ ]`) syntax.

**Goal:** `aqb diff --base <run> --new <run>` classifies the report change between two stored runs (added / removed / changed / common) using SATest's two-tier report identity, prints a summary, and applies an `--expect` verdict as the exit code.

**STATUS: COMPLETE (2026-07-17).** All four tasks implemented + committed on `bb/aqb-design` (108 tests green). Verified end-to-end: self-diffing the stored zstd run (`r-20260717-221515-b593`) reports `common=985 added=0 removed=0 changed=0`, `--expect same-reports` → PASS. Deferred as documented: `--baseline` (awaits `promote`), metric-delta diffing, crash-aware `no-crashes`.

**Architecture:** The diff operates on the **stored, self-contained runs** (`reports/findings.json`), not by re-parsing plists — a stored `Finding` already carries every field CmpRuns' `compare_results` needs (location = `file`/`line`/`column`; tier-1 identity = `issue_id`; tier-2 similarity = `checker`/`category`/`description`; `path_length` for deltas). AQB ports that ~30-line two-tier algorithm to operate on `Finding` (the *identity* is still SATest's — `issue_id` comes from `CmpRuns.get_issue_identifier` via `normalize.load_findings`). This keeps runs diffable by reference, exactly as the design intends.

**Tech Stack:** stdlib only; `unittest`; runs loaded via `RunStore`.

---

## Scope (v1) — explicit decisions

- **Reports only.** v1 diff classifies the report streams + verdict. **Metric-delta diffing is deferred** (it belongs with the benchmark/plot half; a stored run already has `metrics/entry-point-stats.csv` for later).
- **`--base` / `--new` only.** `--baseline --new` needs a committed baseline, which `promote` (a later phase) creates. v1 supports two explicit runs; `--baseline` is added when `promote` lands. (Documented, not stubbed misleadingly.)
- **Verdict policies (`--expect`):** `no-crashes` (default — pass unless a crash is recorded; crashes aren't tracked in findings yet, so v1 treats a completed run as crash-free and this always passes — a placeholder honored now, tightened when crash artifacts land), `same-count` (pass iff total finding counts are equal), `same-reports` (pass iff nothing added/removed/changed). Fail → exit 1.
- **Corpus alignment:** findings are keyed per project (`findings.json` is `{project: [findings]}`). Diff each project present in *either* run; a project in only one run has all-added or all-removed findings. The summary notes corpus mismatches.

## Data model

```
ReportDelta (per run pair):
  common:  List[Finding]                       # same issue identity, same location
  added:   List[Finding]                        # only in new
  removed: List[Finding]                        # only in base
  changed: List[Tuple[Finding, Finding]]        # same location, similar (checker/category/description), different identity
```

## Decomposition

| Task | Deliverable | Test |
|---|---|---|
| **1 — compare_findings** | `aqb/diff.py`: `compare_findings(base, new) -> ReportDelta`, the two-tier algorithm ported to `Finding`. | pure unit tests (added/removed/common/changed) |
| **2 — diff a run pair** | `diff_runs(base_reports, new_reports) -> Dict[str, ReportDelta]` per project + a flat summary counter. | unit tests with fabricated per-project dicts |
| **3 — verdict** | `verdict(summary, expect) -> (passed: bool, reason: str)`. | unit tests per policy |
| **4 — CLI** | `aqb diff --base/--new [--expect] [--format text|json]`; loads both runs' `findings.json` from the store, prints summary, returns exit code. Drop `diff` from `STUB_COMMANDS`. | fake-store CLI tests |

---

## Task 1: `compare_findings` — the two-tier engine on `Finding`

**Files:** Create `clang/utils/analyzer/aqb/diff.py`; Test `clang/utils/analyzer/aqb/tests/test_diff.py`.

The algorithm mirrors `CmpRuns.compare_results` (verified in `CmpRuns.py:374`):
group by location; within a shared location, tier-1 match on `issue_id` → common;
of the leftovers, tier-2 match on `(checker, category, description)` → changed;
remaining old → removed, remaining new → added; locations unique to one side are
wholly removed/added.

- [ ] **Step 1: failing test** — `test_diff.py`:

```python
from __future__ import annotations

import unittest

from aqb.diff import ReportDelta, compare_findings
from aqb.normalize import Finding


def _f(issue_id, file="a.c", line=1, col=1, checker="core.X", cat="Logic", desc="d", pl=1):
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
        base = [_f("id1")]
        new = [_f("id1")]
        d = compare_findings(base, new)
        self.assertEqual(len(d.common), 1)
        self.assertEqual(d.added, [])
        self.assertEqual(d.removed, [])

    def test_added_and_removed(self):
        d = compare_findings([_f("old")], [_f("new")])
        # Different identity AND different location fields default to same
        # location (a.c:1:1) but different KEY_FIELDS? They share checker/cat/
        # desc, so at the same location they are "changed", not add/remove.
        # Force distinct locations to get add/remove:
        d = compare_findings([_f("old", line=1)], [_f("new", line=2)])
        self.assertEqual(len(d.removed), 1)
        self.assertEqual(len(d.added), 1)
        self.assertEqual(d.changed, [])

    def test_same_location_similar_is_changed(self):
        # Same location, different issue identity, same checker/category/desc
        # => a "changed" pair (tier-2 fuzzy match).
        base = [_f("id-a", line=5)]
        new = [_f("id-b", line=5)]
        d = compare_findings(base, new)
        self.assertEqual(len(d.changed), 1)
        self.assertEqual(d.added, [])
        self.assertEqual(d.removed, [])

    def test_same_location_dissimilar_is_add_remove(self):
        base = [_f("id-a", line=5, checker="core.A", desc="da")]
        new = [_f("id-b", line=5, checker="core.B", desc="db")]
        d = compare_findings(base, new)
        self.assertEqual(len(d.added), 1)
        self.assertEqual(len(d.removed), 1)
        self.assertEqual(d.changed, [])
```

- [ ] **Step 2: run → fail** (`No module named 'aqb.diff'`).

- [ ] **Step 3: implement** — `diff.py`:

```python
from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List, Tuple

from aqb.normalize import Finding

# Tier-2 similarity fields (mirrors CmpRuns.AnalysisDiagnostic.KEY_FIELDS).
_SIMILAR_FIELDS = ("checker", "category", "description")


@dataclass
class ReportDelta:
    common: List[Finding] = field(default_factory=list)
    added: List[Finding] = field(default_factory=list)
    removed: List[Finding] = field(default_factory=list)
    changed: List[Tuple[Finding, Finding]] = field(default_factory=list)


def _location(f: Finding) -> Tuple[str, int, int]:
    return (f.file, f.line, f.column)


def _similar(a: Finding, b: Finding) -> bool:
    return all(getattr(a, k) == getattr(b, k) for k in _SIMILAR_FIELDS)


def _group(findings: List[Finding]) -> Dict[Tuple[str, int, int], List[Finding]]:
    grouped: Dict[Tuple[str, int, int], List[Finding]] = {}
    for f in findings:
        grouped.setdefault(_location(f), []).append(f)
    return grouped


def compare_findings(base: List[Finding], new: List[Finding]) -> ReportDelta:
    """Two-tier classification of two normalized finding sets, mirroring
    CmpRuns.compare_results: bucket by location; tier-1 match on issue identity
    (common); tier-2 match on checker/category/description (changed); leftovers
    are removed (base-only) / added (new-only).
    """
    delta = ReportDelta()
    gb = _group(base)
    gn = _group(new)

    for loc in set(gb) & set(gn):
        old = list(gb[loc])
        cur = list(gn[loc])

        # Tier 1: exact issue identity.
        matched_new_ids = set()
        remaining_old = []
        for a in old:
            hit = next(
                (b for b in cur if b.issue_id == a.issue_id and id(b) not in matched_new_ids),
                None,
            )
            if hit is not None:
                delta.common.append(hit)
                matched_new_ids.add(id(hit))
            else:
                remaining_old.append(a)
        cur = [b for b in cur if id(b) not in matched_new_ids]

        # Tier 2: fuzzy similarity (checker/category/description).
        used_new = set()
        still_old = []
        for a in remaining_old:
            hit = next(
                (b for b in cur if id(b) not in used_new and _similar(a, b)), None
            )
            if hit is not None:
                delta.changed.append((a, hit))
                used_new.add(id(hit))
            else:
                still_old.append(a)
        cur = [b for b in cur if id(b) not in used_new]

        delta.removed.extend(still_old)
        delta.added.extend(cur)

    for loc in set(gb) - set(gn):
        delta.removed.extend(gb[loc])
    for loc in set(gn) - set(gb):
        delta.added.extend(gn[loc])

    return delta
```

- [ ] **Step 4: run → pass. Step 5: black + commit** (`feat(aqb): two-tier compare_findings for the diff engine`).

## Task 2: `diff_runs` — per-project + summary

**Files:** Modify `aqb/diff.py`; Modify `test_diff.py`.

- [ ] **Step 1: failing test** — given two `{project: [Finding]}` dicts, `diff_runs`
  returns `{project: ReportDelta}` covering the union of projects, and
  `summarize(deltas)` returns totals:

```python
def test_diff_runs_union_of_projects_and_summary(self):
    from aqb.diff import diff_runs, summarize

    base = {"p": [_f("id1", line=1)], "gone": [_f("g", line=9)]}
    new = {"p": [_f("id1", line=1), _f("id2", line=2)], "fresh": [_f("z", line=3)]}
    deltas = diff_runs(base, new)
    self.assertEqual(set(deltas), {"p", "gone", "fresh"})
    self.assertEqual(len(deltas["p"].common), 1)
    self.assertEqual(len(deltas["p"].added), 1)      # id2 new
    self.assertEqual(len(deltas["gone"].removed), 1)  # project only in base
    self.assertEqual(len(deltas["fresh"].added), 1)   # project only in new

    s = summarize(deltas)
    self.assertEqual(s["added"], 2)     # id2 + fresh/z
    self.assertEqual(s["removed"], 1)   # gone/g
    self.assertEqual(s["common"], 1)
    self.assertEqual(s["changed"], 0)
```

- [ ] **Step 2: fail. Step 3: implement**:

```python
def diff_runs(
    base: Dict[str, List[Finding]], new: Dict[str, List[Finding]]
) -> Dict[str, ReportDelta]:
    """Compare two runs' per-project findings. Projects present in only one run
    yield an all-removed (base-only) or all-added (new-only) delta."""
    deltas: Dict[str, ReportDelta] = {}
    for project in sorted(set(base) | set(new)):
        deltas[project] = compare_findings(base.get(project, []), new.get(project, []))
    return deltas


def summarize(deltas: Dict[str, ReportDelta]) -> Dict[str, int]:
    """Flat totals across all projects."""
    out = {"common": 0, "added": 0, "removed": 0, "changed": 0}
    for d in deltas.values():
        out["common"] += len(d.common)
        out["added"] += len(d.added)
        out["removed"] += len(d.removed)
        out["changed"] += len(d.changed)
    return out
```

- [ ] **Step 4: pass. Step 5: black + commit.**

## Task 3: `verdict` — the `--expect` policy

**Files:** Modify `aqb/diff.py`; Modify `test_diff.py`.

- [ ] **Step 1: failing test**:

```python
def test_verdict_policies(self):
    from aqb.diff import verdict

    clean = {"common": 5, "added": 0, "removed": 0, "changed": 0}
    drift = {"common": 5, "added": 2, "removed": 1, "changed": 0}

    self.assertTrue(verdict(clean, "same-reports")[0])
    self.assertFalse(verdict(drift, "same-reports")[0])
    # same-count: equal totals pass even if identities changed.
    self.assertTrue(verdict({"common": 4, "added": 1, "removed": 1, "changed": 0}, "same-count")[0])
    self.assertFalse(verdict(drift, "same-count")[0])  # 7 vs 6
    # no-crashes always passes in v1 (no crash tracking yet).
    self.assertTrue(verdict(drift, "no-crashes")[0])
```

- [ ] **Step 2: fail. Step 3: implement**:

```python
def verdict(summary: Dict[str, int], expect: str) -> Tuple[bool, str]:
    """Apply an --expect policy to a diff summary. Returns (passed, reason)."""
    base_count = summary["common"] + summary["removed"] + summary["changed"]
    new_count = summary["common"] + summary["added"] + summary["changed"]
    if expect == "no-crashes":
        # v1: crashes are not yet tracked in findings; a completed run is
        # treated as crash-free, so this passes. Tightened when crash artifacts
        # are collected.
        return True, "no crashes recorded"
    if expect == "same-count":
        ok = base_count == new_count
        return ok, f"base {base_count} vs new {new_count}"
    if expect == "same-reports":
        drift = summary["added"] + summary["removed"] + summary["changed"]
        return drift == 0, f"{drift} report(s) differ"
    raise ValueError(f"unknown --expect policy: {expect!r}")
```

- [ ] **Step 4: pass. Step 5: black + commit.**

## Task 4: `aqb diff` CLI verb

**Files:** Modify `aqb/cli.py`; Modify `aqb/tests/test_cli.py`.

Loads `reports/findings.json` for `--base` and `--new` (resolved via
`RunStore.resolve` for id prefixes), diffs, prints a summary, applies `--expect`.
`Finding` is reconstructed from each stored dict.

- [ ] **Step 1: failing CLI test** — create two runs in a temp store (write
  `findings.json` under each), then:

```python
class DiffCliTest(unittest.TestCase):
    def _make_run(self, store, run_id, findings_by_project):
        # create_run makes the dirs + metadata; then write findings.json.
        from aqb.metadata import (
            AnalyzerProvenance, ContainerProvenance, ExecutionProvenance, Metadata,
        )
        store.create_run(
            Metadata(
                run_id=run_id, kind="functional", created="2026-07-17T00:00:00+00:00",
                analyzer=AnalyzerProvenance(commit="c"),
                container=ContainerProvenance(), execution=ExecutionProvenance(),
            )
        )
        import json, os
        path = os.path.join(store.runs_dir, run_id, "reports", "findings.json")
        with open(path, "w") as f:
            json.dump(findings_by_project, f)

    def test_diff_reports_added_and_gates(self):
        import tempfile
        from aqb.store import RunStore

        row = dict(issue_id="i1", file="a.c", line=1, column=1,
                   checker="core.X", category="Logic", description="d", path_length=1)
        row2 = dict(row, issue_id="i2", line=2)
        with tempfile.TemporaryDirectory() as root:
            store = RunStore(root)
            self._make_run(store, "r-base", {"p": [row]})
            self._make_run(store, "r-new", {"p": [row, row2]})
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                code = main(["--home", root, "diff", "--base", "r-base", "--new", "r-new"])
            self.assertEqual(code, 0)                      # default no-crashes passes
            self.assertIn("added", out.getvalue().lower())

    def test_diff_expect_same_reports_fails_on_drift(self):
        import tempfile
        from aqb.store import RunStore

        row = dict(issue_id="i1", file="a.c", line=1, column=1,
                   checker="core.X", category="Logic", description="d", path_length=1)
        row2 = dict(row, issue_id="i2", line=2)
        with tempfile.TemporaryDirectory() as root:
            store = RunStore(root)
            self._make_run(store, "r-base", {"p": [row]})
            self._make_run(store, "r-new", {"p": [row, row2]})
            with contextlib.redirect_stdout(io.StringIO()):
                code = main(["--home", root, "diff", "--base", "r-base",
                             "--new", "r-new", "--expect", "same-reports"])
            self.assertEqual(code, 1)
```

- [ ] **Step 2: fail. Step 3: implement** in `cli.py`: add `load_findings_json`
  helper (read `<run>/reports/findings.json` → `{project: [Finding(**row)]}`),
  a `diff` subparser (`--base` required, `--new` required, `--expect` choices
  `["no-crashes","same-count","same-reports"]` default `no-crashes`,
  `--format ["text","json"]` default `text`), and `cmd_diff` that resolves both
  run ids, loads findings, calls `diff_runs`/`summarize`/`verdict`, prints, and
  returns `0`/`1`. Remove `diff` from `STUB_COMMANDS`.

- [ ] **Step 4: pass. Step 5: black + commit.**

## Self-Review

- **Spec coverage:** two-tier identity (Task 1, ported faithfully from
  `CmpRuns.compare_results`), run-pair + summary (Task 2), `--expect` verdict
  (Task 3), CLI wired to the store over self-contained `findings.json` (Task 4).
  Deferred items (`--baseline`, metric deltas, crash-aware `no-crashes`) are
  called out, not silently dropped.
- **Type consistency:** `Finding` fields used in `_location`/`_similar` match
  `normalize.Finding`; `summarize` keys match `verdict`'s reads.
- **Reuse:** report *identity* stays SATest's (`issue_id` via
  `normalize.load_findings`); only the small classification algorithm is ported
  so diff runs on stored artifacts.
