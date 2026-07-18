# AQB Phase 5 — benchmark runs + `plot` (Design & Decomposition)

> **For agentic workers:** REQUIRED SUB-SKILL: superpowers:subagent-driven-development to execute. Steps use checkbox (`- [ ]`) syntax.

**Goal:** `aqb run --bench -n N` produces a **benchmark run** (N analyze iterations; per-entry-point metric samples stored; reports ignored), and `aqb plot <run> [<run> …]` renders **one self-contained HTML** with candlestick diagrams for **every metric at all three granularities** (per-run, per-TU, per-entry-point), overlaying the given runs. No plot flags beyond the run list + `-o`.

**STATUS: code-complete (2026-07-18).** All four tasks implemented + committed on `bb/aqb-design` (122 tests green). Plot pipeline validated locally at real scale: 449 entry points × 3 iterations × 2 runs → a 10 MB self-contained HTML with 102 charts (≈34 metrics × 3 granularities), overlaying both runs, collapsible + TOC. Remaining: a real multi-iteration benchmark run on the daemon (`aqb run --bench -n N` on the container runtime).

**Architecture:** A benchmark run stores raw per-iteration, per-entry-point samples (`metrics/samples.json`). `plot` loads one or more runs, aggregates *up* from the entry-point samples to all three levels (entry-point → group by file = per-TU → sum over all = per-run) for every metric, computes candlestick five-numbers with stdlib `statistics.quantiles`, and renders inline SVG into a single dependency-free HTML (no matplotlib/pandas/seaborn — none are available; design mandates a self-contained backend).

**Determinism note:** the analyzer's *count* metrics (`NumSteps`, `NumBlocks`, …) are deterministic across identical iterations, so their within-run candlestick collapses to a flat line (still meaningful when overlaying *different* runs/configs). The *timing* columns (`PathRunningTime`, `SyntaxRunningTime`, `TimeSpentSolvingZ3Queries`, …) are the ones that spread across N iterations. We render all of them either way.

**Tech Stack:** stdlib only (`statistics`, `csv`, `json`, `html`); `unittest`.

---

## Data captured & stored

Source per iteration: the merged per-entry-point CSV (`analyze.merge_entry_point_csvs` → `metrics.parse_entry_point_csv` → `dedup_entry_points`, one row per USR). A benchmark run's `metrics/samples.json`:

```json
{
  "iterations": 10,
  "metrics": ["CFGSize", "PathRunningTime", "NumSteps", ...],   // union of stat columns
  "samples": [
    [ {"usr": "...", "file": "programs/fileio.c", "debug_name": "...",
       "stats": {"NumSteps": 342, "PathRunningTime": 1, ...}}, ... ],   // iteration 0
    ...                                                                   // iteration 1..N-1
  ]
}
```

`reports/` is omitted for a benchmark run (`kind == "benchmark"`).

## Aggregation (at plot time, from the stored samples)

For a run, for each metric `m`:

- **per-entry-point:** for each USR, samples = `[stats[m] for that USR in each iteration]` (missing → skip that iteration for that USR/metric).
- **per-TU:** for each `file`, per iteration sum `m` over the entry points in that file → one value/iteration → samples across iterations.
- **per-run:** per iteration sum `m` over *all* entry points → one value/iteration → samples across iterations.

`candlestick(samples) -> (min, q1, median, q3, max, n)` via `statistics.quantiles(data, n=4)` (n>=2; for n==1 all five collapse to the single value).

## Plot output (one HTML)

A single self-contained file: a table of contents, then three top-level sections (Per-run, Per-TU, Per-entry-point). Within each, one collapsible `<details>` per metric containing an inline-SVG chart whose x-axis lists the entities (the single run-bucket / the files / the USRs) and, at each entity, one candlestick per input run (color-coded, with a legend). Charts are horizontally scrollable (CSS `overflow-x`) so wide per-entry-point charts stay usable.

## Decomposition

| Task | Deliverable | Test |
|---|---|---|
| **1 — candlestick + aggregate** | `aqb/benchmark.py`: `candlestick()`, `aggregate_samples()` (3 levels × all metrics). | pure unit tests |
| **2 — benchmark capture** | `perform_run(..., iterations=N, kind)` loops the analyze container N times into per-iteration EP dirs, stores `samples.json`, skips reports for benchmark; `run --bench -n N` CLI. | fake-runtime unit tests |
| **3 — SVG render** | `aqb/plot.py`: `svg_chart(entities, series_by_run)` (one chart) + `render_html(runs)` (full bundle, all metrics × 3 levels). | pure string/DOM-substring tests |
| **4 — plot CLI** | `aqb plot <run>… [-o out.html]`; loads samples, renders, writes HTML. Drop `plot` from stubs. | fake-store CLI tests |

---

## Task 1: `candlestick` + `aggregate_samples`

**Files:** Create `clang/utils/analyzer/aqb/benchmark.py`; Test `clang/utils/analyzer/aqb/tests/test_benchmark.py`.

- [ ] **Step 1: failing test** — `test_benchmark.py`:

```python
from __future__ import annotations

import unittest

from aqb.benchmark import Candle, aggregate_samples, candlestick
from aqb.metrics import EntryPointMetrics


class CandlestickTest(unittest.TestCase):
    def test_five_number_summary(self):
        c = candlestick([1, 2, 3, 4, 5])
        self.assertEqual(c.min, 1)
        self.assertEqual(c.max, 5)
        self.assertEqual(c.median, 3)
        self.assertEqual(c.n, 5)
        self.assertLessEqual(c.q1, c.median)
        self.assertLessEqual(c.median, c.q3)

    def test_single_sample_collapses(self):
        c = candlestick([7])
        self.assertEqual((c.min, c.q1, c.median, c.q3, c.max), (7, 7, 7, 7, 7))
        self.assertEqual(c.n, 1)

    def test_empty_is_none(self):
        self.assertIsNone(candlestick([]))


class AggregateTest(unittest.TestCase):
    def _ep(self, usr, file, **stats):
        return EntryPointMetrics(usr=usr, file=file, debug_name=usr, stats=stats)

    def test_three_levels(self):
        # 2 iterations, 2 entry points across 2 files.
        it0 = [self._ep("a", "f1.c", NumSteps=10), self._ep("b", "f2.c", NumSteps=5)]
        it1 = [self._ep("a", "f1.c", NumSteps=20), self._ep("b", "f2.c", NumSteps=5)]
        agg = aggregate_samples([it0, it1])

        # per-run: sum over all EPs per iteration -> [15, 25]
        self.assertEqual(agg["run"]["(all)"]["NumSteps"], [15, 25])
        # per-TU: file f1.c -> [10, 20]
        self.assertEqual(agg["tu"]["f1.c"]["NumSteps"], [10, 20])
        # per-entry-point: USR a -> [10, 20]
        self.assertEqual(agg["entry-point"]["a"]["NumSteps"], [10, 20])
```

- [ ] **Step 2: run → fail.** **Step 3: implement** — `benchmark.py`:

```python
from __future__ import annotations

import statistics
from dataclasses import dataclass
from typing import Dict, List, Optional

from aqb.metrics import EntryPointMetrics

RUN_ENTITY = "(all)"


@dataclass
class Candle:
    min: float
    q1: float
    median: float
    q3: float
    max: float
    n: int


def candlestick(samples: List[float]) -> Optional[Candle]:
    """Five-number summary of a sample list; None if empty. A single sample
    collapses all five numbers to that value (statistics.quantiles needs n>=2)."""
    if not samples:
        return None
    data = sorted(samples)
    lo, hi, med = data[0], data[-1], statistics.median(data)
    if len(data) == 1:
        return Candle(lo, lo, lo, lo, lo, 1)
    q1, _, q3 = statistics.quantiles(data, n=4, method="inclusive")
    return Candle(lo, q1, med, q3, hi, len(data))


# level -> entity -> metric -> [one value per iteration]
Aggregated = Dict[str, Dict[str, Dict[str, List[float]]]]


def aggregate_samples(iterations: List[List[EntryPointMetrics]]) -> Aggregated:
    """Aggregate per-iteration entry-point metrics up to per-run, per-TU (by
    file), and per-entry-point (by USR). Each leaf is one value per iteration
    (summed over the relevant entry points for run/TU; the USR's value for
    entry-point). Metrics absent in an iteration simply contribute no sample
    for that iteration."""
    agg: Aggregated = {"run": {RUN_ENTITY: {}}, "tu": {}, "entry-point": {}}

    for ep_list in iterations:
        run_totals: Dict[str, float] = {}
        tu_totals: Dict[str, Dict[str, float]] = {}
        for ep in ep_list:
            for metric, value in ep.stats.items():
                run_totals[metric] = run_totals.get(metric, 0) + value
                tu_totals.setdefault(ep.file, {})
                tu_totals[ep.file][metric] = tu_totals[ep.file].get(metric, 0) + value
                epd = agg["entry-point"].setdefault(ep.usr, {})
                epd.setdefault(metric, []).append(value)
        for metric, total in run_totals.items():
            agg["run"][RUN_ENTITY].setdefault(metric, []).append(total)
        for file, metrics in tu_totals.items():
            fd = agg["tu"].setdefault(file, {})
            for metric, total in metrics.items():
                fd.setdefault(metric, []).append(total)
    return agg
```

- [ ] **Step 4: pass. Step 5: black + commit** (`feat(aqb): candlestick + 3-level metric aggregation`).

## Task 2: benchmark capture in `perform_run` + `run --bench`

**Files:** Modify `aqb/analyze.py` (parameterize the EP dir), `aqb/run.py`, `aqb/cli.py`; Tests `test_run.py`, `test_analyze.py`, `test_cli.py`.

- [ ] **analyze.py:** give `analyze_run_argv` an `ep_csv_dir: str = f"{PROJECTS_MOUNT}/{EP_CSV_DIR_NAME}"` param and emit `-e AQB_EP_CSV_DIR={ep_csv_dir}` from it (default unchanged). Add a test that a custom `ep_csv_dir` shows up in the argv.

- [ ] **run.py:** `perform_run` gains `iterations: int = 1` and uses `kind` already present. When `iterations == 1 and kind == "functional"`: unchanged (reports + single merged CSV). When benchmarking (`iterations > 1` or `kind == "benchmark"`): materialize once, then for `i in range(iterations)` run the analyze container with `ep_csv_dir=f"{PROJECTS_MOUNT}/{EP_CSV_DIR_NAME}/iter-{i}"`; after each, parse+dedup that iteration's merged CSV (`collect_entry_point_csvs` on the host iter dir → `merge_entry_point_csvs` → write to a temp text → `metrics.parse_entry_point_csv` → `dedup_entry_points`) into a per-iteration list; write `metrics/samples.json` (`iterations`, `metrics` = sorted union of stat keys, `samples`). Skip `reports/` for a benchmark run. Metadata `kind="benchmark"`, `execution.n = iterations`.

  Test (`test_run.py`): a fake runtime that writes a distinct per-iteration CSV into the `iter-<i>` dir it sees in the argv; assert `samples.json` has `iterations == N`, N sample-lists, and no `reports/findings.json`.

- [ ] **cli.py `run`:** add `--bench` (store_true) and `-n/--iterations` (int, default 1). When `--bench`, pass `kind="benchmark"` and `iterations=args.iterations` (require `-n >= 2` when `--bench`, else argparse error). Test that `--bench -n 3` calls `perform_run` with `kind="benchmark", iterations=3`.

- [ ] black + commit per file-group.

## Task 3: SVG candlestick rendering

**Files:** Create `aqb/plot.py`; Test `test_plot.py`.

- [ ] **Step 1: failing test** — `test_plot.py`:

```python
from __future__ import annotations

import unittest

from aqb.benchmark import Candle
from aqb.plot import render_html, svg_chart


class SvgChartTest(unittest.TestCase):
    def test_chart_has_one_candle_per_run_per_entity(self):
        # entity "f1.c" with candles from two runs.
        series = {
            "run-A": {"f1.c": Candle(1, 2, 3, 4, 5, 4)},
            "run-B": {"f1.c": Candle(2, 3, 4, 5, 6, 4)},
        }
        svg = svg_chart(["f1.c"], series, title="NumSteps")
        self.assertIn("<svg", svg)
        self.assertIn("NumSteps", svg)
        # a <rect> for each run's box (>=2) and the run names in a legend
        self.assertGreaterEqual(svg.count("<rect"), 2)
        self.assertIn("run-A", svg)
        self.assertIn("run-B", svg)


class RenderHtmlTest(unittest.TestCase):
    def test_bundles_all_levels_and_metrics(self):
        runs = {
            "run-A": {
                "run": {"(all)": {"NumSteps": [10, 12], "PathRunningTime": [1, 2]}},
                "tu": {"f1.c": {"NumSteps": [10, 12]}},
                "entry-point": {"usrA": {"NumSteps": [10, 12]}},
            }
        }
        html = render_html(runs)
        self.assertIn("<html", html)
        # every metric and every level is represented
        self.assertIn("NumSteps", html)
        self.assertIn("PathRunningTime", html)
        self.assertIn("Per-run", html)
        self.assertIn("Per-TU", html)
        self.assertIn("Per-entry-point", html)
        # self-contained: no external script/link tags
        self.assertNotIn("<script src", html)
        self.assertNotIn("http://", html)
```

- [ ] **Step 2: fail. Step 3: implement** `plot.py` with:
  - `svg_chart(entities: List[str], series_by_run: Dict[str, Dict[str, Candle]], title: str) -> str`: linear y-scale from 0 to the max `Candle.max` across the chart; for each entity, lay out one candle per run (offset within the entity's x-slot, colored per run); each candle = a vertical line `min→max`, a `<rect>` `q1→q3`, a median tick; x labels (`html.escape`d, truncated) and a run-color legend. Deterministic colors from a fixed palette by run order.
  - `render_html(runs: Dict[str, Aggregated]) -> str`: build the union of metrics and, per level (`run`, `tu`, `entry-point`) and per metric, the union of entities across runs; emit a TOC + three sections, one `<details><summary>metric</summary>` per metric wrapping the `svg_chart`. All CSS inline in a `<style>`; charts in `overflow-x:auto` divs. Everything `html.escape`d.

- [ ] **Step 4: pass. Step 5: black + commit.**

## Task 4: `aqb plot` CLI verb

**Files:** Modify `aqb/cli.py`; Test `test_cli.py`.

- [ ] Add a `plot` subparser: positional `runs` (nargs="+"), `-o/--output` (default `aqb-plot.html`). `cmd_plot` resolves each run id, loads `metrics/samples.json`, calls `aggregate_samples` per run (rebuilding `EntryPointMetrics` from the stored dicts), then `render_html({run_id: aggregated})`, writes the file, prints its path. Error clearly if a named run has no `samples.json` (i.e. it is a functional, non-benchmark run). Remove `plot` from `STUB_COMMANDS`.

- [ ] Test (`test_cli.py`): create a store, write a benchmark-style `samples.json` under a run, run `main(["--home", root, "plot", "r-b", "-o", out])`, assert exit 0 and the output HTML exists and contains `<svg`.

- [ ] black + commit.

## Self-Review

- **Spec coverage:** all three granularities + all metrics, one bundled self-contained HTML, no extra flags (Tasks 1/3/4); benchmark runs via `run --bench -n N` storing raw per-iteration samples with reports omitted (Task 2). Overlays multiple runs (`render_html` takes a dict of runs).
- **Type consistency:** `Aggregated` shape (`level -> entity -> metric -> [values]`) is produced by `aggregate_samples` and consumed by `render_html`/`svg_chart`; `Candle` fields match between `candlestick` and `svg_chart`.
- **Reuse:** samples come through the existing `analyze.merge_entry_point_csvs` + `metrics.parse_entry_point_csv`/`dedup_entry_points`; no new parsing. No external plotting deps (stdlib `statistics` + hand-written SVG), honoring the self-contained-backend design.
- **Deferred/noted:** wall-clock time/memory per iteration (needs driver-emitted timing) is not in v1; the timing *columns already in the entry-point CSV* cover the distribution story. Large per-entry-point bundles are made navigable (collapsible + scroll), not truncated, per "always generate all".
