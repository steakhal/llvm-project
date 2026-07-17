# AQB Phase 3c — Analyze Pipeline + `aqb run` (Design & Decomposition)

> **For agentic workers:** REQUIRED SUB-SKILL: superpowers:subagent-driven-development to execute. But note the **Open Questions** section — sub-phase 3c-1 must resolve the SATest reuse-seam unknowns (with a ground-truth pass + a daemon-gated prototype) before 3c-2's tasks can be fully precise.

**Goal:** `aqb run --commit <sha> [--source …] [--projects …]` produces a **functional run**: resolve/build the Clang Volume (Phase 3b, done), analyze the pinned corpus with that clang, and persist reports + metrics + provenance as a Run in the store.

**Status:** Phases 1, 2, 3a, 3b are complete and committed on `bb/aqb-design`; `aqb build-clang` builds a real Clang Volume end-to-end on the container runtime. This phase consumes that volume.

---

## Design: reuse SATest's analyze machinery

Ground truth (from `SATestBuild.py`): SATest analyzes a project by cloning it (pinned), running its `run_static_analyzer.cmd` build with each command prefixed by `scan-build --use-analyzer '<clang>' -plist-html -o <out> -enable-checker <checkers> --keep-empty -analyzer-config '<generate_config()>'`, producing `<project>/ScanBuildResults/*/*.plist` + `Logs/`. `generate_config()` = `serialize-stats=true,stable-report-filename=true` + `extra_analyzer_config`. In the SATest container, clang is at `/analyzer/bin` (`PATH=/analyzer/bin`), driven by `entrypoint.py`→`SATest.py build` from `/projects`.

**AQB reuse strategy:** run SATest's analyze inside a container that mounts the **Clang Volume at `/analyzer`** — so `/analyzer/bin/clang` satisfies SATest's existing contract with zero SATest changes — with the corpus recipes at `/projects`, the shared ccache at `/ccache`, and the clang volume `:ro`. AQB injects `dump-entry-point-stats-to-csv=<path>` as SATest's `extra_analyzer_config` (on top of the already-present `serialize-stats=true`). After the run, AQB collects the per-project `ScanBuildResults/*.plist` + the entry-point CSV(s) and feeds them through the **Phase-3a data model**: `normalize.load_findings` (reports) and `metrics.parse_*` (TU + entry-point stats), then populates `Metadata` and persists via `RunStore`.

**Data flow:**
```
aqb run --commit X --source Y --projects [..]
  → resolve_or_build_clang (3b)        → Clang Volume (clang at /opt/aqb/clang -> mount :ro at /analyzer)
  → corpus.select_projects (3a)        → pinned ProjectInfo list
  → analyze container (SATest.py build) → per-project ScanBuildResults/*.plist + entry-point CSVs
  → normalize.load_findings (3a)       → deduped Findings
  → metrics.parse_* (3a)               → per-TU/entry-point metrics
  → Metadata (AnalyzerProvenance from 3b's ClangVolume; ProjectProvenance from corpus) 
  → RunStore.create_run                → runs/<id>/{metadata.json,reports/,metrics/,logs/}
```

## Decomposition

| Sub-phase | Deliverable | Testability |
|---|---|---|
| **3c-1 — Analyze primitive + reuse-seam** | `aqb/analyze.py`: `analyzer_config()`, the analyze-container `run` argv (mount volume `:ro` at `/analyzer`, projects, ccache; invoke SATest analyze), and output-collection helpers. **Resolve the open questions** (below) via a ground-truth pass + daemon-gated prototype. | argv/config/collection pure-unit-testable; the SATest invocation validated by a daemon-gated smoke |
| **3c-2 — `aqb run` orchestration** | `aqb/run.py` + the `run` CLI verb: resolve clang → select corpus → analyze → collect → normalize + metrics → populate `Metadata` → `RunStore.create_run`. Provenance wiring (3b `ClangVolume`→`AnalyzerProvenance`; `ProjectInfo`→`ProjectProvenance`). | fake-runtime + fixture-output unit tests |
| **3c-3 — End-to-end + docs** | daemon-gated end-to-end smoke (build clang → run over 1 tiny project → inspect the persisted run); update `AQB-design.rst`. | manual/daemon-gated |

## Open questions 3c-1 MUST resolve (ground-truth + prototype, not guesswork)

These are the analyze-seam unknowns; a first sub-phase ground-truth dive + a real daemon run (the user has a container runtime) settles them, exactly as the builder image was validated by real runs:

### RESOLVED (validated on the container runtime over zstd, 2026-07-17)

All five are now settled by a real run; results captured here so 3c-2 is precise.

1. **Analyze-only invocation — RESOLVED (driver, not `build`).** `SATest.py build -r` was supposed to give analyze-only (reference) behavior, but a real run showed it still wrote `ScanBuildResults` and ran SATest's reference-*compare* (the "Mismatch in number of results folders" error). `-r` parses to `regenerate=True` on the host, yet the container run compared anyway — the `build` path is unreliable for AQB's needs. **Fix:** AQB ships `aqb/analyze_driver.py`, which reuses the lower seam `SATestAdd` uses — `ProjectTester(TestInfo(is_reference_build=True)).test()`. That *structurally* writes `RefScanBuildResults` and skips `run_cmp_results`. `--extra-analyzer-config` is a driver flag threaded into `TestInfo.extra_analyzer_config`.
2. **`scan-build` in the volume — RESOLVED (yes).** `build.sh` now runs `install-scan-build install-scan-build-py`; the driver run analyzed 70 TUs, so `scan-build`/`clang` resolve via `PATH=/analyzer/bin`.
3. **Analyze container entry — RESOLVED.** `--entrypoint python3 … /scripts/aqb/analyze_driver.py`, analyzer dir mounted `:ro` at `/scripts` (so the driver's sibling imports `SATestBuild`/`ProjectMap` resolve), volume `:ro` at `/analyzer`, corpus at `/projects`, `-w /projects`.
4. **Output collection paths — RESOLVED.** Results land at `/projects/<name>/RefScanBuildResults/*/*.plist` (70 plists for zstd; html/css/js stripped by `cleanup_reference_results`, `.plist` kept). `collect_plists` already globs `RefScanBuildResults`. The entry-point CSV lands at the `dump-entry-point-stats-to-csv=<path>` path — **but see the clobbering finding below.**
5. **runtime quirks — RESOLVED.** `-m 24G --cpus 8` (shim maps `--cpus`→`-c`), `:ro` volume, host corpus bind-mounted at `/projects` all worked.

### NEW finding for 3c-2: entry-point CSV is clobbered per-TU

The zstd run produced 70 plists across many source files, but `zstd-ep.csv` held only **one** TU's rows (`programs/fileio.c`, 55 entry points). Root cause (confirmed in source): `EntryPointStat::dumpStatsAsCSV` opens the path with `llvm::sys::fs::OF_Text` (**truncate**, not append) and is called once per TU from `AnalysisConsumer` (`AnalysisConsumer.cpp:673`). scan-build runs one clang process per TU, all pointed at the same fixed `dump-entry-point-stats-to-csv` path, so the last TU's process truncates+overwrites — last writer wins. **3c-2 must resolve how AQB gets per-TU stats without clobbering** (see plan §"CSV aggregation decision").

### Original questions (for history)

1. **`SATest.py build` invocation for analyze-only.** Which subcommand/flags produce plists without SATest's own reference-compare/verdict? (`build` runs `RegressionTester.test_all` = build + compare.) Is there a flag, or should AQB drive `ProjectTester(is_reference_build=True)` (as `SATestAdd` does) to just materialize+analyze? How is `extra_analyzer_config` passed on the CLI (a `--extra-analyzer-config` flag on `SATest.py build`, or only via the Python API)? **Read `SATest.py`'s argparse + `RegressionTester`/`ProjectTester` constructors.**
2. **Does the Clang Volume contain `scan-build`?** `build.sh` runs `ninja install-clang install-clang-resource-headers` — that installs the clang binary + headers but likely **not** `scan-build` (a separate install target/tool). Options: (a) add `install-scan-build` (or the right target) to `build.sh`'s ninja install so the volume is self-contained; (b) use the analyze **image**'s `scan-build` (from its apt `clang` package, if present) with `--use-analyzer /analyzer/bin/clang`. Decide + verify `scan-build` is actually present on whichever path.
3. **Analyze container entry.** The AQB builder image's ENTRYPOINT is `aqb-build-clang`. For analyze, override it (e.g. `--entrypoint python3 … /scripts/SATest.py build …`) and mount `SATest.py`/scripts at `/scripts` and the corpus at `/projects`. Confirm the mount+entry that makes `SATest.py build` run against the mounted `/analyzer` clang.
4. **Output collection paths.** `SATest.py build` writes `/projects/<name>/ScanBuildResults/*/*.plist` + `Logs/`. Confirm AQB can read those back (mount `/projects` from a host dir AQB owns) and where the entry-point CSV lands (AQB sets its path via `dump-entry-point-stats-to-csv`).
5. **runtime quirks (like 3b).** The analyze `run` will need the same treatment: `-m`/`--cpus` (analysis is also heavy), the shim's `--cpus`→`-c`, `:ro` mount of the volume, and possibly bind-mounting the corpus/output dirs (a container can't see the host FS — same lesson as the source mount). Expect a few real-run iterations.


## Sub-phase 3c-1 — concrete tasks (the parts that are precise now)

> `analyzer_config()` and the output-collection helpers are pure and specifiable now; the analyze-container `run` argv is drafted here but **confirmed against a real run** (its exact SATest invocation depends on Open Question #1/#3).

### Task 1: `analyzer_config()` — the stats-config AQB injects

**Files:** Create `clang/utils/analyzer/aqb/analyze.py`; Test `clang/utils/analyzer/aqb/tests/test_analyze.py`.

- [ ] **Step 1: failing test** — `test_analyze.py`:

```python
from __future__ import annotations

import unittest

from aqb.analyze import EP_CSV_NAME, analyzer_config


class AnalyzerConfigTest(unittest.TestCase):
    def test_includes_entry_point_csv(self):
        cfg = analyzer_config("/out/ep.csv")
        self.assertIn("dump-entry-point-stats-to-csv=/out/ep.csv", cfg)

    def test_appends_extra(self):
        cfg = analyzer_config("/out/ep.csv", extra="max-nodes=0")
        parts = cfg.split(",")
        self.assertIn("dump-entry-point-stats-to-csv=/out/ep.csv", parts)
        self.assertIn("max-nodes=0", parts)

    def test_ep_csv_name_is_a_plain_filename(self):
        self.assertNotIn("/", EP_CSV_NAME)
```

- [ ] **Step 2: run → fail** (`No module named 'aqb.analyze'`).

- [ ] **Step 3: implement** — `analyze.py`:

```python
from __future__ import annotations

from typing import List

# Mount contract for the analyze container (clang volume mounts here so
# SATest's PATH=/analyzer/bin + scan-build --use-analyzer /analyzer/bin/clang
# work unchanged).
ANALYZER_MOUNT = "/analyzer"
PROJECTS_MOUNT = "/projects"
CCACHE_MOUNT = "/ccache"
EP_CSV_NAME = "entry-point-stats.csv"


def analyzer_config(ep_csv_path: str, extra: str = "") -> str:
    """The extra ``-analyzer-config`` string AQB adds on top of SATest's
    defaults (which already include ``serialize-stats=true``): enable the
    per-entry-point CSV dump, plus any caller-supplied options.
    """
    parts: List[str] = [f"dump-entry-point-stats-to-csv={ep_csv_path}"]
    if extra:
        parts.append(extra)
    return ",".join(parts)
```

- [ ] **Step 4: run → pass.** **Step 5: black + commit** (`feat(aqb): analyzer-config for the analyze step`).

### Task 2: output-collection helpers

`collect_plist_dirs(projects_root)` → the per-project `ScanBuildResults` dirs; `collect_entry_point_csvs(output_root)` → the CSV files. Pure `os.walk`/`glob`, unit-tested with a synthetic dir tree. (Full code authored during execution once Open Question #4's exact paths are confirmed.)

### Task 3 (spike, daemon-gated): analyze-container `run` argv + real prototype

Draft `analyze_run_argv(clang_volume, projects_dir, ccache_volume, image, ep_csv_path, memory, cpus)` mounting `clang_volume:/analyzer:ro`, `projects_dir:/projects`, `ccache_volume:/ccache`, with `-m/--cpus`, overriding the entrypoint to run SATest's analyze for the selected projects. **Validate against a real container run over one tiny project** (resolves Open Questions #1–#5); refine argv/env to match. Unit-test the final argv shape with a fake runtime.

## Sub-phase 3c-2 — per-TU stats wrapper + `aqb run` orchestration

> 3c-1 is validated on the container runtime (70 plists in `RefScanBuildResults`, no compare). 3c-2 fixes the per-TU CSV clobber (chosen approach: **AQB-side wrapper, no clang change**) and wires the `run` pipeline. The wrapper's container behavior gets a daemon-gated re-validation like 3c-1; its arg logic + the merge/argv/orchestration are pure-unit-testable now.

### The wrapper mechanism (why it works)

SATest sets the analyzer clang to `$CC` (`SATestBuild.py:122-130`: `CLANG = os.environ["CC"]`) and passes it via `scan-build --use-analyzer '{CLANG}'`, with one shared `-analyzer-config '{generate_config()}'` for every TU. Because `EntryPointStat::dumpStatsAsCSV` truncates (`OF_Text`) and runs once per TU, a single shared `dump-entry-point-stats-to-csv` path is clobbered — last TU wins.

**Fix:** point `CC` at `aqb/clang-analyzer-wrapper.sh`. For each *analysis* invocation the wrapper injects a **PID-unique** `dump-entry-point-stats-to-csv=$AQB_EP_CSV_DIR/$$.csv` and execs the real clang (`$AQB_REAL_CLANG=/analyzer/bin/clang`); non-analysis invocations pass through untouched. AQB then merges all `$AQB_EP_CSV_DIR/*.csv` (identical headers, concat rows) into one table. AQB stops putting `dump-entry-point-stats-to-csv` in the shared analyzer-config so the wrapper is the sole source (no duplicate key).

### Task A1: the analyzer-clang wrapper script

**Files:** Create `clang/utils/analyzer/aqb/clang-analyzer-wrapper.sh` (executable); Test `clang/utils/analyzer/aqb/tests/test_wrapper.py`.

- [ ] **Step 1: failing test** — `test_wrapper.py` drives the script with `AQB_REAL_CLANG=/bin/echo` (echo captures the final argv) and asserts:
  - analysis args (contain `--analyze` or `-analyze`) + `AQB_EP_CSV_DIR` set → output contains `dump-entry-point-stats-to-csv=<dir>/` and a `.csv` suffix;
  - non-analysis args (e.g. `-c foo.c -o foo.o`) → output is the args verbatim, NO `dump-entry-point-stats-to-csv`;
  - analysis args but `AQB_EP_CSV_DIR` unset → passthrough (no injection).

```python
from __future__ import annotations

import os
import subprocess
import tempfile
import unittest

WRAPPER = os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
    "clang-analyzer-wrapper.sh",
)


def _run(args, ep_dir=None):
    env = dict(os.environ, AQB_REAL_CLANG="/bin/echo")
    if ep_dir is not None:
        env["AQB_EP_CSV_DIR"] = ep_dir
    else:
        env.pop("AQB_EP_CSV_DIR", None)
    return subprocess.run(
        [WRAPPER, *args], env=env, capture_output=True, text=True, check=True
    ).stdout


class WrapperTest(unittest.TestCase):
    def test_injects_unique_csv_on_analysis(self):
        with tempfile.TemporaryDirectory() as d:
            out = _run(["--analyze", "foo.c"], ep_dir=d)
            self.assertIn("dump-entry-point-stats-to-csv=", out)
            self.assertIn(d, out)
            self.assertIn(".csv", out)

    def test_passthrough_on_compile(self):
        with tempfile.TemporaryDirectory() as d:
            out = _run(["-c", "foo.c", "-o", "foo.o"], ep_dir=d)
            self.assertNotIn("dump-entry-point-stats-to-csv", out)

    def test_no_injection_without_ep_dir(self):
        out = _run(["--analyze", "foo.c"], ep_dir=None)
        self.assertNotIn("dump-entry-point-stats-to-csv", out)
```

- [ ] **Step 2: run → fail** (script missing).
- [ ] **Step 3: implement** — `clang-analyzer-wrapper.sh`:

```bash
#!/usr/bin/env bash
# AQB analyzer-clang wrapper. Stands in as $CC / scan-build --use-analyzer so
# each per-TU clang *analysis* process writes entry-point stats to a UNIQUE
# (PID-named) CSV under $AQB_EP_CSV_DIR — avoiding the single-shared-path clobber
# where every TU truncates one file (EntryPointStats.cpp uses OF_Text). Non-
# analysis invocations (plain compiles) pass through untouched.
set -euo pipefail

real="${AQB_REAL_CLANG:?AQB_REAL_CLANG is required}"

is_analysis=0
for arg in "$@"; do
    if [ "$arg" = "--analyze" ] || [ "$arg" = "-analyze" ]; then
        is_analysis=1
        break
    fi
done

if [ "$is_analysis" = "1" ] && [ -n "${AQB_EP_CSV_DIR:-}" ]; then
    mkdir -p "$AQB_EP_CSV_DIR"
    exec "$real" "$@" \
        -Xclang -analyzer-config \
        -Xclang "dump-entry-point-stats-to-csv=$AQB_EP_CSV_DIR/$$.csv"
fi

exec "$real" "$@"
```

- [ ] **Step 4: `chmod +x`, run → pass. Step 5: black test + commit** (`feat(aqb): per-TU analyzer wrapper for unique entry-point CSVs`).

> **Daemon-gated re-validation (like 3c-1):** the exact analysis-flag scan-build/ccc-analyzer passes (`--analyze` at driver level vs. `-cc1 -analyze`, and whether `-Xclang -analyzer-config` is accepted in that form) is confirmed by a real container run over zstd before 3c-2 is called done. Refine the flag-detection / injection form to match if needed.

### Task A2: merge the per-TU CSVs

**Files:** Modify `clang/utils/analyzer/aqb/analyze.py`; Modify `test_analyze.py`.

- [ ] **Step 1: failing test** — given two CSV files with the same header and disjoint rows, `merge_entry_point_csvs([p1, p2])` returns one header + all rows (sorted, deduped):

```python
def test_merge_entry_point_csvs(self):
    with tempfile.TemporaryDirectory() as d:
        a = os.path.join(d, "1.csv")
        b = os.path.join(d, "2.csv")
        with open(a, "w") as f:
            f.write("USR,File,DebugName\nu1,f1,d1\n")
        with open(b, "w") as f:
            f.write("USR,File,DebugName\nu2,f2,d2\n")
        merged = merge_entry_point_csvs([a, b])
        self.assertEqual(merged[0], "USR,File,DebugName")
        self.assertIn("u1,f1,d1", merged)
        self.assertIn("u2,f2,d2", merged)
        self.assertEqual(len(merged), 3)  # header + 2 rows
```

- [ ] **Step 2: run → fail.** **Step 3: implement** in `analyze.py`:

```python
def merge_entry_point_csvs(csv_paths: List[str]) -> List[str]:
    """Merge per-TU entry-point CSVs (identical headers) into one line list:
    a single header followed by the sorted-unique union of data rows. Empty or
    header-only files contribute nothing. Raises if headers disagree.
    """
    header: str = ""
    rows: set = set()
    for path in csv_paths:
        with open(path) as handle:
            lines = [ln.rstrip("\n") for ln in handle if ln.strip()]
        if not lines:
            continue
        if not header:
            header = lines[0]
        elif lines[0] != header:
            raise ValueError(f"CSV header mismatch in {path!r}")
        rows.update(lines[1:])
    if not header:
        return []
    return [header, *sorted(rows)]
```

- [ ] **Step 4: run → pass. Step 5: black + commit.**

### Task A3: wire the wrapper into `analyze_run_argv`

**Files:** Modify `analyze.py` + `test_analyze.py`.

- [ ] Point `CC` at the wrapper, set `AQB_REAL_CLANG=/analyzer/bin/clang` and `AQB_EP_CSV_DIR=<per-project stats dir under /projects>`, and **drop** `dump-entry-point-stats-to-csv` from the shared `--extra-analyzer-config` (the wrapper injects it). Add constants `EP_CSV_DIR_NAME = "entry-point-stats"` and `WRAPPER = "aqb/clang-analyzer-wrapper.sh"`. `analyze_run_argv` gains `ep_csv_dir` (in-container path) and sets the three env vars; `analyzer_config` no longer takes `ep_csv_path` (only `extra`). Tests assert `-e CC=/scripts/aqb/clang-analyzer-wrapper.sh`, `-e AQB_REAL_CLANG=/analyzer/bin/clang`, `-e AQB_EP_CSV_DIR=<dir>` are present and that the driver's `--extra-analyzer-config` no longer contains `dump-entry-point-stats-to-csv`. Update `analyze_driver.py` only if the flag surface changes (it stays: `--extra-analyzer-config` passthrough).
- [ ] black + commit.

### Task B: `aqb run` orchestration + CLI verb

**Files:** Create `clang/utils/analyzer/aqb/run.py`; Modify `aqb/cli.py`; Test `aqb/tests/test_run.py`.

- [ ] `run.py` orchestration (fake-runtime unit tests): `resolve_or_build_clang` (3b) → `corpus.select_projects` (3a) → materialize a host corpus dir (writable) + stage the analyzer scripts dir → `runtime.run(analyze_run_argv(...))` → `collect_plists` + `collect_entry_point_csvs`/`merge_entry_point_csvs` → `normalize.load_findings` (fold **deterministic dedup**: stable sort, then min-`path_length` per key) + `metrics.parse_*` → build `Metadata` (`AnalyzerProvenance` from the `ClangVolume` — 3b must expose `config_digest`+`volume`; `commit`/`commit_title` from inputs; `ProjectProvenance` from `ProjectInfo`, mapping `source.value`, per-project `commit_title`) → `RunStore.create_run`, writing `reports/`, `metrics/` (incl. the merged entry-point CSV), `logs/`.
- [ ] `run` CLI verb: `--commit` (default HEAD), `--source`, `--projects`/`--size`, `--runtime`/`--memory`/`--cpus`. Mounts the host corpus/output dirs into the container (host-FS-not-visible lesson from 3b). Prints the created run id/path.
- [ ] TDD each seam with a fake `Runtime` and fixture plist/CSV outputs; black + commit per task.

## Sub-phase 3c-3 — end-to-end + docs

**STATUS: COMPLETE — validated end-to-end on the container runtime (2026-07-17).**
`aqb run --commit 83fd1f0ef0c6 --source … --projects zstd --runtime docker`
resolved the cached Clang Volume (no rebuild), analyzed zstd, and persisted run
`r-20260717-221515-b593`: `metadata.json` (analyzer commit/volume/digest,
container image digest, zstd pinned commit), `reports/findings.json` (985 deduped
findings), `metrics/entry-point-stats.csv` (623 unique entry-point rows over 41
files — the clobber-free merge). `AQB-design.rst` updated with the analyze seam.

Known follow-ups (non-blocking, for a later phase):

- `logs/` is not yet populated (`perform_run` doesn't copy the per-project
  `RefScanBuildResults/Logs/run_static_analyzer.log` into the run's `logs/`).
- Findings include CMake compiler-probe TUs (e.g. `CMakeCCompilerABI.c`),
  inherent to cmake recipes; a Normalize-stage filter could drop non-corpus
  sources. SATest has the same behavior.

- [x] Daemon-gated end-to-end smoke: `aqb build-clang` → `aqb run --projects zstd`
  → populated run in the store, CSV covering all TUs. **Done.**
- [x] Update `AQB-design.rst`: analyze flow (reference-build driver, per-TU
  wrapper, merge), the `run` verb, resolved mount/env contract. **Done.**

## Self-Review

**Honesty about placeholders:** 3c-1 Task 1 is full TDD code. Tasks 2–3 and 3c-2/3c-3 are deliberately *not* pretend-precise, because the SATest analyze-invocation, scan-build packaging, and container entry are genuine unknowns that a ground-truth pass + one real container run must settle (documented as Open Questions). This mirrors how the Phase-3b builder image was authored then validated on real runs. Executing 3c-1 begins with resolving those, then the remaining tasks become precise.

**Reuse:** analyze reuses SATest's `scan_build`/project-recipe machinery (mount volume at `/analyzer`) and the Phase-3a data model (`normalize`, `metrics`, `corpus`). No reimplementation of scan-build orchestration.
