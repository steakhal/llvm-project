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

## Sub-phases 3c-2 / 3c-3 — sequenced stubs

- **3c-2 (`aqb run`):** `aqb/run.py` orchestration + `run` CLI verb. materialize (3b) → analyze (3c-1) → collect → `normalize.load_findings` + `metrics` (3a) → build `Metadata` (`AnalyzerProvenance` from the `ClangVolume` (needs 3b to return/expose `config_digest`+`volume`; `commit`/`commit_title` from inputs), `ProjectProvenance` from `corpus.select_projects` — map `ProjectInfo.source.value`, resolve per-project `commit_title`) → `RunStore.create_run`, writing `reports/`, `metrics/`, `logs/`. Fold the 3a follow-up: **deterministic dedup** in `load_findings` (sort or min-`path_length`) before persisting, since findings now come from multiple plists. Mount the corpus/output dirs into the container (host-FS-not-visible lesson from 3b). `--projects`/`--size` selection; `--runtime`/`--memory`/`--cpus`.
- **3c-3:** daemon-gated end-to-end smoke (`build-clang` → `run` over one TINY project → assert a populated run in the store); analyze-container `:ro` clang mount confirmed; update `AQB-design.rst` (analyze flow, `run` verb).

## Self-Review

**Honesty about placeholders:** 3c-1 Task 1 is full TDD code. Tasks 2–3 and 3c-2/3c-3 are deliberately *not* pretend-precise, because the SATest analyze-invocation, scan-build packaging, and container entry are genuine unknowns that a ground-truth pass + one real container run must settle (documented as Open Questions). This mirrors how the Phase-3b builder image was authored then validated on real runs. Executing 3c-1 begins with resolving those, then the remaining tasks become precise.

**Reuse:** analyze reuses SATest's `scan_build`/project-recipe machinery (mount volume at `/analyzer`) and the Phase-3a data model (`normalize`, `metrics`, `corpus`). No reimplementation of scan-build orchestration.
