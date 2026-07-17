=================================================
AQB: Analysis Qualification Bench --- Design Spec
=================================================

.. contents::
   :local:

Status and Intent
=================

This document is a **design specification** for a new tool, the *Analysis
Qualification Bench* (**AQB**), that unifies the capabilities of three existing
static-analysis evaluation tools and grounds them in the Clang Static Analyzer's
statistics facility:

- **SATest** (``clang/utils/analyzer/``) --- the in-tree qualification harness:
  committed-baseline diffing with a two-tier report-identity model, a Docker
  reproducibility envelope, and a pass/fail verdict.
- **csa-testbench** --- an A/B differential experiment framework
  (*configurations x projects* matrix) that delegates storage/dedup/diff/UI to
  CodeChecker and adds per-TU timing, coverage, and engine statistics.
- **CTIT** --- a single-candidate clang-tidy check tester: run one check over a
  pinned corpus, report counts/crashes/per-check profile, no baseline diff.
- **Analysis Statistics** (:doc:`Statistics`, :doc:`StatisticsReference`) --- the
  engine's own per-TU and per-entry-point metric facility, which is AQB's source
  of truth for what can be measured.

AQB's purpose is to let a Clang Static Analyzer developer **experiment with and
measure the effect of a change** --- to reports, to robustness, and to cost ---
across a corpus of real-world C/C++ projects, under a single tool.

AQB v1 targets the **Clang Static Analyzer only**. CTIT is drawn on for its
single-candidate evaluation model and its measurement ideas, not because AQB
runs clang-tidy; clang-tidy support is out of scope for v1 (see `Scope and
Non-Goals`_).

This spec is written around a concrete implementation approach (see
`Approach: New Orchestration, Reused Cores`_); it is not implementation-neutral.

The Question AQB Answers
------------------------

Given a change to the analyzer, and a corpus of pinned
real-world projects, AQB answers:

- **Robustness** --- does the analyzer crash, assert, or fail to complete on any
  translation unit? (An unconditional regression; needs no reference.)
- **Report-set change** --- relative to a reference, which reports are added,
  removed, modified, or unchanged? (AQB surfaces the delta precisely; it does not
  judge whether a delta is good or bad.)
- **Cost** --- how much wall-clock time and peak memory does analysis consume, and
  --- measured distributionally over repeated runs --- did the change shift it?

Approach: New Orchestration, Reused Cores
=========================================

AQB is a **new in-tree tool** rather than an extension of ``SATestBuild.py``.
SATest's flow is a linear "build then compare against a committed baseline"; the
scope AQB requires --- runs as first-class persisted artifacts, arbitrary
run-vs-run diffing, repeated-iteration benchmarking with distributional charts,
and a swappable container runtime --- is cross-cutting and does not fit that flow
cleanly.

However, AQB is **not a rewrite**. It reuses SATest's proven, hard-to-reproduce
algorithmic cores by direct import, and only rebuilds the orchestration around
them:

.. list-table::
   :header-rows: 1
   :widths: 40 60

   * - Reused verbatim from SATest
     - Rebuilt fresh in AQB
   * - Corpus registry and metadata (``ProjectMap.py``, ``projects/projects.json``)
     - Run Store (persisted, addressable run artifacts)
   * - Report identity, similarity, and classification (``CmpRuns.py``)
     - CLI verbs (``run`` / ``diff`` / ``plot`` / ``promote`` / ``report``)
   * - Baseline normalization contract
     - Iteration-based benchmarking + candlestick charts
   * - Cost-as-distribution benchmarking (``SATestBenchmark.py``)
     - Runtime selection (docker-compatible executable name)

SATest is deprecated and retired once AQB reaches feature parity.

Conceptual Model
================

The Pipeline and Its Units
--------------------------

AQB is a pipeline of small, independently testable units. Each has one
responsibility and a well-defined interface; the two places existing tools
disagree --- *where the comparison reference comes from* (Compare) and *where
results are stored* (Report) --- are isolated behind boundaries rather than woven
through the orchestrator.

.. list-table::
   :header-rows: 1
   :widths: 16 44 40

   * - Unit
     - Does
     - Input -> Output
   * - **Corpus**
     - Select pinned projects from the registry by name / size class / enabled
       flag
     - ``projects.json`` -> project list (deterministic, analyzer-independent)
   * - **Materialize**
     - Fetch sources at the pinned revision, apply an optional patch, cache
     - fetch recipe -> source tree
   * - **Isolate**
     - Provide the execution environment
     - image spec -> container the build/analyze steps run in
   * - **Analyze**
     - Drive the project build, intercept compilations, run the Clang Static
       Analyzer with a fixed checker/config
     - source tree + config -> raw reports + timing/mem + crash artifacts + stats
       channels
   * - **Observe**
     - Collect the raw per-TU artifacts
     - run dir -> artifact set
   * - **Normalize**
     - Canonicalize reports: relativize paths, strip transient fields, assign a
       stable identity
     - raw reports -> normalized reports
   * - **Compare**
     - Diff the candidate against a **Reference** (see below); classify each report
     - normalized set + reference -> classification
   * - **Report**
     - Render results (self-contained HTML/JSON; backend is pluggable)
     - classification + metrics -> report
   * - **Verdict**
     - Apply the ``--expect`` policy -> pass/fail + summary
     - classification + crash evidence -> verdict (exit code)

The Reference Abstraction (three modes, one diff engine)
--------------------------------------------------------

The three comparison models AQB supports are **the same Compare stage with a
different Reference provider**:

- **Committed baseline** (SATest-style) --- Reference = a stored, normalized
  baseline on disk.
- **Another run** (csa-testbench-style A/B) --- Reference = another run's
  normalized report set.
- **None** (CTIT-style single-candidate) --- Reference = the empty set; every
  finding is "added", so the report is absolute counts + crashes + cost with no
  subtraction.

There is no declared "experiment matrix" file. The *configurations x projects*
matrix is **emergent**: the user produces runs (each run = one analyzer
configuration over selected projects) and composes them afterwards with ``diff``
and ``plot``.

Two Signal Streams
------------------

Observe produces two independent streams that are consumed differently per mode:

- **Report stream** --- the findings. Flows Normalize -> deduplicate ->
  Compare-against-Reference. Only *matters* when there is something to diff.
- **Metrics stream** --- per-TU and per-entry-point statistics plus timing and
  peak memory. This is exactly the facility documented in :doc:`Statistics`: TU
  level via ``-analyzer-stats`` / ``serialize-stats=true``, per entry point via
  ``dump-entry-point-stats-to-csv``, and the ``analyzer`` ``TimerGroup``. Flows
  into distributional aggregation.

**Deduplication applies to both streams.** The identity/join keys are:

- Reports --- the stable content-hash issue identity (SATest's model, below).
- Metrics --- **TU keyed by main source file; entry point keyed by USR** (the
  per-entry-point CSV's stable key). This both lets samples line up across
  iterations to form a distribution *and* prevents a header analyzed through many
  TUs from being counted many times.

Iterations (N) and Benchmarking
-------------------------------

The iteration count ``N`` is orthogonal to the Reference:

- **N = 1 (functional run).** Capture reports (deduplicated) *and* a single
  metrics sample per TU/entry-point.
- **N > 1 (benchmark run).** Run the *same config on the same project* N times.
  Reports are **not** diffed or rendered --- they are irrelevant to a benchmark.
  Instead each ``(TU, metric)`` accrues N samples, forming a **distribution**
  rendered as a **candlestick** (low/high = min/max, box = quartiles, midline =
  median). Per entry point (USR) is the finer optional granularity.

Robustness is *always* observed, including during benchmarking: a crash in **any**
of the N iterations is a fatal signal even when reports are otherwise ignored.

Runs Are Persisted Artifacts; Measuring != Presenting
-----------------------------------------------------

A **run is a durable, addressable, self-describing artifact**. Everything
downstream --- diffing, plotting, reporting --- consumes stored runs *by
reference*, not only within a single live invocation. This is what lets a user
benchmark config A today, benchmark config B next week, and chart them together:
the two benchmarks never had to be part of the same invocation --- they only have
to be joinable and carry provenance.

Consequently, **measurement and presentation are separate actions** (separate CLI
verbs); see `CLI Surface`_.

Report Identity and the Baseline Contract
==========================================

Reused verbatim from SATest (``CmpRuns.py`` and the normalization step), because
this is the single most valuable and hardest-to-reproduce piece.

Two-tier report identity
-------------------------

Deciding when two diagnostics from two different analyzer builds are "the same
report" is the hardest problem in diff-based qualification.

Matching is first **scoped to a location bucket**: diagnostics are grouped by
``file:line:col`` (``get_grouped_diagnostics``) and the tiers below only compare
reports that share a location. Because AQB's corpus is **pinned**, the source is
byte-identical between a run and its reference, so a given issue's location is
stable across runs --- the only thing that varies is the analyzer.

- **Tier 1 --- stable issue identifier.** Within a location bucket, each report
  carries an identifier (``get_issue_identifier``) built from the source file, the
  enclosing ``issue_context`` (typically the function), and
  ``issue_hash_content_of_line_in_context`` --- a hash of the line's content, not
  its line *number*. Its job here is to distinguish multiple distinct issues that
  share a location and to stay stable across analyzer changes that do not move the
  report. The checker name, category, and description are deliberately **not** part
  of this identity. Equal identifiers = the same issue; only attributes (e.g. path
  length) are then compared.

- **Tier 2 --- fuzzy similarity fallback.** Reports unmatched by Tier 1 are not
  immediately declared added/removed: two otherwise-unmatched reports at the same
  location are treated as the *same issue, modified* (``is_similar_to``) if they
  agree on **at least one** of ``check_name``, ``category``, or ``description``.
  This is a boolean rule (similar unless *all three* differ), not a scored
  similarity.

Why Tier 2 still matters under a pinned corpus: pinning fixes the source, but the
analyzer binary --- the thing under test --- is exactly what churns a report's own
fields. A change that renames a checker, reclassifies a bug's category, or rewords
a message would, under Tier 1 alone, look like one *removed* plus one *added*
report. Tier 2 folds those into a single **modified** entry, keeping the reviewer
focused on genuine added/removed findings. (The ``--expect same-reports`` mode
deliberately wants every such difference to count and effectively bypasses this
tier.)

Note: ``CmpRuns.py`` also carries a ``# FIXME`` sketching a richer structural
fuzzy matcher that is **not implemented**; AQB inherits the attribute-overlap rule
above, which is sufficient for a pinned corpus.

Only reports unmatched by both tiers are classified as genuinely **added** or
**removed**. The payoff is signal-to-noise: "found/lost a bug" (added/removed) is
separated from "says the same thing slightly differently" (modified).

Normalization
-------------

Before a run is stored or compared, run-to-run and machine-to-machine detail is
stripped so a surviving difference reflects real analyzer behavior:

- **Absolute paths are made relative** to the project root.
- **Transient fields are removed** (e.g. the generated HTML rendering of each
  report). The analyzer version string is *not* used for identity; provenance
  captures it separately (see `The metadata.json File`_).

The commit / promote lifecycle
-------------------------------

A committed baseline is a stored, normalized, reviewed run. Changing it is a
deliberate, auditable act performed by the ``promote`` verb --- never an automatic
side effect --- so accidental baseline drift is impossible.

Measurements and Metrics
========================

AQB's metric source of truth is the analyzer's statistics facility; see
:doc:`Statistics` for the facilities and output channels and
:doc:`StatisticsReference` for the per-statistic catalog. AQB captures:

- **Translation-unit statistics** (``ALWAYS_ENABLED_STATISTIC`` / ``STATISTIC``)
  --- via ``-analyzer-stats`` (stderr) or ``serialize-stats=true`` (plist JSON).
- **Per-entry-point statistics** (``STAT_COUNTER`` / ``STAT_MAX`` /
  ``UnsignedEPStat``) --- via ``dump-entry-point-stats-to-csv``, keyed by USR.
- **Timers** --- the ``analyzer`` ``TimerGroup`` (``syntaxchecks``,
  ``exprengine``, ``bugreporter``), plus ``PathRunningTime`` / ``SyntaxRunningTime``
  per entry point.
- **Cost** --- wall-clock time and peak memory, sampled per iteration.
- **Robustness** --- crashes, assertions, ``UNREACHABLE``, non-zero exit,
  mined from failure output.

Candlestick charts are drawn per ``(TU, metric)`` (or ``(entry-point, metric)``)
over the N iterations of a benchmark. A ``plot`` of multiple runs overlays their
candles, each series labeled from provenance.

Comparison Modes Summary
========================

.. list-table::
   :header-rows: 1
   :widths: 30 22 8 40

   * - Mode
     - Reference
     - N
     - Rendered
   * - Single-candidate (CTIT-like)
     - none
     - 1
     - reports (deduplicated) + metrics
   * - A/B or vs-baseline diff (SATest / csa-testbench)
     - another run, or committed baseline
     - 1
     - report classification (+ metric delta)
   * - Benchmark
     - none, or another run at ``plot`` time
     - > 1
     - metric candlesticks (+ robustness)

Verdict / Expectation Policy
============================

Reused from SATest. The operator declares the expectation for the change with the
``--expect`` flag; ``diff`` / ``report`` check it and surface the result as an
**exit code**, so CI-gating can be layered on later without new machinery. Each
level is chosen to match the *situation* the change is in:

- ``--expect no-crashes`` --- pass unless something crashed/asserted/failed;
  report changes are tolerated. For *exploratory* changes where reports are
  expected to move and a human will inspect the delta.
- ``--expect same-count`` --- pass unless the *number* of reports changed relative
  to the reference. For *tuning/feature* changes: catches a flood of new false
  positives or a checker going silent while tolerating cosmetic per-report churn.
- ``--expect same-reports`` --- pass only if every report matches the reference.
  For *NFC / refactor* changes where any difference is itself the bug (this level
  bypasses the fuzzy Tier 2, so every field difference counts).

Robustness is always evaluated (it needs no reference), and a crash in any
benchmark iteration is fatal.

CLI Surface
===========

There is no experiment-definition file. ``projects.json`` *is* the corpus
registry; a run is defined by CLI flags (the analyzer **commit** to build, build
config, analyzer args, and project selection). The verbs split measurement from
presentation:

.. list-table::
   :header-rows: 1
   :widths: 16 84

   * - Verb
     - Action
   * - ``run``
     - Resolve-or-build the Clang Volume for ``--commit`` (see `Clang Build
       Artifacts`_), then Materialize + Analyze + Observe + Normalize; persist a
       Run. ``N = 1`` functional (reports + metrics), or ``--bench -n N`` benchmark
       (metrics distribution; reports ignored). An optional ``--note "<text>"``
       is recorded on the run. *Measurement only.*
   * - ``diff``
     - Compare two stored runs, given explicitly as ``--base <run> --new
       <run>``; or a run against the committed baseline via ``--baseline --new
       <run>``. Classify reports + metric deltas. Honors ``--expect``.
   * - ``plot``
     - Take **a list of stored runs** and render a full report of overlaid metric
       candlesticks. Takes no configuration --- just runs.
   * - ``report``
     - Render a full HTML/JSON report for a run (may embed a diff and/or plots).
   * - ``promote``
     - Promote a stored run to the committed baseline (the auditable
       regenerate/update lifecycle).
   * - ``build-clang``
     - Build (or resolve) a Clang Volume for ``--commit`` using the builder image
       and a CMake preset (``--preset`` / ``--preset-file``), printing the volume
       name. A utility verb; ``run`` builds volumes implicitly.

Example Invocations
-------------------

::

   # 1. Single-candidate functional run (reports + metrics, deduplicated)
   aqb run --commit 349146da                            # -> run r-cand
   aqb report r-cand

   # 2. A/B report diff over two stored runs (explicit base/new)
   aqb run --commit v20-base                            # -> r-base
   aqb run --commit 349146da --note "with workaround X" # -> r-cand
   aqb diff --base r-base --new r-cand

   # 3. Diff vs. committed baseline, gate, then accept
   aqb diff --baseline --new r-cand --expect same-reports   # nonzero exit if any report differs
   aqb promote r-cand

   # 4. Benchmark now + later, then plot together
   aqb run --commit v20-base --bench -n 20             # -> b-base
   aqb run --commit 349146da --bench -n 20             # -> b-cand
   aqb plot b-base b-cand                               # full report, all candlesticks

   # build a commit that lives only in a local (unpushed) clone
   aqb run --commit 349146da --source /work/llvm-project

   # runtime selection
   aqb run --commit 349146da --runtime=podman
   AQB_RUNTIME=podman aqb run --commit 349146da

Analyze Seam: How AQB Reuses SATest (implemented)
-------------------------------------------------

The Analyze/Materialize/Observe units are implemented by reusing SATest's
project-recipe machinery inside a container, with two deliberate seams that a
casual reading of SATest would get wrong (both validated end-to-end on a real
container runtime over the ``zstd`` corpus):

- **Analyze without SATest's compare.** ``SATest.py build`` runs
  ``RegressionTester.test_all`` = build *and* compare-against-reference; even
  its ``-r`` (regenerate) path proved unreliable for analyze-only use. AQB
  instead drives the lower seam ``SATestAdd`` uses for a new project:
  ``ProjectTester(TestInfo(is_reference_build=True)).test()`` (via
  ``aqb/analyze_driver.py``). A *reference build* structurally writes to
  ``RefScanBuildResults/`` and skips ``run_cmp_results`` --- AQB does its own
  diffing later (the Compare unit). ``cleanup_reference_results`` strips
  transient HTML/CSS/JS + the log but keeps every ``.plist``.

- **Per-entry-point stats without clobbering.** ``EntryPointStat::dumpStatsAsCSV``
  opens its target with ``OF_Text`` (truncate) and runs once per TU, so a single
  shared ``dump-entry-point-stats-to-csv`` path across many clang processes is
  overwritten --- only the last TU survives. AQB sets ``CC`` to
  ``aqb/clang-analyzer-wrapper.sh`` (SATest uses ``CLANG = os.environ["CC"]`` as
  ``scan-build --use-analyzer``). scan-build's ``ccc-analyzer`` invokes that
  clang twice per TU --- ``clang -### --analyze`` to expand the frontend command,
  then ``clang -cc1 ... -analyze ...`` to analyze. The wrapper injects a
  **PID-unique** ``-analyzer-config dump-entry-point-stats-to-csv=$AQB_EP_CSV_DIR/$$.csv``
  **only** on the real ``-cc1 ... -analyze`` (cc1-native form; ``-Xclang`` is a
  driver-only flag rejected under ``-cc1``); every other invocation passes
  through. AQB then merges the per-TU CSVs (dedup header + sorted-unique rows,
  entry point keyed by USR).

Resolved container contract (the analyze ``run``): the Clang Volume mounts
``:ro`` at ``/analyzer``; the materialized corpus (``projects.json`` + selected
recipe dirs, staged writable) mounts at ``/projects`` (``-w /projects``); the
analyzer scripts dir (SATest modules + ``aqb/``) mounts ``:ro`` at ``/scripts``;
the shared ccache volume at ``/ccache``. Env: ``PATH=/analyzer/bin:...``,
``CC=/scripts/aqb/clang-analyzer-wrapper.sh``,
``AQB_REAL_CLANG=/analyzer/bin/clang``,
``AQB_EP_CSV_DIR=/projects/aqb-entry-point-stats``. Results are read back from
``/projects/<name>/RefScanBuildResults/`` (reports) and
``/projects/aqb-entry-point-stats/*.csv`` (metrics).

Run Artifacts and Provenance
============================

Runs live in the **Run Store** (self-contained backend; see `Storage Backend`_).
Each run has an ID and this layout:

::

   runs/r-0711-abc123/
     metadata.json     # provenance (see below); references a Clang Volume by name
     reports/          # normalized, deduplicated findings   (omitted for --bench runs)
     metrics/          # per-TU + per-entry-point stats, one set per iteration
     logs/             # build/analyze logs + crash artifacts

The analyzer binary is **not** copied into the run. It lives in a shared,
content-addressed **Clang Volume** (see `Clang Build Artifacts`_); the run only
records the volume's name and the identity needed to recreate it.

The metadata.json File
----------------------

All provenance is content-addressed / self-describing --- no soft "version"
strings:

- **Analyzer** --- the **commit hash and commit title** of that hash, the
  **build-config digest**, and the **Clang Volume name** the run used. The binary
  itself is not stored in the run; it is materialized in the Clang Volume and can
  be rebuilt on demand from the commit + build config (see `Clang Build
  Artifacts`_).
- **Corpus** --- the **git commit hash and title** of each pinned test project.
- **Container** --- the **image digest** (hash), not a tag.
- **Execution** --- the runtime name, iteration count ``N``, the analyzer
  arguments, and the optional free-text ``--note``. The note is a **run**
  property: it does not affect the Clang Volume and never triggers a rebuild.

Clang Build Artifacts
=====================

The analyzer binary a run exercises is stored once in a **Clang Volume** --- a
named runtime (docker/podman) volume holding a single clang **install tree**
(``bin/clang``, the resource dir, ``lib/clang/<v>/include`` --- not the whole
build tree). It is content-addressed and immutable once built, so many runs share
one copy instead of each run copying 1--3 GB.

**Identity.** A Clang Volume is keyed on the **commit hash** *and* a **digest of
the build configuration** --- the canonical JSON of the assembled CMake presets
(see below), the selected preset name, and the builder image --- so two builds of
the same commit with different configurations coexist and recreation is exact:

::

   name:   aqb-clang-<shortcommit>-<configdigest>
   labels: aqb.role=clang
           aqb.commit=<full 40-char hash>
           aqb.commit_title=<subject line>
           aqb.source=<git remote URL or absolute local path>
           aqb.build_config=preset=<name>               # digested via the preset JSON
           aqb.builder_image=<image content id>
           aqb.created=<ISO-8601 timestamp>             # NOT part of the digest

Labels are set at creation (they are immutable afterward) and carry everything
needed to rebuild the volume identically. ``aqb.source`` may be a git remote URL
*or an absolute path to a local clone*, so a commit that exists only in an
unpushed working repository can still be built. ``aqb.created`` records when the
volume was built; it is provenance only and is deliberately excluded from the
digest so it never affects the volume's identity.

The run-level ``--note`` is intentionally **not** a volume label: the note does
not change the clang artifact, so making it part of the volume identity would
force a needless rebuild for two runs that differ only in their note. Notes live
on runs (see `The metadata.json File`_), not on volumes.

**AQB owns the build recipe, expressed as CMake presets.** The build
configuration is a **CMake preset**: a built-in ``aqb-base`` preset (Release,
``clang``, assertions, ccache) plus an optional **user overlay** --- a
``CMakeUserPresets.json`` that may ``inherits: aqb-base`` (or LLVM's own tracked
``llvm-*`` presets). AQB assembles ``aqb-base`` + the overlay into one canonical
(sorted-key) ``CMakeUserPresets.json``; that canonical JSON is the digest input.
The builder fetches the commit from ``aqb.source`` (a git remote *or* an absolute
local clone path, so unpushed commits work), ``checkout <commit>``, writes the
assembled ``CMakeUserPresets.json`` into the checked-out ``llvm/`` directory
(alongside LLVM's tracked ``CMakePresets.json``, which it never clobbers), and
runs ``cmake --preset <name>`` --- forcing the build directory (``-B``) and
install prefix (``-D CMAKE_INSTALL_PREFIX``) on the command line, which override
any preset values. Owning the recipe is what makes automatic recreation possible.
There are no raw ``-D`` flags in the AQB interface; all configuration is a preset.

**Resolve-or-build workflow.** When a ``run`` needs a clang for a
``(commit, build configuration)``:

#. Compute the volume name ``aqb-clang-<shortcommit>-<configdigest>``.
#. ``<runtime> volume inspect`` it.

   - **Present and complete:** reuse it. The build writes a completion marker
     (``/opt/aqb/clang/.aqb-complete``) into the install tree as its *final* step;
     completeness is verified by running the builder image with ``test -e`` on the
     marker.
   - **Present but incomplete:** the residue of an interrupted build (killed
     before the marker was written). Discard the volume and rebuild. (If the
     completeness check cannot even run --- e.g. the builder image was pruned ---
     AQB raises rather than deleting a possibly-good volume.)
   - **Absent:** create the volume with its labels, ensure the shared cache
     volume, and run the builder container to build and install clang into it. On
     build failure the partial volume is removed.

The mount contract is fixed on both sides (Python and the builder script): the
Clang Volume mounts at ``/opt/aqb/clang`` and the cache volume at ``/ccache``.

The Shared Cache Volume
-----------------------

AQB manages one long-lived, shared **cache volume** (``aqb-ccache``, labeled
``aqb.role=cache``) mounted read-write into *both* the clang-builder container and
the project-analysis container. It holds a ``ccache`` (and, where applicable,
compiler-cache state) that speeds up two expensive, repeated compilations:

- **Building clang** for a new Clang Volume --- rebuilds after a small analyzer
  change hit warm cache instead of compiling LLVM from scratch.
- **Analyzing the corpus** --- ``scan-build``-style analysis compiles each project
  as it intercepts it, so the projects are effectively (re)built on every run;
  caching those object compilations makes repeated runs and benchmark iterations
  dramatically cheaper.

Unlike Clang Volumes, the cache volume is mutable, is *not* content-addressed, and
never affects any digest or run identity --- it only affects wall-clock build time,
never results. It is safe to prune at any time (the next run simply repopulates
it); note that pruning it slows the subsequent build/analysis but changes nothing
about correctness or provenance.

**Reclamation and automatic recreation.** Clang Volumes are a *cache* and may be
pruned. Re-running a stored run's ``diff`` or ``plot`` needs no clang at all --- the
reports/metrics already live in the run artifact. Clang is only needed to
*execute analysis* (a fresh ``run``, or re-running one whose artifacts were
discarded). If the required volume was reclaimed, AQB **rebuilds it automatically**
from the recorded commit + build config before proceeding. The one limit: a run is
re-runnable only as long as its commit and recipe remain fetchable and the build
stays reproducible (a force-pushed branch or deleted fork breaks recreation); AQB
does not archive the binary to guard against that in v1.

Runtime
=======

AQB invokes a runtime executable whose CLI is **docker-compatible**; there is no
abstraction interface. AQB simply substitutes the program name in each
``run`` / ``build`` / ``pull`` / ``inspect`` invocation. Selection precedence:

1. ``--runtime=<name>`` flag,
2. ``AQB_RUNTIME`` environment variable,
3. default ``docker``.

``podman`` is the primary alternative; any other docker-CLI-compatible runtime
(e.g. ``nerdctl``) works by naming it. The recorded image digest is the runtime's
**content id** (``<runtime> image inspect --format {{.Id}}``), which is present
for both locally-built and pulled images; AQB does not rely on registry
``RepoDigests``.

Storage Backend
===============

The Run Store sits behind a backend boundary. Only the **self-contained** backend
(files on disk: ``metadata.json`` + ``reports/`` + ``metrics/``, plus the shared
Clang Volumes) is implemented. A **CodeChecker** backend is explicitly
**deferred**: the artifact and identity model is designed so it can be retrofitted
later (CodeChecker would provide storage/dedup/diff/web-UI), but AQB owns the
report-identity and diff logic regardless of backend.

Scope and Non-Goals
===================

- **Clang Static Analyzer only in v1.** clang-tidy is not run or evaluated. The
  pipeline is designed so a clang-tidy analyzer path could be added later behind
  the same units, but no such path is built now.
- **No false-positive triage / AI-reviewer worklist.** Unlike CTIT, AQB does not
  emit a per-finding TP/FP worklist. Correctness judgment is left to a human
  reading the diff/report.
- **No CI-service or scheduled-sweep surface in v1.** Local CLI only. CI-gating
  falls out of the ``diff``/``report`` exit code; a service/sweep can wrap the CLI
  later.
- **CodeChecker backend deferred** (above).
- **Not a build fixer or dependency resolver.** Each project must be buildable via
  its recipe inside the container image.
- **Does not adjudicate report correctness.** It surfaces the delta precisely and
  reproducibly.

Reproducibility and Provenance
==============================

A run is reconstructable from its artifacts: the analyzer commit + build-config
digest (which recreate the Clang Volume on demand), each project's pinned commit,
the container image digest, the analyzer arguments, and ``N`` are all captured in
``metadata.json``. Two runs of the same analyzer over the same corpus differ only
by measurement noise --- which is why cost is measured as a distribution
(benchmarking) rather than a single sample.

Load-bearing assumptions (inherited from SATest):

#. Diagnostics are independently addressable (a stable identity per report).
#. The corpus is representative enough for results to generalize.
#. The committed baseline is trusted at the moment it was promoted.
#. Runs are deterministic enough (non-crash signals are stable modulo
   normalization; cost varies only by bounded noise).
#. Only the analyzer binary changes between a run and its reference; corpus,
   checker set, config, and environment are held fixed.

Open Questions / Future Work
============================

- **clang-tidy support** --- add a clang-tidy analyzer path (single-candidate
  counts, crashes, per-check profiling) behind the existing units.
- **CodeChecker backend** --- retrofit when a browsable/shared web UI is needed.
- **CI-gate and on-demand service surfaces** --- wrap the CLI once local use is
  proven.
- **Coverage metric** --- csa-testbench's line-coverage signal requires a
  non-upstream instrumented clang; out of scope for v1, notable as a future
  dimension.
- **Combined structured output** --- there is currently no single document that
  merges TU-level and per-entry-point statistics (see :doc:`Statistics`); AQB's
  ``metrics/`` layout may standardize one.
