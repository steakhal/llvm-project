===================
Analysis Statistics
===================

The Clang Static Analyzer can report a range of numeric statistics that
describe what the engine did while analyzing a translation unit: how many
functions it analyzed, how many symbolic-execution steps it took, how often it
gave up on a path, how many solver queries it issued, and so on. These numbers
are the primary tool for understanding analyzer performance and coverage, for
spotting pathological inputs, and for evaluating the impact of a change.

This page is the *guide* to that facility: what kinds of statistics exist, at
what granularity they are collected, how the data leaves the process, and how to
add a new statistic. For an exhaustive, per-statistic catalog of everything the
analyzer collects today, see :doc:`StatisticsReference`.

.. contents::
   :local:

Overview
========

The analyzer collects statistics with two facilities:

- **Translation-unit-level** statistics use `llvm/ADT/Statistic.h`_. Each such
  statistic is a single number aggregated over the whole translation unit (for
  example, "the number of entry points analyzed").
- **Per-entry-point** statistics use
  `clang/StaticAnalyzer/Core/PathSensitive/EntryPointStats.h`_. These record one
  value *per analyzed entry point*, so you can see how an individual function
  behaved rather than just the translation-unit total.

Many quantities are interesting at both levels, so ``EntryPointStats.h`` also
provides *dual-tracking* macros that feed a translation-unit statistic and a
per-entry-point statistic from the same update site.

.. _llvm/ADT/Statistic.h: https://github.com/llvm/llvm-project/blob/main/llvm/include/llvm/ADT/Statistic.h#L171
.. _clang/StaticAnalyzer/Core/PathSensitive/EntryPointStats.h: https://github.com/llvm/llvm-project/blob/main/clang/include/clang/StaticAnalyzer/Core/PathSensitive/EntryPointStats.h

Granularity model
=================

Understanding *what a number counts* requires knowing the unit it is attached
to. There are three granularities in play.

Translation unit
    A single value for the entire translation unit, accumulated across every
    function the analyzer touches. This is the granularity of a plain
    ``STATISTIC`` / ``ALWAYS_ENABLED_STATISTIC``.

Entry point
    An **entry point** is a top-level function that the analyzer explores
    *path-sensitively*. The analyzer picks entry points from the translation
    unit and runs the symbolic execution engine starting from each one.

    Two consequences of this definition are worth calling out, because they
    determine how per-entry-point numbers should be read:

    - **Inlined callees are not entry points.** When the engine inlines a call,
      the work spent inside the callee is attributed to the entry point that
      pulled it in, not to the callee. The same function can therefore
      contribute to many entry points' totals (once per caller that inlines it)
      and also appear as its own entry point.
    - **Syntax-only functions produce no per-entry-point row.** A per-entry-point
      snapshot is taken only after path-sensitive analysis of a function
      (``EntryPointStat::takeSnapshot`` is called from
      ``AnalysisConsumer::HandleCode`` right after ``RunPathSensitiveChecks``).
      Functions that are only visited by AST/syntax checkers never get a
      snapshot.

Per-function summary
    Separately from the statistics facilities, the engine maintains a
    ``FunctionSummariesTy`` (see
    ``clang/StaticAnalyzer/Core/PathSensitive/FunctionSummary.h``) keyed by
    ``Decl``. It is not a statistics facility, but several translation-unit
    statistics are *derived* from it at the end of analysis --- for example
    ``NumBlocksInAnalyzedFunctions``, ``NumVisitedBlocksInAnalyzedFunctions``,
    and ``PercentReachableBlocks`` are computed from the visited/total basic
    block bitvectors it stores. It also holds data that is currently *not*
    surfaced as a statistic (such as ``TimesInlined`` and the inlining-eligibility
    decision).

Facilities
==========

Translation-unit statistics (``Statistic.h``)
---------------------------------------------

For a value that only makes sense for the whole translation unit --- for
example "the number of entry points" --- use one of the two macros from
`llvm/ADT/Statistic.h`_:

- ``ALWAYS_ENABLED_STATISTIC`` --- prefer this one.
- ``STATISTIC`` --- controlled by ``LLVM_ENABLE_STATS`` /
  ``LLVM_FORCE_ENABLE_STATS``.

Note that with ``LLVM_ENABLE_STATS`` disabled, only *storage* of the values is
disabled; the computations producing those values still run unless you took an
explicit precaution to make them conditional too. That is why
``ALWAYS_ENABLED_STATISTIC`` is usually the better choice: the computation is
already being paid for.

Per-entry-point statistics (``EntryPointStats.h``)
--------------------------------------------------

For a value that should be recorded once per entry point, ``EntryPointStats.h``
provides three classes:

- ``UnsignedEPStat`` --- an unsigned value assigned **at most once** per entry
  point. For example: "the number of source characters in an entry-point body".
  If no value is assigned during analysis of an entry point, the corresponding
  CSV cell will be empty.
- ``CounterEPStat`` --- an **additive** statistic. It starts at 0 and you can add
  to it as many times as needed. For example: "the number of bugs discovered".
- ``UnsignedMaxEPStat`` --- a **maximizing** statistic. It starts at 0 and when
  you join it with a value it keeps the larger of the two. For example: "the
  longest execution path".

Dual-tracking macros
--------------------

In many cases it makes sense to collect the same quantity at both granularities.
``EntryPointStats.h`` defines two macros for that. Each one updates a
per-entry-point statistic *and* a translation-unit ``llvm::TrackingStatistic``
from the same call site:

- ``STAT_COUNTER`` --- additive, for example "the number of steps executed" or
  "the number of functions inlined".
- ``STAT_MAX`` --- maximizing, for example "the maximum worklist size" or "the
  longest execution path".

Implementation and lifecycle
============================

The per-entry-point machinery lives in ``EntryPointStats.h`` and
``clang/lib/StaticAnalyzer/Core/EntryPointStats.cpp``. The flow is:

1. **Registration.** Every ``UnsignedEPStat`` / ``CounterEPStat`` /
   ``UnsignedMaxEPStat`` (including the ones created behind ``STAT_COUNTER`` /
   ``STAT_MAX``) registers itself into a process-wide registry when it is
   constructed at static-initialization time. Because these are static objects,
   the set of statistics is fixed by which translation units are linked in.

2. **Locking.** Early in ``AnalysisConsumer`` construction,
   ``EntryPointStat::lockRegistry`` is called. It sorts the registered
   statistics by name, validates the names, records the main source file name,
   freezes further registration, and installs a cleanup callback tied to the
   ``ASTContext`` teardown. After this point the column set is stable.

3. **Accumulation and snapshot.** While an entry point is analyzed, the various
   statistics are set/incremented/maximized in place. When analysis of the entry
   point finishes, ``EntryPointStat::takeSnapshot(D)`` copies the current values
   into a per-entry-point record and **resets** the live statistics so the next
   entry point starts clean.

4. **Serialization.** ``EntryPointStat::dumpStatsAsCSV`` writes one row per
   recorded entry point. The header is::

       USR,File,DebugName,<statistic names...>

   where ``USR`` is the Unified Symbol Resolution string for the entry-point
   ``Decl`` (via ``index::generateUSRForDecl``), ``File`` is the main source
   file, and ``DebugName`` is the human-readable function name. Statistic columns
   follow in a fixed order --- ``UnsignedEPStat`` columns first (which may be
   empty if never set), then the maximizing and counter columns (always at least
   ``0``). Rows are sorted by ``USR``.

Output channels
===============

The collected numbers can leave the process through several independent
channels. Each is controlled by its own flag, so you can enable only what you
need.

.. list-table::
   :header-rows: 1
   :widths: 22 30 24 24

   * - Channel
     - How to enable
     - Where it goes
     - What it contains
   * - LLVM statistics dump
     - ``-analyzer-stats`` (``AnalyzerOptions::PrintStats``)
     - stderr, via ``llvm::PrintStatistics()``
     - All translation-unit statistics.
   * - Statistics in plist
     - ``-analyzer-config serialize-stats=true``
     - The plist report, under the ``statistics`` key as embedded JSON
       (``llvm::PrintStatisticsJSON``)
     - All translation-unit statistics.
   * - Per-entry-point CSV
     - ``-analyzer-config dump-entry-point-stats-to-csv=<file>.csv``
     - The named CSV file
     - One row per entry point: ``USR``, ``File``, ``DebugName``, and every
       per-entry-point statistic.
   * - ``debug.Stats`` checker
     - ``-analyzer-checker=debug.Stats``
     - Analyzer warnings (``AnalyzerStatsChecker``)
     - Per-entry-point coverage: total vs. unreachable CFG blocks, whether blocks
       were exhausted, whether the worklist emptied, and a "sink" note at each
       block the engine bailed out on.
   * - Timers
     - Any of the statistics flags above (or ``-analyzer-display-progress``)
     - stderr, as an ``llvm::TimerGroup`` named ``analyzer`` printed on exit
     - Wall/user/system time for the ``syntaxchecks``, ``exprengine``, and
       ``bugreporter`` phases.

How the channels relate
-----------------------

These channels are not five independent views; they fall into three categories,
with some overlap worth understanding:

- **Serialized numeric stats** --- the LLVM statistics dump, the plist JSON, and
  the per-entry-point CSV. The LLVM dump and the plist JSON carry the *same*
  translation-unit data in two encodings. The CSV is a *superset* of the numeric
  data: every dual-tracked ``STAT_COUNTER`` / ``STAT_MAX`` appears there per entry
  point, so the translation-unit aggregate can be recomputed by summing or
  maximizing the CSV columns. The only values *not* recoverable from the CSV are
  the translation-unit-only ``ALWAYS_ENABLED_STATISTIC`` / ``STATISTIC`` ones
  (for example ``NumReachedMaxSteps`` and ``PercentReachableBlocks``), which have
  no per-entry-point counterpart.
- **Diagnostics** --- the ``debug.Stats`` checker is not a statistics *dump*. It
  emits coverage information into the normal diagnostics stream, so it reaches
  whichever ``PathDiagnosticConsumer`` is active (text, plist, HTML, SARIF).
- **Timing infrastructure** --- the timers are LLVM ``TimerGroup`` output,
  independent of the ``Statistic.h`` machinery (though ``PathRunningTime`` and
  ``SyntaxRunningTime`` mirror two of them per entry point).

There is currently no single output that combines translation-unit and
per-entry-point statistics in one structured document; consumers that want JSON
per entry point typically convert the CSV themselves (the in-tree test for
``dump-entry-point-stats-to-csv`` pipes it through a ``csv2json`` helper).

Interpreting the numbers
========================

A few things to keep in mind when reading the output:

- **Dual-tracked statistics appear twice.** A ``STAT_COUNTER`` / ``STAT_MAX``
  value shows up both in the translation-unit dump (summed/maximized over all
  entry points) and as a per-entry-point column in the CSV. The CSV column is the
  per-entry-point contribution; the translation-unit dump is the aggregate.

- **Several numbers are relative to analyzer budgets.** Many statistics only make
  sense next to the limits that produced them. For instance ``NumReachedMaxSteps``
  is relative to ``max-nodes``, ``NumMaxBlockCountReached`` /
  ``NumMaxBlockCountReachedInInlined`` to the per-block visit limit,
  ``NumCTUSteps`` to the ``ctu-max-nodes-pct`` / ``ctu-max-nodes-min`` budget, and
  inlining counts to ``max-inlinable-size``. ``graph-trim-interval`` changes the
  engine's working set (node reclamation) rather than these counts directly.
  These options live in ``AnalyzerOptions.def``.

Adding a new statistic
======================

Pick the facility that matches the granularity you need:

- A value meaningful only for the whole translation unit (for example, "the
  number of entry points"): use ``ALWAYS_ENABLED_STATISTIC`` (or ``STATISTIC``)
  from ``Statistic.h``.
- A value meaningful per entry point: use ``UnsignedEPStat``,
  ``CounterEPStat``, or ``UnsignedMaxEPStat`` depending on whether it is
  set-once, additive, or maximizing.
- A value meaningful at *both* levels: use ``STAT_COUNTER`` (additive) or
  ``STAT_MAX`` (maximizing), which wire up both for you.

``EntryPointStats.h`` is intentionally not exhaustive. If you feel it is lacking a
kind of statistic you need, odds are that it is --- feel free to extend it. When
you add a statistic, please also add a row to :doc:`StatisticsReference` so the
catalog stays current.
