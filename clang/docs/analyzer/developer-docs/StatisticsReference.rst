=============================
Analysis Statistics Reference
=============================

This page is a catalog of every statistic the Clang Static Analyzer collects
today. It is a companion to :doc:`Statistics`, which explains the facilities,
granularities, and output channels in prose; read that first if the terms below
are unfamiliar.

.. note::

   This catalog is maintained by hand and can drift from the source. The
   authoritative definitions are the ``STATISTIC`` / ``ALWAYS_ENABLED_STATISTIC``
   / ``STAT_COUNTER`` / ``STAT_MAX`` declarations and the ``UnsignedEPStat`` /
   ``CounterEPStat`` / ``UnsignedMaxEPStat`` objects in the sources. To
   regenerate the raw list, run::

       grep -rnE 'STATISTIC\(|ALWAYS_ENABLED_STATISTIC\(|STAT_COUNTER\(|STAT_MAX\(|UnsignedEPStat |CounterEPStat |UnsignedMaxEPStat ' clang/lib/StaticAnalyzer clang/lib/Analysis

.. contents::
   :local:

Legend
======

**Kind** --- the facility/macro used, which also determines the granularity and
where the value is emitted:

.. list-table::
   :header-rows: 1
   :widths: 26 20 54

   * - Kind
     - Granularity
     - Emitted through
   * - ``ALWAYS_ENABLED_STATISTIC``
     - Translation unit
     - LLVM statistics dump (``-analyzer-stats``) and plist
       (``serialize-stats=true``). Always computed.
   * - ``STATISTIC``
     - Translation unit
     - Same as above, but only when ``LLVM_ENABLE_STATS`` /
       ``LLVM_FORCE_ENABLE_STATS`` is on.
   * - ``STAT_COUNTER`` (additive)
     - Both
     - Translation-unit dump/plist **and** the per-entry-point CSV
       (``dump-entry-point-stats-to-csv``).
   * - ``STAT_MAX`` (maximizing)
     - Both
     - Same as ``STAT_COUNTER``.
   * - ``UnsignedEPStat`` (set once)
     - Entry point
     - Per-entry-point CSV only (empty cell if never set).

Frontend and analysis overview
==============================

Defined in ``clang/lib/StaticAnalyzer/Frontend/AnalysisConsumer.cpp``.

.. list-table::
   :header-rows: 1
   :widths: 34 22 44

   * - Statistic
     - Kind
     - Meaning
   * - ``NumFunctionTopLevel``
     - ``STAT_COUNTER``
     - Number of top-level functions.
   * - ``NumFunctionsAnalyzed``
     - ``ALWAYS_ENABLED_STATISTIC``
     - Number of functions analyzed as top level with inlining turned on.
   * - ``NumFunctionsAnalyzedSyntaxOnly``
     - ``ALWAYS_ENABLED_STATISTIC``
     - Number of functions analyzed by syntax (AST) checkers only.
   * - ``NumBlocksInAnalyzedFunctions``
     - ``ALWAYS_ENABLED_STATISTIC``
     - Total number of basic blocks in the analyzed functions (derived from the
       per-function summaries).
   * - ``NumVisitedBlocksInAnalyzedFunctions``
     - ``ALWAYS_ENABLED_STATISTIC``
     - Number of those basic blocks that were actually visited.
   * - ``PercentReachableBlocks``
     - ``ALWAYS_ENABLED_STATISTIC``
     - Visited blocks as a percentage of total blocks.
   * - ``MaxCFGSize``
     - ``ALWAYS_ENABLED_STATISTIC``
     - Maximum number of basic blocks in a single analyzed function.
   * - ``CFGSize``
     - ``UnsignedEPStat``
     - Number of CFG blocks in this entry point's function.
   * - ``PathRunningTime``
     - ``UnsignedEPStat``
     - Wall time (ms) spent in path-sensitive analysis of this entry point.
   * - ``SyntaxRunningTime``
     - ``UnsignedEPStat``
     - Wall time (ms) spent in syntax-based analysis of this entry point.

Core engine
===========

Defined in ``clang/lib/StaticAnalyzer/Core/CoreEngine.cpp``.

.. list-table::
   :header-rows: 1
   :widths: 34 22 44

   * - Statistic
     - Kind
     - Meaning
   * - ``NumSteps``
     - ``STAT_COUNTER``
     - Number of symbolic-execution steps executed.
   * - ``NumSTUSteps``
     - ``STAT_COUNTER``
     - Number of single-translation-unit steps executed.
   * - ``NumCTUSteps``
     - ``STAT_COUNTER``
     - Number of cross-translation-unit steps executed.
   * - ``NumReachedMaxSteps``
     - ``ALWAYS_ENABLED_STATISTIC``
     - Number of times the maximum number of steps (``max-nodes``) was reached.
   * - ``NumPathsExplored``
     - ``STAT_COUNTER``
     - Number of paths explored by the analyzer.

Expression engine
=================

Defined in ``clang/lib/StaticAnalyzer/Core/ExprEngine.cpp`` and
``clang/lib/StaticAnalyzer/Core/ExprEngineCallAndReturn.cpp``.

.. list-table::
   :header-rows: 1
   :widths: 40 18 42

   * - Statistic
     - Kind
     - Meaning
   * - ``NumRemoveDeadBindings``
     - ``STAT_COUNTER``
     - Number of times ``RemoveDeadBindings`` was called.
   * - ``NumMaxBlockCountReached``
     - ``STAT_COUNTER``
     - Number of paths aborted for reaching the maximum block visit count in a
       top-level function.
   * - ``NumMaxBlockCountReachedInInlined``
     - ``STAT_COUNTER``
     - As above, but for an inlined function.
   * - ``NumTimesRetriedWithoutInlining``
     - ``STAT_COUNTER``
     - Number of times a call was re-evaluated without inlining.
   * - ``NumOfDynamicDispatchPathSplits``
     - ``STAT_COUNTER``
     - Number of times a path was split due to imprecise dynamic-dispatch
       information.
   * - ``NumInlinedCalls``
     - ``STAT_COUNTER``
     - Number of times a call was inlined.
   * - ``NumReachedInlineCountMax``
     - ``STAT_COUNTER``
     - Number of times the inline count maximum was reached.

Worklist
========

Defined in ``clang/lib/StaticAnalyzer/Core/WorkList.cpp``.

.. list-table::
   :header-rows: 1
   :widths: 34 22 44

   * - Statistic
     - Kind
     - Meaning
   * - ``MaxQueueSize``
     - ``STAT_MAX``
     - Maximum size the worklist reached.
   * - ``MaxReachableSize``
     - ``STAT_MAX``
     - Maximum size of the auxiliary worklist (reachable) set.

Constraint solving (Z3 crosscheck)
==================================

Defined in ``clang/lib/StaticAnalyzer/Core/Z3CrosscheckVisitor.cpp``. These
measure the optional Z3-based refutation pass; they are only meaningful when Z3
crosschecking is enabled.

.. list-table::
   :header-rows: 1
   :widths: 44 16 40

   * - Statistic
     - Kind
     - Meaning
   * - ``NumZ3QueriesDone``
     - ``STAT_COUNTER``
     - Number of Z3 queries performed.
   * - ``NumTimesZ3TimedOut``
     - ``STAT_COUNTER``
     - Number of times a Z3 query timed out.
   * - ``NumTimesZ3ExhaustedRLimit``
     - ``STAT_COUNTER``
     - Number of times a Z3 query exhausted the resource limit.
   * - ``NumTimesZ3SpendsTooMuchTimeOnASingleEQClass``
     - ``STAT_COUNTER``
     - Number of times a report equivalence class was cut short for spending too
       much time in Z3.
   * - ``NumTimesZ3QueryAcceptsReport``
     - ``STAT_COUNTER``
     - Number of Z3 queries that accepted a report.
   * - ``NumTimesZ3QueryRejectReport``
     - ``STAT_COUNTER``
     - Number of Z3 queries that rejected a report.
   * - ``NumTimesZ3QueryRejectEQClass``
     - ``STAT_COUNTER``
     - Number of times a whole report equivalence class was rejected.
   * - ``TimeSpentSolvingZ3Queries``
     - ``STAT_COUNTER``
     - Total time spent solving Z3 queries, excluding retries.
   * - ``MaxTimeSpentSolvingZ3Queries``
     - ``STAT_MAX``
     - Maximum time spent solving a single Z3 query, excluding retries.

Bug reporting
=============

Defined in ``clang/lib/StaticAnalyzer/Core/BugReporter.cpp``.

.. list-table::
   :header-rows: 1
   :widths: 40 18 42

   * - Statistic
     - Kind
     - Meaning
   * - ``MaxBugClassSize``
     - ``STAT_MAX``
     - Maximum number of bug reports in a single equivalence class.
   * - ``MaxValidBugClassSize``
     - ``STAT_MAX``
     - Maximum number of bug reports in an equivalence class that has at least one
       valid (non-suppressed) report.
   * - ``NumTimesReportPassesZ3``
     - ``STAT_COUNTER``
     - Number of reports that passed the Z3 refutation check.
   * - ``NumTimesReportRefuted``
     - ``STAT_COUNTER``
     - Number of reports refuted by Z3.
   * - ``NumTimesReportEQClassAborted``
     - ``STAT_COUNTER``
     - Number of times a report equivalence class was aborted by the Z3 oracle
       heuristic.
   * - ``NumTimesReportEQClassWasExhausted``
     - ``STAT_COUNTER``
     - Number of times all reports in an equivalence class were refuted.

Checker statistics
==================

Defined in ``clang/lib/StaticAnalyzer/Checkers/AnalyzerStatsChecker.cpp``. These
counters are updated by the ``debug.Stats`` checker, which additionally emits a
per-entry-point warning summarizing total vs. unreachable CFG blocks, whether the
block budget was exhausted, whether the worklist emptied, and a note at each
block where the engine generated a sink.

.. list-table::
   :header-rows: 1
   :widths: 34 22 44

   * - Statistic
     - Kind
     - Meaning
   * - ``NumBlocks``
     - ``STAT_COUNTER``
     - Number of CFG blocks in top-level functions.
   * - ``NumBlocksUnreachable``
     - ``STAT_COUNTER``
     - Number of unreachable CFG blocks in top-level functions.

Call graph
==========

Defined in ``clang/lib/Analysis/CallGraph.cpp``. Unlike most analyzer
statistics, these use the gated ``STATISTIC`` macro, so they are only recorded
when statistics are enabled at build time.

.. list-table::
   :header-rows: 1
   :widths: 34 22 44

   * - Statistic
     - Kind
     - Meaning
   * - ``NumObjCCallEdges``
     - ``STATISTIC``
     - Number of Objective-C method call edges in the call graph.
   * - ``NumBlockCallEdges``
     - ``STATISTIC``
     - Number of block call edges in the call graph.

Timers
======

Not statistics in the ``Statistic.h`` sense, but reported alongside them.
``AnalysisConsumer`` creates an ``llvm::TimerGroup`` named ``analyzer`` whenever
any statistics/progress flag is set, and prints it on exit. It contains three
timers:

.. list-table::
   :header-rows: 1
   :widths: 24 76

   * - Timer
     - Meaning
   * - ``syntaxchecks``
     - Time spent in syntax-based (AST) analysis.
   * - ``exprengine``
     - Time spent exploring paths in the symbolic execution engine.
   * - ``bugreporter``
     - Time spent in path-sensitive report post-processing.
