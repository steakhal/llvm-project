==========================================
Qualification Testing on External Projects
==========================================

.. contents::
   :local:

Besides the lit-based regression tests under ``clang/test/Analysis``, the Clang
Static Analyzer has a separate *qualification* testing infrastructure that runs
the analyzer over a curated set of real open-source C/C++ projects. It compares
the reports produced on those projects against checked-in reference results, and
can also measure analysis time and peak memory usage. This is the recommended way
to gauge how a change affects the analyzer on real-world code before landing it.

The term "qualification testing" is used here (rather than "regression testing")
to avoid confusion with the lit-based regression tests. The two are complementary:
lit tests pin down behavior on small, hand-written snippets, while qualification
testing exercises the analyzer end-to-end on whole projects through ``scan-build``.

All of the infrastructure lives in ``clang/utils/analyzer/`` and is driven through
a single entry point, ``SATest.py``.

Prerequisites
=============

- A build of Clang that includes both ``clang`` and the ``scan-build`` tool. A
  normal Clang build stages ``scan-build`` into your build's ``bin`` directory
  (target ``scan-build``), next to ``clang``. Put that directory on your ``PATH``
  so both tools are found:

  .. code-block:: bash

     export PATH="/path/to/llvm-build/bin:$PATH"
     which clang scan-build     # both should resolve to your build

  ``scan-build`` runs the analyzer through the ``clang`` given by the ``CC``
  environment variable, falling back to the first ``clang`` on ``PATH``. Make sure
  this is your freshly built compiler and not the system one; set ``CC`` explicitly
  if in doubt:

  .. code-block:: bash

     export CC=/path/to/llvm-build/bin/clang

- The usual tools needed to build the projects themselves. Projects are compiled
  from source through ``scan-build``, so their build systems must be available on
  ``PATH`` (for example ``cmake`` and ``make``/``ninja``). A missing tool surfaces
  as a ``scan-build`` failure such as ``command not found`` or ``Can't exec``.

- The Python dependencies used by the scripts:

  .. code-block:: bash

     pip install -r clang/utils/analyzer/requirements.txt

  This pulls in ``pandas``, ``matplotlib``, ``seaborn``, ``graphviz``, ``psutil``,
  and ``humanize`` (the last few are only needed for benchmarking and plotting).

- The scripts are meant to be run from the ``projects/`` directory, which holds
  the project map file ``projects.json`` and one subdirectory per project:

  .. code-block:: bash

     cd clang/utils/analyzer/projects

  The examples below assume this working directory and refer to the driver as
  ``../SATest.py``.

The ``SATest.py`` subcommands
=============================

``SATest.py`` is a thin dispatcher: each subcommand forwards to a sibling module.
Run ``../SATest.py --help`` or ``../SATest.py <subcommand> --help`` for the
authoritative list of options.

build
-----

The main workhorse. It builds and analyzes the projects and compares the produced
reports against the checked-in reference results.

.. code-block:: bash
   :caption: Run the whole suite with 8 workers, verbosely.

   ../SATest.py build -j8 -v

Useful options:

- ``-j``/``--jobs`` — number of projects to analyze concurrently
  (``0``/``1`` runs single-threaded).
- ``--projects`` — comma-separated list of project names to restrict the run to.
- ``--max-size`` — skip projects larger than the given size (see `Project size`_).
- ``--extra-checkers`` — comma-separated checkers to enable on top of the default
  set.
- ``--extra-analyzer-config`` — extra ``-analyzer-config`` options, forwarded to
  the analyzer.
- ``--override-compiler`` — pass ``--override-compiler`` to ``scan-build``.
- ``-r``/``--regenerate`` — treat this run as a reference build, regenerating the
  reference results in place instead of comparing.
- ``--strictness`` — the pass/fail criterion:

  - ``0`` (default) — fail only on crashes or analyzer failures.
  - ``1`` — additionally fail if the *number* of reported bugs differs from the
    reference.
  - ``2`` — fail on *any* difference from the reference.

.. code-block:: bash
   :caption: Analyze only two projects and try an experimental configuration.

   ../SATest.py build --projects=zstd,tmux \
                      --max-size=small \
                      --extra-checkers=alpha.core.CastToStruct \
                      --extra-analyzer-config=widen-loops=true \
                      --strictness=2

On failure the command exits with status ``42`` and prints a summary of the first
few analysis failures.

compare
-------

Compare two ``scan-build`` result directories in terms of reported warnings and
execution-time statistics. It reports which diagnostics were added, removed, or
changed between the two runs.

.. code-block:: bash

   ../SATest.py compare --show-stats path/to/old_results path/to/new_results

Notable options: ``--show-stats``/``--stats-only`` control statistics output,
``--histogram`` renders a histogram of path-length differences (requires
``matplotlib``), ``--root-old``/``--root-new`` strip a common path prefix from
source files, and ``--verbose-log`` writes extra detail to a log file.

update
------

Regenerate the checked-in reference results after an *intended* change to the
analyzer. It assumes ``SATest.py build`` was just run, then replaces the reference
results with the freshly produced ones for every project in the map.

.. code-block:: bash

   ../SATest.py build              # produce new results
   ../SATest.py update --git       # adopt them as the new reference and git-stage

With ``--git``, the old results are removed and the new ones added via ``git`` so
the update is ready to commit.

add
---

Register a new project for testing. It builds the project once to generate its
initial reference output and appends an entry to ``projects.json``. The project
directory must already exist inside ``projects/`` and be named after the project
(see `Anatomy of a project`_).

.. code-block:: bash

   ../SATest.py add myproject --mode 1 \
                              --source git \
                              --origin https://github.com/example/myproject.git \
                              --commit 0123abc

- ``--mode`` — build mode: ``0`` single file, ``1`` ``scan-build`` (default),
  ``2`` single-file C++11.
- ``--source`` — how the source is obtained: ``script`` (default), ``git``, or
  ``zip``. ``--origin`` and ``--commit`` are required (and only valid) for ``git``.

benchmark
---------

Benchmark the analyzer by building a set of projects multiple times and recording
execution time and peak memory usage into a CSV file.

.. code-block:: bash

   ../SATest.py benchmark -i20 --projects=zstd -o before.csv

``-i``/``--iterations`` controls how many times each project is analyzed (default
``20``), ``-o``/``--output`` names the CSV, and ``--projects``/``--max-size`` filter
the project set as with ``build``.

Two CSV files (for example, one before and one after a change) can be compared and
plotted with the ``benchmark compare`` subcommand:

.. code-block:: bash

   ../SATest.py benchmark -i20 --projects=zstd -o after.csv
   ../SATest.py benchmark compare --old before.csv --new after.csv -o plots.png

This produces box plots of normalized time and memory (old vs. new), which requires
``pandas``, ``matplotlib``, and ``seaborn``.

docker
------

Run the whole infrastructure inside a reproducible Ubuntu container, which builds
LLVM/clang from a mounted source tree and then invokes ``SATest.py``. This avoids
host-dependency drift.

.. code-block:: bash

   ../SATest.py docker --build-image                    # build the image once
   ../SATest.py docker --build-dir /path/to/build -- build -j8
   ../SATest.py docker --shell                          # interactive shell

Arguments after ``--`` are forwarded verbatim to the in-container entrypoint (which
first builds LLVM if requested, then runs ``SATest.py``).

Anatomy of a project
====================

Each project lives in its own directory under ``projects/`` whose name matches the
project's ``name`` in ``projects.json``. The following files set up how the project
is fetched, built, and analyzed (all are optional except a way to obtain the
source and, for ``scan-build`` mode, the build script):

- ``download_project.sh`` — downloads the source into a ``CachedSource/``
  directory. Only used for the ``script`` source type, and only when
  ``CachedSource/`` does not already exist.
- ``CachedSource/`` — an optional committed copy of the source. If present, the
  download step is skipped.
- ``run_static_analyzer.cmd`` — the list of build commands to run through
  ``scan-build`` (one per line, e.g. ``configure``, ``make``, ``xcodebuild``).
  A line of ``#NOPREFIX`` switches off the ``scan-build`` prefix for the following
  commands (the project then invokes the analyzer itself, using the ``CC``,
  ``OUTPUT``, and ``ANALYZER_CONFIG`` environment variables the harness provides).
  ``make`` commands automatically get a ``-jN`` argument.
- ``cleanup_run_static_analyzer.sh`` — prepares/cleans the build environment
  before analysis (e.g. ``make clean``).
- ``changes_for_analyzer.patch`` — an optional patch applied to a fresh copy of the
  source (``PatchedSource/``) before analysis, for instance to adapt the project to
  a newer clang. Analysis always runs on this patched copy.
- Reference results — the expected ``scan-build`` output, checked into the repo and
  compared against on every ``build`` run.

At analysis time the harness copies ``CachedSource/`` to ``PatchedSource/``, applies
the patch if any, runs the cleanup script, then runs ``scan-build`` from within
``PatchedSource/``. Results are written to ``ScanBuildResults/`` (or
``RefScanBuildResults/`` for reference builds).

The project map: ``projects.json``
-----------------------------------

``projects.json`` is a list of project descriptors parsed by ``ProjectMap.py`` into
``ProjectInfo`` records. A typical (git-sourced) entry looks like:

.. code-block:: json

   {
     "name": "zstd",
     "mode": 1,
     "source": "git",
     "origin": "https://github.com/facebook/zstd.git",
     "commit": "2af4e073",
     "size": "small"
   }

Fields: ``name`` and ``mode`` are required; ``source`` is ``git``/``zip``/
``script``; ``origin`` and ``commit`` are required for ``git`` sources; ``enabled``
(default ``true``) can disable a project; and ``size`` classifies the project (see
below). Fields left at their default value are omitted when the file is
(re)written.

.. _Project size:

Project size
------------

The ``size`` field and the ``--max-size`` filter classify projects by *analysis
time*, not by lines of code:

===========  ================
Size         Approximate time
===========  ================
``tiny``     < 1 min
``small``    1 – 10 min
``big``      10 min – 1 h
``huge``     > 1 h
===========  ================

A project with an unspecified size is filtered out by any ``--max-size`` value, so
give new projects a size if you want them included in size-limited runs.

How it works internally
=======================

``SATest.py`` merely parses arguments and delegates to sibling modules:

- **SATestBuild.py** — the core engine. ``RegressionTester`` orchestrates all
  selected projects (single-threaded, or one worker thread per job), and
  ``ProjectTester`` handles a single project: it downloads/patches the source, runs
  ``scan-build`` according to ``run_static_analyzer.cmd`` (or, for modes ``0``/``2``,
  analyzes preprocessed single files directly), collects the ``.plist`` reports,
  drops empty ones, and compares against the reference. A fixed set of default
  checkers is analyzed (``core``, ``cplusplus``, ``deadcode``, ``security``,
  ``unix``, ``osx``, ``nullability``, and a few alpha checkers); the
  ``SA_ADDITIONAL_CHECKERS`` environment variable and ``--extra-checkers`` add more.

- **ProjectMap.py** — the data model for ``projects.json`` (``ProjectInfo``,
  ``Size``, ``DownloadType``), including loading, validation, and serialization.

- **CmpRuns.py** — compares two runs. It loads the diagnostics from each run and
  builds a relation between them, yielding ``(old, new, confidence)`` triples that
  classify each diagnostic as added, removed, or common. It is used both by the
  ``compare`` subcommand and by ``build`` when diffing against the reference, and is
  also usable as a library.

- **SATestUpdateDiffs.py** — implements ``update``: for each project it swaps the
  reference results for the freshly generated ones, normalizes them (making paths
  relative and stripping transient fields), and optionally stages the result with
  ``git add``.

- **SATestBenchmark.py** — implements ``benchmark``: repeatedly analyzes each
  project, records time/memory into a ``pandas`` DataFrame, and (for ``compare``)
  renders normalized ``seaborn`` box plots.

- **entrypoint.py** / **Dockerfile** — the container path. ``entrypoint.py`` builds
  LLVM/clang inside the image (if requested) and then invokes ``SATest.py`` on the
  mounted project tree.

Reference results are normalized before being stored: source paths are made
relative and run-to-run transient fields (such as the clang version and HTML
diagnostic file lists) are removed, so that comparisons are stable across machines.

Related pages
=============

- :doc:`PerformanceInvestigation` — profiling a single analysis run once
  benchmarking points at a slowdown.
- :doc:`Statistics` — the per-translation-unit and per-entry-point statistics the
  analyzer can emit.
