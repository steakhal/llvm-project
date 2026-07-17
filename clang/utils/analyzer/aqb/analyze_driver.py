#!/usr/bin/env python3
"""AQB analyze driver — runs *inside* the analyze container.

AQB deliberately does NOT shell out to ``SATest.py build``: that path runs
``RegressionTester.test_all`` which pairs analysis with SATest's own
reference-*compare* (and picks the output dir) based on a ``regenerate`` flag
threaded through positional constructor args. AQB only wants the analysis half
(produce plists + stats), never the compare/verdict — AQB diffs runs itself.

So this driver reuses the *lower* SATest seam that ``SATestAdd`` uses for a
brand-new project: ``ProjectTester(TestInfo(is_reference_build=True)).test()``.
A reference build structurally (a) writes to ``RefScanBuildResults`` and
(b) skips ``run_cmp_results`` entirely — exactly AQB's contract, with no
dependence on ``build``'s flag wiring.

Usage (from the corpus root, i.e. container ``-w /projects``):
    python3 /scripts/aqb/analyze_driver.py \
        --projects zstd,curl \
        --extra-analyzer-config dump-entry-point-stats-to-csv=/projects/ep.csv

Exit code is non-zero if any project's analysis raised (build/check failure);
a reference build itself never "fails a comparison", so a clean analyze is 0.
"""
from __future__ import annotations

import argparse
import os
import sys


def _add_scripts_to_path() -> None:
    """Ensure the sibling SATest modules (SATestBuild, ProjectMap) import.

    This file lives at ``<analyzer>/aqb/analyze_driver.py``; its parent's
    parent is the analyzer dir that holds ``SATestBuild.py``/``ProjectMap.py``
    (mounted at ``/scripts`` in the container).
    """
    analyzer_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    if analyzer_dir not in sys.path:
        sys.path.insert(0, analyzer_dir)


def _select_projects(project_map, names):
    """Return the ProjectInfo list to analyze, enabled and validated.

    Mirrors ``SATest.py``'s ``get_projects`` selection: when ``names`` is
    given, keep only those (force-enabled so a disabled entry still runs);
    otherwise keep the map's already-enabled projects.
    """
    projects = project_map.projects
    if not names:
        return [p for p in projects if p.enabled]

    available = {p.name for p in projects}
    missing = [n for n in names if n not in available]
    if missing:
        raise SystemExit(
            f"error: unknown project(s) {missing}; available: {sorted(available)}"
        )
    return [p.with_fields(enabled=True) for p in projects if p.name in set(names)]


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description="AQB analyze driver (reference build)")
    parser.add_argument(
        "--projects",
        default="",
        help="Comma-separated project names; empty means all enabled projects.",
    )
    parser.add_argument(
        "--extra-analyzer-config",
        dest="extra_analyzer_config",
        default="",
        help="Passed through to -analyzer-config (on top of SATest defaults).",
    )
    parser.add_argument(
        "--extra-checkers",
        dest="extra_checkers",
        default="",
        help="Extra checkers to enable.",
    )
    args = parser.parse_args(argv)

    _add_scripts_to_path()
    import SATestBuild  # noqa: E402  (path set up above)
    from ProjectMap import ProjectMap  # noqa: E402

    names = [n for n in args.projects.split(",") if n]
    project_map = ProjectMap()
    projects = _select_projects(project_map, names)

    ok = True
    for project in projects:
        test_info = SATestBuild.TestInfo(
            project,
            extra_analyzer_config=args.extra_analyzer_config,
            extra_checkers=args.extra_checkers,
            is_reference_build=True,  # analyze-only: RefScanBuildResults, no compare
        )
        tester = SATestBuild.ProjectTester(test_info)
        # ProjectTester.test() returns True for a reference build unless the
        # build/check itself failed (it raises on hard failures).
        ok = tester.test() and ok

    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
