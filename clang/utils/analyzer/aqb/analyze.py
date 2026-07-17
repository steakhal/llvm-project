from __future__ import annotations

import glob
import os
from typing import List

# Mount contract for the analyze container. The Clang Volume mounts at
# ``/analyzer`` so SATest's existing ``PATH=/analyzer/bin`` +
# ``scan-build --use-analyzer /analyzer/bin/clang`` contract works unchanged.
ANALYZER_MOUNT = "/analyzer"
PROJECTS_MOUNT = "/projects"  # corpus recipes; results land under here
SCRIPTS_MOUNT = "/scripts"  # SATest.py and siblings
CCACHE_MOUNT = "/ccache"

EP_CSV_DIR_NAME = "aqb-entry-point-stats"
# AQB's analyze driver writes results here (SATest's "reference build" layout),
# NOT "ScanBuildResults". AQB drives ``ProjectTester(is_reference_build=True)``
# directly (via analyze_driver.py) so analysis never triggers SATest's own
# reference-compare/verdict — AQB diffs runs itself in a later phase.
SCAN_BUILD_RESULTS_DIR = "RefScanBuildResults"
# The in-container path to AQB's analyze driver (SCRIPTS_MOUNT is the analyzer
# dir; the driver lives at aqb/analyze_driver.py beside SATestBuild.py).
ANALYZE_DRIVER = "aqb/analyze_driver.py"
# The analyzer-clang wrapper (also under SCRIPTS_MOUNT). Set as ``CC`` so every
# per-TU clang analysis process writes a PID-unique entry-point CSV under
# ``AQB_EP_CSV_DIR`` — avoiding the single-shared-path clobber.
WRAPPER = "aqb/clang-analyzer-wrapper.sh"


def collect_plists(projects_root: str) -> List[str]:
    """All analyzer plist reports produced under
    ``<projects_root>/<project>/RefScanBuildResults/*/*.plist`` (SATest's layout).
    """
    pattern = os.path.join(projects_root, "*", SCAN_BUILD_RESULTS_DIR, "*", "*.plist")
    return sorted(glob.glob(pattern))


def collect_entry_point_csvs(root: str) -> List[str]:
    """The per-TU entry-point stat CSVs the wrapper wrote (``<pid>.csv``) under
    ``<root>/aqb-entry-point-stats/``. Merge them with ``merge_entry_point_csvs``.
    """
    return sorted(glob.glob(os.path.join(root, EP_CSV_DIR_NAME, "*.csv")))


def merge_entry_point_csvs(csv_paths: List[str]) -> List[str]:
    """Merge per-TU entry-point CSVs (identical headers) into one line list:
    a single header followed by the sorted-unique union of data rows. Empty or
    header-only files contribute nothing. Raises ValueError if headers disagree.
    """
    header = ""
    rows = set()
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


def analyze_run_argv(
    *,
    clang_volume: str,
    projects_dir: str,
    scripts_dir: str,
    ccache_volume: str,
    image: str,
    projects: List[str],
    memory: str = "24G",
    cpus: str = "8",
    extra_config: str = "",
) -> List[str]:
    """A container ``run`` that analyzes ``projects`` with the Clang Volume's
    clang by reusing SATest's project-recipe machinery. The volume mounts
    ``:ro`` at ``/analyzer``, the corpus at ``/projects`` (results land there),
    and the analyzer dir (SATest modules + AQB's driver + wrapper) at
    ``/scripts``.

    AQB invokes its own ``analyze_driver.py`` — which drives
    ``ProjectTester(is_reference_build=True).test()`` — rather than ``SATest.py
    build``: that gives the analyze half (plists into ``RefScanBuildResults``)
    with no reference-compare/verdict.

    Entry-point stats: ``CC`` points at ``clang-analyzer-wrapper.sh`` (which
    SATest uses as ``--use-analyzer``), so every per-TU clang analysis writes a
    PID-unique CSV under ``AQB_EP_CSV_DIR`` (no single-path clobber). The real
    clang is ``AQB_REAL_CLANG``. Collect + ``merge_entry_point_csvs`` afterward.
    """
    args = ["run", "--rm", "-w", PROJECTS_MOUNT]
    if memory:
        args += ["-m", memory]
    if cpus:
        args += ["--cpus", cpus]
    args += [
        "-v",
        f"{clang_volume}:{ANALYZER_MOUNT}:ro",
        "-v",
        f"{projects_dir}:{PROJECTS_MOUNT}",
        "-v",
        f"{scripts_dir}:{SCRIPTS_MOUNT}:ro",
        "-v",
        f"{ccache_volume}:{CCACHE_MOUNT}",
        "-e",
        f"PATH={ANALYZER_MOUNT}/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin",
        # CC is the analyzer clang SATest uses (CLANG = os.environ["CC"]); point
        # it at the wrapper so per-TU CSVs are unique. The wrapper execs the
        # real clang named by AQB_REAL_CLANG.
        "-e",
        f"CC={SCRIPTS_MOUNT}/{WRAPPER}",
        "-e",
        f"AQB_REAL_CLANG={ANALYZER_MOUNT}/bin/clang",
        "-e",
        f"AQB_EP_CSV_DIR={PROJECTS_MOUNT}/{EP_CSV_DIR_NAME}",
        "-e",
        f"CCACHE_DIR={CCACHE_MOUNT}",
        "--entrypoint",
        "python3",
        image,
        f"{SCRIPTS_MOUNT}/{ANALYZE_DRIVER}",
        "--projects",
        ",".join(projects),
        "--extra-analyzer-config",
        extra_config,
    ]
    return args
