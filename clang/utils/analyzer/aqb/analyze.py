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

EP_CSV_NAME = "entry-point-stats.csv"
# `SATest.py build --regenerate` writes results here (vs. "ScanBuildResults"
# for a comparison run); AQB uses --regenerate to analyze without SATest's own
# compare/verdict (AQB diffs runs itself in a later phase).
SCAN_BUILD_RESULTS_DIR = "RefScanBuildResults"


def analyzer_config(ep_csv_path: str, extra: str = "") -> str:
    """The extra ``-analyzer-config`` string AQB passes to SATest via
    ``--extra-analyzer-config`` (on top of SATest's own ``serialize-stats=true``):
    enable the per-entry-point CSV dump, plus any caller-supplied options.
    """
    parts: List[str] = [f"dump-entry-point-stats-to-csv={ep_csv_path}"]
    if extra:
        parts.append(extra)
    return ",".join(parts)


def collect_plists(projects_root: str) -> List[str]:
    """All analyzer plist reports produced under
    ``<projects_root>/<project>/RefScanBuildResults/*/*.plist`` (SATest's layout).
    """
    pattern = os.path.join(projects_root, "*", SCAN_BUILD_RESULTS_DIR, "*", "*.plist")
    return sorted(glob.glob(pattern))


def collect_entry_point_csvs(root: str) -> List[str]:
    """All per-entry-point stat CSVs written under ``root`` (recursively)."""
    return sorted(glob.glob(os.path.join(root, "**", EP_CSV_NAME), recursive=True))


def analyze_run_argv(
    *,
    clang_volume: str,
    projects_dir: str,
    scripts_dir: str,
    ccache_volume: str,
    image: str,
    projects: List[str],
    ep_csv_path: str,
    memory: str = "24G",
    cpus: str = "8",
    extra_config: str = "",
) -> List[str]:
    """DRAFT (to be confirmed by the Phase-3c daemon-gated prototype).

    A container ``run`` that analyzes ``projects`` with the Clang Volume's clang
    by reusing SATest: the volume mounts ``:ro`` at ``/analyzer`` (so scan-build
    and ``--use-analyzer /analyzer/bin/clang`` resolve via ``PATH``), the corpus
    at ``/projects`` (results land there), and ``SATest.py`` at ``/scripts``.
    ``SATest.py build --regenerate`` analyzes without SATest's compare;
    ``--extra-analyzer-config`` injects the entry-point CSV dump.

    The exact SATest invocation, ``-w``/env handling, and per-TU CSV aggregation
    are validated (and likely refined) by a real run before this is relied on.
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
        "-e",
        f"CC={ANALYZER_MOUNT}/bin/clang",
        "-e",
        f"CCACHE_DIR={CCACHE_MOUNT}",
        "--entrypoint",
        "python3",
        image,
        f"{SCRIPTS_MOUNT}/SATest.py",
        "build",
        "-r",  # regenerate: analyze and write results without SATest's compare
        "--projects",
        ",".join(projects),
        "--extra-analyzer-config",
        analyzer_config(ep_csv_path, extra_config),
    ]
    return args
