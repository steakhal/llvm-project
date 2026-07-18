from __future__ import annotations

import dataclasses
import datetime
import glob
import json
import os
import shutil
from typing import Callable, List, Optional, Sequence

from aqb._satest import ProjectMap
from aqb.analyze import (
    EP_CSV_DIR_NAME,
    PROJECTS_MOUNT,
    SCAN_BUILD_RESULTS_DIR,
    analyze_run_argv,
    collect_entry_point_csvs,
    merge_entry_point_csvs,
)
from aqb.corpus import select_projects
from aqb.metadata import (
    AnalyzerProvenance,
    ContainerProvenance,
    ExecutionProvenance,
    Metadata,
    ProjectProvenance,
)
from aqb.metrics import dedup_entry_points, parse_entry_point_csv, to_sample_records
from aqb.normalize import Finding, load_findings
from aqb.runtime import Runtime
from aqb.runid import new_run_id
from aqb.store import RunStore
from aqb.volume import CCACHE_VOLUME, build_clang_volume


def materialize_corpus(
    projects_src: str, project_names: Sequence[str], dest: str
) -> List[str]:
    """Stage a writable corpus dir AQB owns and bind-mounts at ``/projects``.

    SATest keeps each project's analyze recipe at
    ``clang/utils/analyzer/projects/<name>/`` (``run_static_analyzer.cmd`` etc.)
    alongside ``projects.json``. The container's ``ProjectMap()`` reads
    ``projects.json`` from the working dir, and each project's recipe dir must be
    present for analysis. We copy ``projects.json`` plus the selected recipe dirs
    into ``dest`` (created fresh); the analyze run then clones sources and writes
    ``RefScanBuildResults/`` + ``aqb-entry-point-stats/`` there.

    ``project_names`` empty means stage every recipe dir referenced by
    ``projects.json``. Returns the staged project names. Raises
    ``FileNotFoundError`` if a named project has no recipe dir.
    """
    os.makedirs(dest, exist_ok=True)
    shutil.copy2(
        os.path.join(projects_src, "projects.json"),
        os.path.join(dest, "projects.json"),
    )

    if project_names:
        names = list(project_names)
    else:
        names = [
            entry
            for entry in sorted(os.listdir(projects_src))
            if os.path.isdir(os.path.join(projects_src, entry))
        ]

    staged: List[str] = []
    for name in names:
        recipe = os.path.join(projects_src, name)
        if not os.path.isdir(recipe):
            raise FileNotFoundError(
                f"no recipe dir for project {name!r} in {projects_src!r}"
            )
        shutil.copytree(recipe, os.path.join(dest, name), dirs_exist_ok=True)
        staged.append(name)
    return staged


def _project_source(info) -> str:
    """A human/provenance source string for a project: its git origin when it
    is a git project, else its download-type name."""
    origin = getattr(info, "origin", "") or ""
    return origin if origin else getattr(info.source, "value", str(info.source))


def perform_run(
    *,
    runtime: Runtime,
    home: str,
    commit: str,
    source: str,
    projects_src: str,
    scripts_dir: str,
    project_names: Sequence[str] = (),
    sizes: Optional[Sequence] = None,
    commit_title: str = "",
    preset: str = "aqb-base",
    user_overlay_json: Optional[str] = None,
    builder_image: str = "aqb-clang-builder:latest",
    memory: str = "20G",
    cpus: str = "8",
    extra_config: str = "",
    kind: str = "functional",
    iterations: int = 1,
    now: Optional[datetime.datetime] = None,
    resolve_clang: Callable[..., "object"] = build_clang_volume,
    load_findings_fn: Callable[..., List[Finding]] = load_findings,
) -> str:
    """Perform a run: resolve the Clang Volume, analyze the pinned corpus with
    it, and persist a run.

    - **functional** (``kind="functional"``): one analyze pass; persist
      deduplicated reports (``reports/findings.json``) + the merged entry-point
      metrics (``metrics/entry-point-stats.csv``).
    - **benchmark** (``kind="benchmark"``): run ``iterations`` analyze passes
      into per-iteration CSV dirs; persist the raw per-iteration, per-entry-point
      samples (``metrics/samples.json``). Reports are irrelevant to a benchmark
      and are not stored.

    Returns the created run directory path. ``resolve_clang`` and
    ``load_findings_fn`` are injectable so the orchestration is unit-testable
    without a container daemon or real plist parsing.
    """
    now = now or datetime.datetime.now(datetime.timezone.utc)
    created = now.isoformat()
    run_id = new_run_id(now=now)
    is_bench = kind == "benchmark"
    n_iter = iterations if is_bench else 1

    # Which projects (pinned ProjectInfo, from the in-tree map).
    project_map = ProjectMap.ProjectMap(os.path.join(projects_src, "projects.json"))
    selected = select_projects(
        project_map, names=list(project_names) or None, sizes=sizes
    )
    names = [p.name for p in selected]

    # Resolve (or build) the analyzer.
    volume = resolve_clang(
        runtime,
        commit=commit,
        source=source,
        commit_title=commit_title,
        preset=preset,
        user_overlay_json=user_overlay_json,
        builder_image=builder_image,
        created=created,
        memory=memory,
        cpus=cpus,
    )

    # Stage a writable corpus once; analyze it (once, or N times for a benchmark).
    work_dir = os.path.join(home, "work", run_id)
    materialize_corpus(projects_src, names, work_dir)

    def _analyze(ep_csv_dir: str = "") -> None:
        argv = analyze_run_argv(
            clang_volume=volume.name,
            projects_dir=work_dir,
            scripts_dir=scripts_dir,
            ccache_volume=CCACHE_VOLUME,
            image=builder_image,
            projects=names,
            memory=memory,
            cpus=cpus,
            extra_config=extra_config,
            ep_csv_dir=ep_csv_dir,
        )
        runtime.run(argv, check=True, capture=False)

    reports = {}
    samples: List[List[dict]] = []
    if is_bench:
        for i in range(n_iter):
            _analyze(ep_csv_dir=f"{PROJECTS_MOUNT}/{EP_CSV_DIR_NAME}/iter-{i}")
            iter_dir = os.path.join(work_dir, EP_CSV_DIR_NAME, f"iter-{i}")
            merged = merge_entry_point_csvs(sorted(glob.glob(f"{iter_dir}/*.csv")))
            df = dedup_entry_points(parse_entry_point_csv("\n".join(merged)))
            samples.append(to_sample_records(df))
    else:
        _analyze()
        for name in names:
            results_dir = os.path.join(work_dir, name, SCAN_BUILD_RESULTS_DIR)
            project_root = os.path.join(work_dir, name)
            findings = load_findings_fn(results_dir, project_root=project_root)
            reports[name] = [dataclasses.asdict(f) for f in findings]
        merged_csv = merge_entry_point_csvs(collect_entry_point_csvs(work_dir))

    # Provenance.
    image_digest = ""
    try:
        image_digest = runtime.image_id(builder_image)
    except Exception:
        pass
    by_name = {p.name: p for p in selected}
    metadata = Metadata(
        run_id=run_id,
        kind=kind,
        created=created,
        analyzer=AnalyzerProvenance(
            commit=commit,
            commit_title=commit_title,
            config_digest=getattr(volume, "config_digest", ""),
            volume=volume.name,
        ),
        container=ContainerProvenance(runtime=runtime.name, image_digest=image_digest),
        execution=ExecutionProvenance(
            n=n_iter, analyzer_args=[extra_config] if extra_config else []
        ),
        corpus=[
            ProjectProvenance(
                name=name,
                source=_project_source(by_name[name]),
                commit=getattr(by_name[name], "commit", "") or "",
            )
            for name in names
        ],
    )

    # Persist.
    store = RunStore(home)
    run_path = store.create_run(metadata)
    if is_bench:
        metric_names = sorted({m for it in samples for ep in it for m in ep["stats"]})
        with open(os.path.join(run_path, "metrics", "samples.json"), "w") as handle:
            json.dump(
                {"iterations": n_iter, "metrics": metric_names, "samples": samples},
                handle,
                indent=2,
                sort_keys=True,
            )
    else:
        with open(os.path.join(run_path, "reports", "findings.json"), "w") as handle:
            json.dump(reports, handle, indent=2, sort_keys=True)
        with open(
            os.path.join(run_path, "metrics", "entry-point-stats.csv"), "w"
        ) as handle:
            handle.write("\n".join(merged_csv) + ("\n" if merged_csv else ""))
    return run_path
