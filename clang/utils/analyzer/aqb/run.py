from __future__ import annotations

import os
import shutil
from typing import List, Sequence


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
