from __future__ import annotations

from typing import List, Optional, Sequence

from aqb._satest import ProjectMap


def select_projects(
    project_map: "ProjectMap.ProjectMap",
    names: Optional[Sequence[str]] = None,
    sizes: Optional[Sequence["ProjectMap.Size"]] = None,
    include_disabled: bool = False,
) -> List["ProjectMap.ProjectInfo"]:
    """Select projects from a loaded ProjectMap, preserving map order.

    - ``names``: if given, keep only projects whose name is in this set.
    - ``sizes``: if given, keep only projects whose size is in this set.
    - ``include_disabled``: unless True, projects with ``enabled=False`` are
      dropped.
    """
    name_set = set(names) if names is not None else None
    size_set = set(sizes) if sizes is not None else None

    selected: List["ProjectMap.ProjectInfo"] = []
    for project in project_map.projects:
        if not include_disabled and not project.enabled:
            continue
        if name_set is not None and project.name not in name_set:
            continue
        if size_set is not None and project.size not in size_set:
            continue
        selected.append(project)
    return selected
