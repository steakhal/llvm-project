from __future__ import annotations

import json
import os
import tempfile
import unittest

from aqb._satest import ProjectMap
from aqb.corpus import select_projects

PROJECTS = [
    {
        "name": "curl",
        "mode": 1,
        "source": "git",
        "origin": "https://x/curl.git",
        "commit": "aaaa",
        "size": "small",
    },
    {
        "name": "redis",
        "mode": 1,
        "source": "git",
        "origin": "https://x/redis.git",
        "commit": "bbbb",
        "size": "tiny",
        "enabled": False,
    },
    {
        "name": "box2d",
        "mode": 1,
        "source": "git",
        "origin": "https://x/box2d.git",
        "commit": "cccc",
        "size": "tiny",
    },
]


def _load_map(root: str) -> "ProjectMap.ProjectMap":
    path = os.path.join(root, "projects.json")
    with open(path, "w") as handle:
        json.dump(PROJECTS, handle)
    return ProjectMap.ProjectMap(path=path)


class SelectProjectsTest(unittest.TestCase):
    def test_drops_disabled_by_default(self):
        with tempfile.TemporaryDirectory() as root:
            names = [p.name for p in select_projects(_load_map(root))]
            self.assertEqual(names, ["curl", "box2d"])

    def test_include_disabled(self):
        with tempfile.TemporaryDirectory() as root:
            names = [
                p.name for p in select_projects(_load_map(root), include_disabled=True)
            ]
            self.assertEqual(names, ["curl", "redis", "box2d"])

    def test_filter_by_name(self):
        with tempfile.TemporaryDirectory() as root:
            names = [p.name for p in select_projects(_load_map(root), names=["curl"])]
            self.assertEqual(names, ["curl"])

    def test_filter_by_size_respects_enabled(self):
        with tempfile.TemporaryDirectory() as root:
            selected = select_projects(_load_map(root), sizes=[ProjectMap.Size.TINY])
            self.assertEqual([p.name for p in selected], ["box2d"])
