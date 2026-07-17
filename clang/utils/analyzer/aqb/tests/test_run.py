from __future__ import annotations

import json
import os
import tempfile
import unittest

from aqb.run import materialize_corpus


class MaterializeCorpusTest(unittest.TestCase):
    def _fake_projects_src(self, root):
        """Build a fake SATest projects/ tree: projects.json + recipe dirs."""
        os.makedirs(root)
        projects = [
            {"name": "zstd", "mode": 1, "source": "git", "origin": "u", "commit": "c"},
            {"name": "fmt", "mode": 1, "source": "git", "origin": "u", "commit": "c"},
        ]
        with open(os.path.join(root, "projects.json"), "w") as f:
            json.dump(projects, f)
        for name in ("zstd", "fmt"):
            d = os.path.join(root, name)
            os.makedirs(d)
            with open(os.path.join(d, "run_static_analyzer.cmd"), "w") as f:
                f.write("cmake .\n")

    def test_stages_projects_json_and_selected_recipes(self):
        with tempfile.TemporaryDirectory() as tmp:
            src = os.path.join(tmp, "projects")
            dest = os.path.join(tmp, "work")
            self._fake_projects_src(src)

            materialize_corpus(src, ["zstd"], dest)

            # projects.json is copied so the in-container ProjectMap() resolves.
            self.assertTrue(os.path.isfile(os.path.join(dest, "projects.json")))
            # The selected recipe dir is staged...
            self.assertTrue(
                os.path.isfile(os.path.join(dest, "zstd", "run_static_analyzer.cmd"))
            )
            # ...but unselected projects are NOT staged.
            self.assertFalse(os.path.exists(os.path.join(dest, "fmt")))

    def test_empty_selection_stages_all_recipes(self):
        with tempfile.TemporaryDirectory() as tmp:
            src = os.path.join(tmp, "projects")
            dest = os.path.join(tmp, "work")
            self._fake_projects_src(src)

            materialize_corpus(src, [], dest)

            self.assertTrue(os.path.isdir(os.path.join(dest, "zstd")))
            self.assertTrue(os.path.isdir(os.path.join(dest, "fmt")))

    def test_unknown_project_raises(self):
        with tempfile.TemporaryDirectory() as tmp:
            src = os.path.join(tmp, "projects")
            dest = os.path.join(tmp, "work")
            self._fake_projects_src(src)
            with self.assertRaises(FileNotFoundError):
                materialize_corpus(src, ["nope"], dest)


if __name__ == "__main__":
    unittest.main()
