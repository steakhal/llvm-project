from __future__ import annotations

import json
import os
import tempfile
import unittest

from aqb.run import materialize_corpus, perform_run
from aqb.runtime import ProcResult, Runtime
from aqb.volume import ClangVolume


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


class PerformRunTest(unittest.TestCase):
    def _projects_src(self, root):
        os.makedirs(root)
        with open(os.path.join(root, "projects.json"), "w") as f:
            json.dump(
                [
                    {
                        "name": "zstd",
                        "mode": 1,
                        "source": "git",
                        "origin": "https://example/zstd.git",
                        "commit": "deadbeef",
                        "size": "small",
                    }
                ],
                f,
            )
        d = os.path.join(root, "zstd")
        os.makedirs(d)
        with open(os.path.join(d, "run_static_analyzer.cmd"), "w") as f:
            f.write("cmake .\n")

    def _fake_runtime(self, work_holder):
        """A Runtime whose runner fabricates the container's outputs: one
        per-TU entry-point CSV and one (empty) plist, under the staged corpus."""

        def runner(argv, capture=True):
            # The corpus host dir is the source of the `<host>:/projects` mount.
            projects_dir = None
            for i, a in enumerate(argv):
                if (
                    a == "-v"
                    and i + 1 < len(argv)
                    and argv[i + 1].endswith(":/projects")
                ):
                    projects_dir = argv[i + 1].split(":/projects")[0]
            assert projects_dir, f"no /projects mount in {argv}"
            work_holder.append(projects_dir)
            ep_dir = os.path.join(projects_dir, "aqb-entry-point-stats")
            os.makedirs(ep_dir, exist_ok=True)
            with open(os.path.join(ep_dir, "101.csv"), "w") as f:
                f.write("USR,File,DebugName\nu1,zstd/a.c,fn1\n")
            plist_dir = os.path.join(projects_dir, "zstd", "RefScanBuildResults", "r1")
            os.makedirs(plist_dir, exist_ok=True)
            open(os.path.join(plist_dir, "x.plist"), "w").close()
            return ProcResult(returncode=0, stdout="", stderr="")

        return Runtime("fake", runner=runner)

    def test_persists_a_functional_run(self):
        from aqb.normalize import Finding

        fake_finding = Finding(
            issue_id="id1",
            file="a.c",
            line=1,
            column=2,
            checker="core.Null",
            category="Logic error",
            description="boom",
            path_length=3,
        )

        with tempfile.TemporaryDirectory() as tmp:
            src = os.path.join(tmp, "projects")
            self._projects_src(src)
            home = os.path.join(tmp, "home")
            work_holder = []
            runtime = self._fake_runtime(work_holder)

            def fake_resolve_clang(rt, **kwargs):
                return ClangVolume(
                    name="aqb-clang-deadbeef-abc123", config_digest="abc123", built=True
                )

            run_path = perform_run(
                runtime=runtime,
                home=home,
                commit="deadbeef",
                source="https://example/llvm.git",
                projects_src=src,
                scripts_dir="/host/scripts",
                project_names=["zstd"],
                resolve_clang=fake_resolve_clang,
                load_findings_fn=lambda results_dir, project_root="": [fake_finding],
            )

            # metadata.json exists and carries analyzer/corpus provenance.
            with open(os.path.join(run_path, "metadata.json")) as f:
                meta = json.load(f)
            self.assertEqual(meta["analyzer"]["commit"], "deadbeef")
            self.assertEqual(meta["analyzer"]["volume"], "aqb-clang-deadbeef-abc123")
            self.assertEqual(meta["analyzer"]["config_digest"], "abc123")
            self.assertEqual(meta["kind"], "functional")
            self.assertEqual([p["name"] for p in meta["corpus"]], ["zstd"])

            # reports: the loaded findings are persisted.
            with open(os.path.join(run_path, "reports", "findings.json")) as f:
                reports = json.load(f)
            self.assertIn("zstd", reports)
            self.assertEqual(reports["zstd"][0]["issue_id"], "id1")

            # metrics: the merged per-TU CSV is persisted.
            csv_path = os.path.join(run_path, "metrics", "entry-point-stats.csv")
            self.assertTrue(os.path.isfile(csv_path))
            with open(csv_path) as f:
                lines = [ln for ln in f.read().splitlines() if ln]
            self.assertEqual(lines[0], "USR,File,DebugName")
            self.assertIn("u1,zstd/a.c,fn1", lines)


class BenchmarkRunTest(unittest.TestCase):
    def _projects_src(self, root):
        os.makedirs(root)
        with open(os.path.join(root, "projects.json"), "w") as f:
            json.dump(
                [
                    {
                        "name": "zstd",
                        "mode": 1,
                        "source": "git",
                        "origin": "o",
                        "commit": "c",
                    }
                ],
                f,
            )
        d = os.path.join(root, "zstd")
        os.makedirs(d)
        with open(os.path.join(d, "run_static_analyzer.cmd"), "w") as f:
            f.write("cmake .\n")

    def _bench_runtime(self):
        """A Runtime whose runner writes a per-iteration CSV into whatever
        iter-<i> dir the argv's AQB_EP_CSV_DIR points at, with a NumSteps value
        that varies by iteration."""
        counter = {"i": 0}

        def runner(argv, capture=True):
            ep_dir = None
            for a in argv:
                if a.startswith("AQB_EP_CSV_DIR="):
                    ep_dir = a.split("=", 1)[1]
            # Map the in-container /projects prefix back to the host work dir.
            projects_dir = None
            for i, a in enumerate(argv):
                if a == "-v" and argv[i + 1].endswith(":/projects"):
                    projects_dir = argv[i + 1].split(":/projects")[0]
            host_ep = ep_dir.replace("/projects", projects_dir, 1)
            os.makedirs(host_ep, exist_ok=True)
            steps = 10 + counter["i"] * 5  # 10, 15, 20, ...
            counter["i"] += 1
            with open(os.path.join(host_ep, "1.csv"), "w") as f:
                f.write("USR,File,DebugName,NumSteps\nu1,zstd/a.c,fn1,%d\n" % steps)
            return ProcResult(returncode=0, stdout="", stderr="")

        return Runtime("fake", runner=runner)

    def test_benchmark_stores_per_iteration_samples_and_no_reports(self):
        with tempfile.TemporaryDirectory() as tmp:
            src = os.path.join(tmp, "projects")
            self._projects_src(src)
            home = os.path.join(tmp, "home")

            def fake_resolve_clang(rt, **kwargs):
                return ClangVolume(name="v", config_digest="d", built=False)

            run_path = perform_run(
                runtime=self._bench_runtime(),
                home=home,
                commit="c",
                source="/s",
                projects_src=src,
                scripts_dir="/host/scripts",
                project_names=["zstd"],
                kind="benchmark",
                iterations=3,
                resolve_clang=fake_resolve_clang,
            )

            # No reports for a benchmark run.
            self.assertFalse(
                os.path.exists(os.path.join(run_path, "reports", "findings.json"))
            )
            with open(os.path.join(run_path, "metrics", "samples.json")) as f:
                data = json.load(f)
            self.assertEqual(data["iterations"], 3)
            self.assertEqual(data["metrics"], ["NumSteps"])
            self.assertEqual(len(data["samples"]), 3)
            # Each iteration captured the single entry point with its NumSteps.
            steps = [it[0]["stats"]["NumSteps"] for it in data["samples"]]
            self.assertEqual(steps, [10, 15, 20])
            # Metadata records the iteration count and benchmark kind.
            with open(os.path.join(run_path, "metadata.json")) as f:
                meta = json.load(f)
            self.assertEqual(meta["kind"], "benchmark")
            self.assertEqual(meta["execution"]["n"], 3)


if __name__ == "__main__":
    unittest.main()
