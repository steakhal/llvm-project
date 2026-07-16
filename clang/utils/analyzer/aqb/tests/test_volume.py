from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
import unittest
from typing import Callable, List

from aqb.errors import ClangBuildError, RuntimeCommandError
from aqb.runtime import ProcResult, Runtime
from aqb.volume import (
    CCACHE_VOLUME,
    ClangBuildSpec,
    build_clang_volume,
    build_config_digest,
    clang_volume_labels,
    clang_volume_name,
    ensure_cache_volume,
    resolve_or_build_clang,
)


class DigestTest(unittest.TestCase):
    _PRESETS = '{"version": 3, "configurePresets": [{"name": "aqb-base"}]}'

    def test_digest_is_stable(self):
        a = build_config_digest(self._PRESETS, "aqb-base", "sha256:img")
        b = build_config_digest(self._PRESETS, "aqb-base", "sha256:img")
        self.assertEqual(a, b)

    def test_digest_changes_with_preset_name(self):
        a = build_config_digest(self._PRESETS, "aqb-base", "sha256:img")
        b = build_config_digest(self._PRESETS, "other", "sha256:img")
        self.assertNotEqual(a, b)

    def test_digest_changes_with_presets_content(self):
        other = '{"version": 3, "configurePresets": [{"name": "aqb-base", "x": 1}]}'
        a = build_config_digest(self._PRESETS, "aqb-base", "sha256:img")
        b = build_config_digest(other, "aqb-base", "sha256:img")
        self.assertNotEqual(a, b)

    def test_digest_changes_with_builder_image(self):
        a = build_config_digest(self._PRESETS, "aqb-base", "sha256:one")
        b = build_config_digest(self._PRESETS, "aqb-base", "sha256:two")
        self.assertNotEqual(a, b)


class NameAndLabelTest(unittest.TestCase):
    def test_volume_name_uses_short_commit_and_digest(self):
        self.assertEqual(
            clang_volume_name("349146dabe4b07651d02fb", "cfg0a1b2c3d4"),
            "aqb-clang-349146dabe4b-cfg0a1b2c3d4",
        )

    def test_labels_carry_full_commit_role_and_created(self):
        labels = clang_volume_labels(
            commit="349146dabe4b07651d02fb",
            commit_title="do the thing",
            source="/work/llvm-project",
            build_config="cmake: -DA=1 (asserts)",
            builder_image_id="sha256:img",
            created="2026-07-16T13:15:00+00:00",
        )
        self.assertEqual(labels["aqb.role"], "clang")
        self.assertEqual(
            labels["aqb.commit"], "349146dabe4b07651d02fb"
        )  # full, not short
        self.assertEqual(labels["aqb.source"], "/work/llvm-project")
        self.assertEqual(labels["aqb.builder_image"], "sha256:img")
        self.assertEqual(labels["aqb.created"], "2026-07-16T13:15:00+00:00")


class ScriptedRunner:
    """Fake runner routing by argv; records every call."""

    def __init__(self, handler: Callable[[List[str]], ProcResult]):
        self.calls: List[List[str]] = []
        self.captures: List[bool] = []
        self._handler = handler

    def __call__(self, argv: List[str], capture: bool = True) -> ProcResult:
        self.calls.append(list(argv))
        self.captures.append(capture)
        return self._handler(argv)


def _spec() -> ClangBuildSpec:
    return ClangBuildSpec(
        commit="349146dabe4b07651d02fb",
        source="/work/llvm-project",
        commit_title="do the thing",
        preset="aqb-base",
        user_presets_json='{"version": 3, "configurePresets": [{"name": "aqb-base"}]}',
        builder_image="aqb-clang-builder:latest",
        builder_image_id="sha256:img",
        created="2026-07-16T13:15:00+00:00",
        build_config="preset=aqb-base",
    )


def _expected_name(spec: ClangBuildSpec) -> str:
    from aqb.volume import build_config_digest, clang_volume_name

    digest = build_config_digest(
        spec.user_presets_json, spec.preset, spec.builder_image_id
    )
    return clang_volume_name(spec.commit, digest)


class CacheVolumeTest(unittest.TestCase):
    def test_creates_cache_volume_when_absent(self):
        runner = ScriptedRunner(
            lambda argv: (
                ProcResult(1, "", "")
                if argv[1:3] == ["volume", "inspect"]
                else ProcResult(0, "", "")
            )
        )
        ensure_cache_volume(Runtime("docker", runner))
        create = [c for c in runner.calls if c[1:3] == ["volume", "create"]]
        self.assertEqual(len(create), 1)
        self.assertIn("aqb.role=cache", create[0])
        self.assertIn(CCACHE_VOLUME, create[0])

    def test_noop_when_cache_volume_present(self):
        runner = ScriptedRunner(lambda argv: ProcResult(0, "", ""))
        ensure_cache_volume(Runtime("docker", runner))
        self.assertFalse([c for c in runner.calls if c[1:3] == ["volume", "create"]])


class ResolveOrBuildTest(unittest.TestCase):
    @staticmethod
    def _build_runs(runner):
        # A builder *build* run (not the `test -e` completeness check).
        return [c for c in runner.calls if c[1:2] == ["run"] and "test" not in c]

    @staticmethod
    def _check_runs(runner):
        # The `test -e <marker>` completeness-check run.
        return [c for c in runner.calls if c[1:2] == ["run"] and "test" in c]

    def test_reuses_existing_complete_volume(self):
        spec = _spec()
        name = _expected_name(spec)
        # volume inspect -> present; completeness `run ... test` -> rc 0 (complete).
        runner = ScriptedRunner(lambda argv: ProcResult(0, "", ""))
        result = resolve_or_build_clang(Runtime("docker", runner), spec)
        self.assertEqual(result.name, name)
        self.assertFalse(result.built)
        self.assertFalse([c for c in runner.calls if c[1:3] == ["volume", "create"]])
        self.assertFalse(self._build_runs(runner))  # no build
        self.assertTrue(self._check_runs(runner))  # completeness was checked

    def test_rebuilds_incomplete_volume(self):
        spec = _spec()
        name = _expected_name(spec)

        def handler(argv):
            if argv[1:3] == ["volume", "inspect"]:
                return ProcResult(0, "", "")  # clang + ccache present
            if argv[1:2] == ["run"] and "test" in argv:
                return ProcResult(1, "", "")  # marker absent -> incomplete
            return ProcResult(0, "", "")  # create / rm / build succeed

        runner = ScriptedRunner(handler)
        result = resolve_or_build_clang(Runtime("docker", runner), spec)
        self.assertEqual(result.name, name)
        self.assertTrue(result.built)
        # Incomplete volume was discarded, then rebuilt.
        self.assertTrue(
            [c for c in runner.calls if c[1:3] == ["volume", "rm"] and name in c]
        )
        self.assertEqual(len(self._build_runs(runner)), 1)

    def test_builds_when_absent(self):
        spec = _spec()
        name = _expected_name(spec)

        def handler(argv):
            if argv[1:3] == ["volume", "inspect"]:
                return ProcResult(1 if argv[3] == name else 0, "", "")
            return ProcResult(0, "", "")

        runner = ScriptedRunner(handler)
        result = resolve_or_build_clang(Runtime("docker", runner), spec)
        self.assertEqual(result.name, name)
        self.assertTrue(result.built)
        create = [c for c in runner.calls if c[1:3] == ["volume", "create"]]
        self.assertTrue(
            any(name in c and "aqb.commit=" + spec.commit in c for c in create)
        )
        builds = self._build_runs(runner)
        self.assertEqual(len(builds), 1)
        joined = " ".join(builds[0])
        self.assertIn(f"{name}:", joined)  # clang volume mounted
        self.assertIn(f"{CCACHE_VOLUME}:", joined)  # ccache mounted
        self.assertIn("AQB_PRESET=aqb-base", builds[0])  # preset name passed
        self.assertTrue(any(a.startswith("AQB_USER_PRESETS_JSON=") for a in builds[0]))
        # Volume was absent, so no completeness check ran.
        self.assertFalse(self._check_runs(runner))

    def test_build_failure_removes_volume_and_raises(self):
        spec = _spec()
        name = _expected_name(spec)

        def handler(argv):
            if argv[1:3] == ["volume", "inspect"]:
                return ProcResult(1 if argv[3] == name else 0, "", "")
            if argv[1:2] == ["run"] and "test" not in argv:
                return ProcResult(2, "", "compile error")
            return ProcResult(0, "", "")

        runner = ScriptedRunner(handler)
        with self.assertRaises(ClangBuildError):
            resolve_or_build_clang(Runtime("docker", runner), spec)
        removed = [c for c in runner.calls if c[1:3] == ["volume", "rm"]]
        self.assertTrue(any(name in c for c in removed))

    def test_raises_and_keeps_volume_when_check_cannot_run(self):
        spec = _spec()
        name = _expected_name(spec)

        def handler(argv):
            if argv[1:3] == ["volume", "inspect"]:
                return ProcResult(0, "", "")  # volume present
            if argv[1:2] == ["run"] and "test" in argv:
                # docker/podman exit 125 == the run itself could not start
                # (e.g. builder image was pruned), NOT "marker absent".
                return ProcResult(125, "", "Unable to find image")
            return ProcResult(0, "", "")

        runner = ScriptedRunner(handler)
        with self.assertRaises(RuntimeCommandError):
            resolve_or_build_clang(Runtime("docker", runner), spec)
        # A good volume must NOT be destroyed when the check couldn't run.
        self.assertFalse([c for c in runner.calls if c[1:3] == ["volume", "rm"]])

    def test_completeness_check_overrides_entrypoint_to_test(self):
        # The builder image's ENTRYPOINT is the build script; the completeness
        # check must override it with `test`, else it would run the build script
        # (which fails on missing AQB_* env) and every volume would look
        # incomplete. Regression guard for that container-semantics bug.
        spec = _spec()
        runner = ScriptedRunner(lambda argv: ProcResult(0, "", ""))
        resolve_or_build_clang(Runtime("docker", runner), spec)
        check = self._check_runs(runner)[0]
        self.assertIn("--entrypoint", check)
        self.assertEqual(check[check.index("--entrypoint") + 1], "test")
        # The marker path is an argument to `test` (after the image), and the
        # bare "test" command is NOT appended after the image.
        self.assertIn("/opt/aqb/clang/.aqb-complete", check)

    def test_local_source_is_bind_mounted_readonly(self):
        # A local --source path must be bind-mounted into the builder container
        # (read-only, same path) so `git clone "$AQB_SOURCE"` can reach it.
        spec = _spec()  # source == "/work/llvm-project" (a local absolute path)
        name = _expected_name(spec)

        def handler(argv):
            if argv[1:3] == ["volume", "inspect"]:
                return ProcResult(1 if argv[3] == name else 0, "", "")
            return ProcResult(0, "", "")

        runner = ScriptedRunner(handler)
        resolve_or_build_clang(Runtime("docker", runner), spec)
        build = self._build_runs(runner)[0]
        self.assertIn("/work/llvm-project:/work/llvm-project:ro", build)

    def test_url_source_is_not_bind_mounted(self):
        spec = ClangBuildSpec(
            commit="deadbeefcafe0000",
            source="https://github.com/x/y.git",
            commit_title="t",
            preset="aqb-base",
            user_presets_json='{"version": 3, "configurePresets": [{"name": "aqb-base"}]}',
            builder_image="aqb-clang-builder:latest",
            builder_image_id="sha256:img",
            created="2026-07-16T13:15:00+00:00",
            build_config="preset=aqb-base",
        )
        name = _expected_name(spec)

        def handler(argv):
            if argv[1:3] == ["volume", "inspect"]:
                return ProcResult(1 if argv[3] == name else 0, "", "")
            return ProcResult(0, "", "")

        runner = ScriptedRunner(handler)
        resolve_or_build_clang(Runtime("docker", runner), spec)
        build = self._build_runs(runner)[0]
        # No read-only source bind-mount for a remote URL.
        self.assertFalse(any(":ro" in arg for arg in build))

    def test_rebuild_surfaces_removal_failure(self):
        # If discarding an incomplete volume fails, surface it loudly rather than
        # silently proceeding to a create that collides with "already exists".
        spec = _spec()
        name = _expected_name(spec)

        def handler(argv):
            if argv[1:3] == ["volume", "inspect"]:
                return ProcResult(0, "", "")  # present
            if argv[1:2] == ["run"] and "test" in argv:
                return ProcResult(1, "", "")  # incomplete
            if argv[1:3] == ["volume", "rm"]:
                return ProcResult(1, "", "cannot remove")  # removal fails
            return ProcResult(0, "", "")

        runner = ScriptedRunner(handler)
        with self.assertRaises(RuntimeCommandError):
            resolve_or_build_clang(Runtime("docker", runner), spec)

    def test_extra_mounts_are_bind_mounted(self):
        # e.g. a git worktree's common dir, computed by build_clang_volume.
        import dataclasses

        spec = dataclasses.replace(_spec(), extra_mounts=["/main/.git:/main/.git:ro"])
        name = _expected_name(spec)

        def handler(argv):
            if argv[1:3] == ["volume", "inspect"]:
                return ProcResult(1 if argv[3] == name else 0, "", "")
            return ProcResult(0, "", "")

        runner = ScriptedRunner(handler)
        resolve_or_build_clang(Runtime("docker", runner), spec)
        build = self._build_runs(runner)[0]
        self.assertIn("/main/.git:/main/.git:ro", build)


class BuildClangVolumeTest(unittest.TestCase):
    def test_assembles_presets_resolves_image_and_builds(self):
        def handler(argv):
            if argv[1:3] == ["image", "inspect"]:
                return ProcResult(0, "sha256:BUILDERID\n", "")
            if argv[1:3] == ["volume", "inspect"]:
                return ProcResult(1, "", "")  # absent -> build
            return ProcResult(0, "", "")

        runner = ScriptedRunner(handler)
        vol = build_clang_volume(
            Runtime("docker", runner),
            commit="349146dabe4b07651d02fb",
            source="/work/llvm-project",
            commit_title="t",
            preset="aqb-base",
            user_overlay_json=None,
            builder_image="aqb-clang-builder:latest",
            created="2026-07-16T13:15:00+00:00",
        )
        self.assertTrue(vol.name.startswith("aqb-clang-349146dabe4b-"))
        self.assertTrue(vol.built)
        self.assertTrue([c for c in runner.calls if c[1:3] == ["image", "inspect"]])
        build = [c for c in runner.calls if c[1:2] == ["run"] and "test" not in c][0]
        self.assertTrue(
            any(
                '"aqb-base"' in a
                for a in build
                if a.startswith("AQB_USER_PRESETS_JSON=")
            )
        )
        # Default resource limits are passed to the build (docker-standard flags).
        self.assertIn("-m", build)
        self.assertEqual(build[build.index("-m") + 1], "24G")
        self.assertIn("--cpus", build)
        self.assertEqual(build[build.index("--cpus") + 1], "8")
        # The build streams (not captured) so its progress is visible live.
        build_idx = next(
            i
            for i, c in enumerate(runner.calls)
            if c[1:2] == ["run"] and "test" not in c
        )
        self.assertFalse(runner.captures[build_idx])


class WorktreeCommonDirMountTest(unittest.TestCase):
    """Real-git tests for detecting a worktree's common dir (gated on git)."""

    def _git(self, *args):
        subprocess.run(
            ["git", *args],
            check=True,
            capture_output=True,
            text=True,
            env={
                **os.environ,
                "GIT_AUTHOR_NAME": "x",
                "GIT_AUTHOR_EMAIL": "x@y",
                "GIT_COMMITTER_NAME": "x",
                "GIT_COMMITTER_EMAIL": "x@y",
            },
        )

    @unittest.skipUnless(shutil.which("git"), "git required")
    def test_worktree_mounts_common_dir(self):
        from aqb.volume import _worktree_common_dir_mount

        with tempfile.TemporaryDirectory() as root:
            main = os.path.join(root, "main")
            wt = os.path.join(root, "wt")
            self._git("init", "-q", main)
            self._git("-C", main, "commit", "--allow-empty", "-qm", "init")
            self._git("-C", main, "worktree", "add", "--detach", "-q", wt, "HEAD")

            mount = _worktree_common_dir_mount(wt)
            self.assertIsNotNone(mount)
            # git's own reported common dir must be the mount's host path.
            common = subprocess.run(
                ["git", "-C", wt, "rev-parse", "--git-common-dir"],
                capture_output=True,
                text=True,
            ).stdout.strip()
            self.assertEqual(mount, f"{common}:{common}:ro")

    @unittest.skipUnless(shutil.which("git"), "git required")
    def test_normal_repo_has_no_extra_mount(self):
        from aqb.volume import _worktree_common_dir_mount

        with tempfile.TemporaryDirectory() as root:
            self._git("init", "-q", root)
            self.assertIsNone(_worktree_common_dir_mount(root))


class ResolveCommitTitleTest(unittest.TestCase):
    """Real-git tests for resolving a commit's subject (gated on git)."""

    def _git(self, *args):
        subprocess.run(
            ["git", *args],
            check=True,
            capture_output=True,
            text=True,
            env={
                **os.environ,
                "GIT_AUTHOR_NAME": "x",
                "GIT_AUTHOR_EMAIL": "x@y",
                "GIT_COMMITTER_NAME": "x",
                "GIT_COMMITTER_EMAIL": "x@y",
            },
        )

    @unittest.skipUnless(shutil.which("git"), "git required")
    def test_resolves_subject_from_local_repo(self):
        from aqb.volume import _resolve_commit_title

        with tempfile.TemporaryDirectory() as repo:
            self._git("init", "-q", repo)
            self._git(
                "-C", repo, "commit", "--allow-empty", "-qm", "hello world subject"
            )
            sha = subprocess.run(
                ["git", "-C", repo, "rev-parse", "HEAD"],
                capture_output=True,
                text=True,
            ).stdout.strip()
            self.assertEqual(_resolve_commit_title(repo, sha), "hello world subject")

    def test_url_source_yields_empty(self):
        from aqb.volume import _resolve_commit_title

        self.assertEqual(_resolve_commit_title("https://github.com/x/y.git", "abc"), "")

    def test_bad_commit_yields_empty(self):
        from aqb.volume import _resolve_commit_title

        with tempfile.TemporaryDirectory() as repo:
            # not even a git repo -> empty, no raise
            self.assertEqual(_resolve_commit_title(repo, "deadbeef"), "")
