from __future__ import annotations

import unittest
from typing import Callable, List

from aqb.errors import ClangBuildError, RuntimeCommandError
from aqb.runtime import ProcResult, Runtime
from aqb.volume import (
    CCACHE_VOLUME,
    ClangBuildSpec,
    build_config_digest,
    clang_volume_labels,
    clang_volume_name,
    ensure_cache_volume,
    resolve_or_build_clang,
)


class DigestTest(unittest.TestCase):
    def test_digest_is_stable(self):
        a = build_config_digest(["-DA=1", "-DB=2"], True, "sha256:img")
        b = build_config_digest(["-DA=1", "-DB=2"], True, "sha256:img")
        self.assertEqual(a, b)

    def test_digest_is_order_independent_over_cmake_args(self):
        a = build_config_digest(["-DA=1", "-DB=2"], True, "sha256:img")
        b = build_config_digest(["-DB=2", "-DA=1"], True, "sha256:img")
        self.assertEqual(a, b)

    def test_digest_changes_with_assertions(self):
        on = build_config_digest(["-DA=1"], True, "sha256:img")
        off = build_config_digest(["-DA=1"], False, "sha256:img")
        self.assertNotEqual(on, off)

    def test_digest_changes_with_builder_image(self):
        a = build_config_digest(["-DA=1"], True, "sha256:one")
        b = build_config_digest(["-DA=1"], True, "sha256:two")
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
        self._handler = handler

    def __call__(self, argv: List[str]) -> ProcResult:
        self.calls.append(list(argv))
        return self._handler(argv)


def _spec() -> ClangBuildSpec:
    return ClangBuildSpec(
        commit="349146dabe4b07651d02fb",
        source="/work/llvm-project",
        commit_title="do the thing",
        cmake_args=["-DLLVM_ENABLE_ASSERTIONS=ON"],
        assertions=True,
        builder_image="aqb-clang-builder:latest",
        builder_image_id="sha256:img",
        created="2026-07-16T13:15:00+00:00",
        build_config="cmake: -DLLVM_ENABLE_ASSERTIONS=ON",
    )


def _expected_name(spec: ClangBuildSpec) -> str:
    from aqb.volume import build_config_digest, clang_volume_name

    digest = build_config_digest(
        spec.cmake_args, spec.assertions, spec.builder_image_id
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
