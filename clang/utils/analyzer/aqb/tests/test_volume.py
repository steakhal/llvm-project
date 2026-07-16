from __future__ import annotations

import unittest
from typing import Callable, List

from aqb.errors import ClangBuildError
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
    def test_reuses_existing_volume_without_building(self):
        spec = _spec()
        name = _expected_name(spec)
        runner = ScriptedRunner(
            lambda argv: ProcResult(0, "", "")  # every inspect succeeds -> present
        )
        result = resolve_or_build_clang(Runtime("docker", runner), spec)
        self.assertEqual(result, name)
        # No create and no builder run when the clang volume already exists.
        self.assertFalse([c for c in runner.calls if c[1:3] == ["volume", "create"]])
        self.assertFalse([c for c in runner.calls if c[1:2] == ["run"]])

    def test_builds_when_absent(self):
        spec = _spec()
        name = _expected_name(spec)

        def handler(argv):
            if argv[1:3] == ["volume", "inspect"]:
                # clang volume absent; ccache present.
                return ProcResult(1 if argv[3] == name else 0, "", "")
            return ProcResult(0, "", "")

        runner = ScriptedRunner(handler)
        result = resolve_or_build_clang(Runtime("docker", runner), spec)
        self.assertEqual(result, name)
        create = [c for c in runner.calls if c[1:3] == ["volume", "create"]]
        self.assertTrue(
            any(name in c and "aqb.commit=" + spec.commit in c for c in create)
        )
        build = [c for c in runner.calls if c[1:2] == ["run"]]
        self.assertEqual(len(build), 1)
        joined = " ".join(build[0])
        self.assertIn(f"{name}:", joined)  # clang volume mounted
        self.assertIn(f"{CCACHE_VOLUME}:", joined)  # ccache mounted

    def test_build_failure_removes_volume_and_raises(self):
        spec = _spec()
        name = _expected_name(spec)

        def handler(argv):
            if argv[1:3] == ["volume", "inspect"]:
                return ProcResult(1 if argv[3] == name else 0, "", "")
            if argv[1:2] == ["run"]:
                return ProcResult(2, "", "compile error")
            return ProcResult(0, "", "")

        runner = ScriptedRunner(handler)
        with self.assertRaises(ClangBuildError):
            resolve_or_build_clang(Runtime("docker", runner), spec)
        removed = [c for c in runner.calls if c[1:3] == ["volume", "rm"]]
        self.assertTrue(any(name in c for c in removed))
