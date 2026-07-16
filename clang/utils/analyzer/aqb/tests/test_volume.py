from __future__ import annotations

import unittest

from aqb.volume import (
    build_config_digest,
    clang_volume_labels,
    clang_volume_name,
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
