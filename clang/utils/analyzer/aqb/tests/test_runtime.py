from __future__ import annotations

import os
import unittest
from unittest import mock

from aqb.runtime import resolve_runtime


class ResolveRuntimeTest(unittest.TestCase):
    def test_flag_beats_env_and_default(self):
        with mock.patch.dict(os.environ, {"AQB_RUNTIME": "podman"}):
            self.assertEqual(resolve_runtime("nerdctl"), "nerdctl")

    def test_env_used_when_no_flag(self):
        with mock.patch.dict(os.environ, {"AQB_RUNTIME": "podman"}):
            self.assertEqual(resolve_runtime(None), "podman")

    def test_defaults_to_docker(self):
        with mock.patch.dict(os.environ, {}, clear=True):
            self.assertEqual(resolve_runtime(None), "docker")
