from __future__ import annotations

import os
import unittest
from unittest import mock

from typing import Callable, List

from aqb.errors import RuntimeCommandError
from aqb.runtime import resolve_runtime
from aqb.runtime import ProcResult, Runtime, _subprocess_runner


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


class RecordingRunner:
    """A fake runner: records each argv and returns a programmed ProcResult.

    ``handler`` maps an argv list to a ProcResult; defaults to success/empty.
    """

    def __init__(self, handler: Callable[[List[str]], ProcResult] = None):
        self.calls: List[List[str]] = []
        self.captures: List[bool] = []
        self._handler = handler or (lambda argv: ProcResult(0, "", ""))

    def __call__(self, argv: List[str], capture: bool = True) -> ProcResult:
        self.calls.append(list(argv))
        self.captures.append(capture)
        return self._handler(argv)


class RuntimeTest(unittest.TestCase):
    def test_run_prepends_runtime_name(self):
        runner = RecordingRunner()
        Runtime("docker", runner).run(["ps", "-a"])
        self.assertEqual(runner.calls, [["docker", "ps", "-a"]])

    def test_run_check_raises_on_nonzero(self):
        runner = RecordingRunner(lambda argv: ProcResult(1, "", "boom"))
        with self.assertRaises(RuntimeCommandError):
            Runtime("podman", runner).run(["bogus"], check=True)

    def test_volume_exists_reflects_inspect_exit_code(self):
        present = RecordingRunner(lambda argv: ProcResult(0, "", ""))
        absent = RecordingRunner(lambda argv: ProcResult(1, "", "no such volume"))
        self.assertTrue(Runtime("docker", present).volume_exists("v"))
        self.assertFalse(Runtime("docker", absent).volume_exists("v"))
        self.assertEqual(present.calls[0], ["docker", "volume", "inspect", "v"])

    def test_create_volume_emits_sorted_labels(self):
        runner = RecordingRunner()
        Runtime("docker", runner).create_volume("v", {"b": "2", "a": "1"})
        self.assertEqual(
            runner.calls[0],
            ["docker", "volume", "create", "--label", "a=1", "--label", "b=2", "v"],
        )

    def test_remove_volume(self):
        runner = RecordingRunner()
        Runtime("docker", runner).remove_volume("v")
        # No -f: portable across runtimes (some runtimes' `volume delete` takes no flags).
        self.assertEqual(runner.calls[0], ["docker", "volume", "rm", "v"])

    def test_remove_volume_raises_on_failure(self):
        runner = RecordingRunner(lambda argv: ProcResult(1, "", "no such volume"))
        with self.assertRaises(RuntimeCommandError):
            Runtime("docker", runner).remove_volume("v")

    def test_image_id_strips_output(self):
        runner = RecordingRunner(lambda argv: ProcResult(0, "sha256:abc\n", ""))
        self.assertEqual(Runtime("docker", runner).image_id("img"), "sha256:abc")
        self.assertEqual(
            runner.calls[0],
            ["docker", "image", "inspect", "--format", "{{.Id}}", "img"],
        )


class CaptureModeTest(unittest.TestCase):
    def test_run_forwards_capture_flag_to_runner(self):
        runner = RecordingRunner()
        rt = Runtime("docker", runner)
        rt.run(["ps"])  # default: capture
        rt.run(["build", "."], capture=False)  # streaming
        self.assertEqual(runner.captures, [True, False])

    def test_subprocess_runner_captures_by_default(self):
        # A real trivial command; capture=True returns its stdout.
        result = _subprocess_runner(["printf", "hi"], capture=True)
        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "hi")

    def test_subprocess_runner_streams_when_not_capturing(self):
        # capture=False inherits stdio (nothing captured), returncode still set.
        result = _subprocess_runner(["true"], capture=False)
        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")
