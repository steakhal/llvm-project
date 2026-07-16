from __future__ import annotations

import os
import subprocess
from dataclasses import dataclass
from typing import Callable, Dict, List, Optional

from aqb.errors import RuntimeCommandError

DEFAULT_RUNTIME = "docker"


def resolve_runtime(cli_value: Optional[str] = None) -> str:
    """Resolve the container-runtime executable name.

    Precedence: an explicit ``--runtime`` value > ``$AQB_RUNTIME`` > ``docker``.
    The name is used verbatim as the program in every runtime command; AQB
    assumes a docker-compatible CLI, so ``podman``/``nerdctl``/etc. work by name.
    """
    if cli_value:
        return cli_value
    env_value = os.environ.get("AQB_RUNTIME")
    if env_value:
        return env_value
    return DEFAULT_RUNTIME


@dataclass
class ProcResult:
    returncode: int
    stdout: str
    stderr: str


Runner = Callable[[List[str]], "ProcResult"]


def _subprocess_runner(argv: List[str]) -> ProcResult:
    completed = subprocess.run(argv, capture_output=True, text=True)
    return ProcResult(completed.returncode, completed.stdout, completed.stderr)


class Runtime:
    """Thin wrapper over a docker-compatible CLI (``docker``/``podman``/...).

    Every command is ``[name, *args]`` handed to ``runner``. Inject a fake
    ``runner`` in tests to capture argv and program results without a daemon.
    """

    def __init__(self, name: str, runner: Optional[Runner] = None):
        self.name = name
        self._runner = runner or _subprocess_runner

    def run(self, args: List[str], check: bool = False) -> ProcResult:
        result = self._runner([self.name, *args])
        if check and result.returncode != 0:
            raise RuntimeCommandError(
                f"{self.name} {' '.join(args)} failed "
                f"(exit {result.returncode}): {result.stderr.strip()}"
            )
        return result

    def volume_exists(self, volume: str) -> bool:
        return self.run(["volume", "inspect", volume]).returncode == 0

    def create_volume(
        self, volume: str, labels: Optional[Dict[str, str]] = None
    ) -> None:
        args = ["volume", "create"]
        for key, value in sorted((labels or {}).items()):
            args += ["--label", f"{key}={value}"]
        args.append(volume)
        self.run(args, check=True)

    def remove_volume(self, volume: str) -> None:
        # No ``-f``: AQB only removes volumes it has confirmed exist, so the
        # force flag is unnecessary on docker and unsupported by some runtimes
        # whose ``volume delete`` takes no flags. ``check=True`` so a
        # failed removal surfaces loudly instead of silently leaving a stale
        # volume that a later ``create`` would collide with.
        self.run(["volume", "rm", volume], check=True)

    def image_id(self, image: str) -> str:
        result = self.run(
            ["image", "inspect", "--format", "{{.Id}}", image], check=True
        )
        return result.stdout.strip()
