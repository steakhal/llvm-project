from __future__ import annotations

import os
from typing import Optional

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
