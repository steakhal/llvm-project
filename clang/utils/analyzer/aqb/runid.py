from __future__ import annotations

import datetime
import secrets
from typing import Optional


def new_run_id(
    prefix: str = "r",
    now: Optional[datetime.datetime] = None,
    suffix: Optional[str] = None,
) -> str:
    """Return a sortable, unique run id like ``r-20260716-131500-a1b2``.

    ``now`` and ``suffix`` may be injected to make the value deterministic in
    tests; otherwise the current UTC time and a random 2-byte suffix are used.
    """
    if now is None:
        now = datetime.datetime.now(datetime.timezone.utc)
    if suffix is None:
        suffix = secrets.token_hex(2)
    stamp = now.strftime("%Y%m%d-%H%M%S")
    return f"{prefix}-{stamp}-{suffix}"
