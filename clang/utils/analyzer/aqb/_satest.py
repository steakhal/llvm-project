"""Bridge to the sibling SATest modules AQB reuses.

The SATest scripts live in the parent directory (``clang/utils/analyzer/``) as
top-level modules, not a package. Put that directory on ``sys.path`` so they can
be imported, then re-export the ones AQB builds on.
"""

from __future__ import annotations

import os
import sys

_SATEST_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
if _SATEST_DIR not in sys.path:
    sys.path.insert(0, _SATEST_DIR)

import CmpRuns  # noqa: E402  (import after sys.path tweak)
import ProjectMap  # noqa: E402

__all__ = ["CmpRuns", "ProjectMap"]
