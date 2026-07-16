from __future__ import annotations

import hashlib
from typing import Dict, List

CLANG_VOLUME_PREFIX = "aqb-clang"
CCACHE_VOLUME = "aqb-ccache"
SHORT_COMMIT_LEN = 12
DIGEST_LEN = 12


def build_config_digest(
    cmake_args: List[str], assertions: bool, builder_image_id: str
) -> str:
    """Stable digest of everything that changes the built clang binary.

    Order-independent over ``cmake_args``. Deliberately excludes the creation
    timestamp and the run-level note, which never affect the artifact and so
    must not force a distinct volume (see AQB-design.rst).
    """
    normalized = "\n".join(
        [
            "cmake=" + "\x1f".join(sorted(cmake_args)),
            f"assertions={int(assertions)}",
            f"builder={builder_image_id}",
        ]
    )
    return hashlib.sha256(normalized.encode("utf-8")).hexdigest()[:DIGEST_LEN]


def clang_volume_name(commit: str, config_digest: str) -> str:
    return f"{CLANG_VOLUME_PREFIX}-{commit[:SHORT_COMMIT_LEN]}-{config_digest}"


def clang_volume_labels(
    *,
    commit: str,
    commit_title: str,
    source: str,
    build_config: str,
    builder_image_id: str,
    created: str,
) -> Dict[str, str]:
    """Immutable labels stamped on a Clang Volume at creation.

    ``aqb.commit`` is the full hash (the name uses the short form); ``aqb.created``
    is provenance only and is intentionally *not* part of the digest.
    """
    return {
        "aqb.role": "clang",
        "aqb.commit": commit,
        "aqb.commit_title": commit_title,
        "aqb.source": source,
        "aqb.build_config": build_config,
        "aqb.builder_image": builder_image_id,
        "aqb.created": created,
    }
