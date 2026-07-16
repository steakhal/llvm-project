from __future__ import annotations

import hashlib
from dataclasses import dataclass
from typing import Dict, List

from aqb.errors import ClangBuildError
from aqb.runtime import Runtime

CLANG_VOLUME_PREFIX = "aqb-clang"
CCACHE_VOLUME = "aqb-ccache"
SHORT_COMMIT_LEN = 12
DIGEST_LEN = 12
CLANG_INSTALL_MOUNT = "/opt/aqb/clang"
CCACHE_MOUNT = "/ccache"


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


def ensure_cache_volume(runtime: Runtime) -> str:
    """Create the shared, mutable ccache volume if missing; return its name.

    The cache volume is *not* content-addressed and never affects any digest or
    run identity; it only speeds up clang builds and project analysis, and is
    safe to prune (the next run repopulates it).
    """
    if not runtime.volume_exists(CCACHE_VOLUME):
        runtime.create_volume(CCACHE_VOLUME, {"aqb.role": "cache"})
    return CCACHE_VOLUME


@dataclass
class ClangBuildSpec:
    """Everything needed to name, label, and build a Clang Volume."""

    commit: str  # full hash
    source: str  # git remote URL or absolute local clone path
    commit_title: str
    cmake_args: List[str]
    assertions: bool
    builder_image: str  # image ref passed to `run`
    builder_image_id: str  # resolved digest, for the label and the digest input
    created: str  # ISO-8601, provenance only (excluded from the digest)
    build_config: str  # human-readable recipe string, for the label


def _builder_run_argv(volume: str, spec: ClangBuildSpec) -> List[str]:
    """Container invocation that builds clang from ``spec.commit`` and installs
    it into ``volume``.

    The builder image (provided in Phase 3) is expected to read these env vars:
    ``AQB_COMMIT``, ``AQB_SOURCE``, ``AQB_CMAKE_ARGS`` (space-joined),
    ``AQB_ASSERTIONS`` (0/1), ``AQB_INSTALL_DIR`` (the mounted clang volume),
    ``AQB_CCACHE_DIR`` (the mounted ccache volume).
    """
    return [
        "run",
        "--rm",
        "-v",
        f"{volume}:{CLANG_INSTALL_MOUNT}",
        "-v",
        f"{CCACHE_VOLUME}:{CCACHE_MOUNT}",
        "-e",
        f"AQB_COMMIT={spec.commit}",
        "-e",
        f"AQB_SOURCE={spec.source}",
        "-e",
        "AQB_CMAKE_ARGS=" + " ".join(spec.cmake_args),
        "-e",
        f"AQB_ASSERTIONS={int(spec.assertions)}",
        "-e",
        f"AQB_INSTALL_DIR={CLANG_INSTALL_MOUNT}",
        "-e",
        f"AQB_CCACHE_DIR={CCACHE_MOUNT}",
        spec.builder_image,
    ]


def resolve_or_build_clang(runtime: Runtime, spec: ClangBuildSpec) -> str:
    """Return the Clang Volume name for ``spec``, building it if it is absent.

    If the volume already exists it is reused as-is. Otherwise the volume is
    created with its immutable labels, the ccache volume is ensured, and the
    builder container is run; on build failure the partial volume is removed and
    ``ClangBuildError`` is raised.
    """
    digest = build_config_digest(
        spec.cmake_args, spec.assertions, spec.builder_image_id
    )
    name = clang_volume_name(spec.commit, digest)
    if runtime.volume_exists(name):
        return name

    runtime.create_volume(
        name,
        clang_volume_labels(
            commit=spec.commit,
            commit_title=spec.commit_title,
            source=spec.source,
            build_config=spec.build_config,
            builder_image_id=spec.builder_image_id,
            created=spec.created,
        ),
    )
    ensure_cache_volume(runtime)
    result = runtime.run(_builder_run_argv(name, spec))
    if result.returncode != 0:
        runtime.remove_volume(name)
        raise ClangBuildError(
            f"building clang for {spec.commit} failed "
            f"(exit {result.returncode}): {result.stderr.strip()}"
        )
    return name
