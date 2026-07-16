from __future__ import annotations

import hashlib
import json
import os
import subprocess
from dataclasses import dataclass, field
from typing import Dict, List, Optional

from aqb.errors import ClangBuildError, RuntimeCommandError
from aqb.presets import assemble_user_presets
from aqb.runtime import Runtime

CLANG_VOLUME_PREFIX = "aqb-clang"
CCACHE_VOLUME = "aqb-ccache"
SHORT_COMMIT_LEN = 12
DIGEST_LEN = 12
CLANG_INSTALL_MOUNT = "/opt/aqb/clang"
CCACHE_MOUNT = "/ccache"
COMPLETE_MARKER = f"{CLANG_INSTALL_MOUNT}/.aqb-complete"


def build_config_digest(
    user_presets_json: str, preset: str, builder_image_id: str
) -> str:
    """Stable digest of everything that changes the built clang binary: the
    assembled CMakeUserPresets.json content, the selected preset name, and the
    builder image. Canonical (sorted-key) JSON, so semantically-equal inputs
    collide and unrelated inputs do not.
    """
    normalized = json.dumps(
        {
            "presets": json.loads(user_presets_json),
            "preset": preset,
            "builder": builder_image_id,
        },
        sort_keys=True,
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
    preset: str  # configure-preset name to build (e.g. "aqb-base")
    user_presets_json: str  # assembled CMakeUserPresets.json content (canonical)
    builder_image: str  # image ref passed to `run`
    builder_image_id: str  # resolved digest, for the label and the digest input
    created: str  # ISO-8601, provenance only (excluded from the digest)
    build_config: str  # human-readable recipe string, for the label
    extra_mounts: List[str] = field(default_factory=list)  # extra ``-v`` values
    builder_memory: str = ""  # ``-m`` limit for the build (e.g. "24G"); "" = none
    builder_cpus: str = ""  # ``--cpus`` limit for the build (e.g. "8"); "" = none


@dataclass
class ClangVolume:
    """The result of resolving (or building) a Clang Volume."""

    name: str
    config_digest: str
    built: bool  # True if this call built it; False if an existing volume was reused


def _is_local_source(source: str) -> bool:
    """True if ``source`` is a local absolute path rather than a git remote URL."""
    return source.startswith("/")


def _worktree_common_dir_mount(source: str) -> Optional[str]:
    """A read-only bind-mount for ``source``'s git *common dir*, if it is a git
    worktree.

    A worktree's ``.git`` is a file pointing at the main repo's git dir (which
    holds the shared object store), so bind-mounting only the worktree isn't
    enough for the builder's ``git clone`` to resolve objects. Returns the
    ``host:host:ro`` mount for that common dir, or ``None`` for a normal repo
    (whose ``.git`` sits inside the already-mounted source) or on any error.
    """
    try:
        result = subprocess.run(
            ["git", "-C", source, "rev-parse", "--git-common-dir"],
            capture_output=True,
            text=True,
        )
    except OSError:
        return None
    if result.returncode != 0:
        return None
    common = result.stdout.strip()
    if not common:
        return None
    if not os.path.isabs(common):
        common = os.path.abspath(os.path.join(source, common))
    # A normal repo's common dir is ``<source>/.git`` (already covered by the
    # source mount); only a worktree points outside the source tree.
    if common == os.path.join(source, ".git") or common.startswith(source + os.sep):
        return None
    return f"{common}:{common}:ro"


def _builder_run_argv(volume: str, spec: ClangBuildSpec) -> List[str]:
    """Container invocation that builds clang from ``spec.commit`` and installs
    it into ``volume``.

    Mount targets are fixed contract constants also hard-coded in build.sh:
    the clang volume at ``/opt/aqb/clang`` and the ccache volume at ``/ccache``.
    When the source is a local path it is bind-mounted read-only at the *same*
    path inside the container, so the builder's ``git clone "$AQB_SOURCE"`` can
    reach it (a container cannot otherwise see the host filesystem); a remote URL
    needs no mount. The builder image is expected to read: ``AQB_COMMIT``,
    ``AQB_SOURCE``, ``AQB_PRESET`` (configure-preset name), and
    ``AQB_USER_PRESETS_JSON`` (the assembled CMakeUserPresets.json content,
    written into ``$SRC/llvm/``). Optional ``builder_memory``/``builder_cpus``
    are passed as docker-standard ``-m``/``--cpus`` resource limits (building
    LLVM is memory-hungry); they do not affect the built binary and so are not
    part of the volume digest.
    """
    args = ["run", "--rm"]
    if spec.builder_memory:
        args += ["-m", spec.builder_memory]
    if spec.builder_cpus:
        args += ["--cpus", spec.builder_cpus]
    args += [
        "-v",
        f"{volume}:{CLANG_INSTALL_MOUNT}",
        "-v",
        f"{CCACHE_VOLUME}:{CCACHE_MOUNT}",
    ]
    if _is_local_source(spec.source):
        args += ["-v", f"{spec.source}:{spec.source}:ro"]
    for mount in spec.extra_mounts:
        args += ["-v", mount]
    args += [
        "-e",
        f"AQB_COMMIT={spec.commit}",
        "-e",
        f"AQB_SOURCE={spec.source}",
        "-e",
        f"AQB_PRESET={spec.preset}",
        "-e",
        f"AQB_USER_PRESETS_JSON={spec.user_presets_json}",
        spec.builder_image,
    ]
    return args


def _clang_volume_status(runtime: Runtime, volume: str, builder_image: str) -> str:
    """Classify an existing Clang Volume as ``"complete"`` or ``"incomplete"``.

    A successful build writes ``COMPLETE_MARKER`` into the install tree as its
    final step; the check runs the builder image with ``test -e`` (reading a
    file inside a volume requires a container). The builder image's
    ``ENTRYPOINT`` is the build script itself, so ``--entrypoint test``
    overrides it for this invocation — otherwise the arguments below would be
    appended to the build script instead of replacing it, running a build
    (which fails immediately on missing ``AQB_*`` env vars) instead of the
    intended completeness check. ``test`` exits 0 when the marker is present
    and 1 when it is absent. Any other exit code means the check itself could
    not run (e.g. the builder image was pruned, or the daemon is unreachable);
    in that case we must NOT treat the volume as incomplete and destroy it, so
    ``RuntimeCommandError`` is raised instead — a good volume is never deleted
    because of an unrelated runtime problem.
    """
    check = runtime.run(
        [
            "run",
            "--rm",
            "--entrypoint",
            "test",
            "-v",
            f"{volume}:{CLANG_INSTALL_MOUNT}",
            builder_image,
            "-e",
            COMPLETE_MARKER,
        ]
    )
    if check.returncode == 0:
        return "complete"
    if check.returncode == 1:
        return "incomplete"
    raise RuntimeCommandError(
        f"cannot verify Clang Volume {volume}: completeness check exited "
        f"{check.returncode} (is builder image {builder_image} available?): "
        f"{check.stderr.strip()}"
    )


def resolve_or_build_clang(runtime: Runtime, spec: ClangBuildSpec) -> ClangVolume:
    """Resolve the Clang Volume for ``spec``, building it if necessary.

    If the volume exists *and* is complete, it is reused (``built=False``). A
    volume that exists but is incomplete (an interrupted build) is discarded and
    rebuilt. Otherwise the volume is created with its immutable labels, the
    ccache volume is ensured, and the builder container is run; on build failure
    the partial volume is removed and ``ClangBuildError`` is raised.
    """
    digest = build_config_digest(
        spec.user_presets_json, spec.preset, spec.builder_image_id
    )
    name = clang_volume_name(spec.commit, digest)

    if runtime.volume_exists(name):
        if _clang_volume_status(runtime, name, spec.builder_image) == "complete":
            return ClangVolume(name=name, config_digest=digest, built=False)
        # Residue of an interrupted build: discard and rebuild.
        runtime.remove_volume(name)

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
        # Best-effort cleanup: don't let a failed removal mask the build error.
        try:
            runtime.remove_volume(name)
        except RuntimeCommandError:
            pass
        raise ClangBuildError(
            f"building clang for {spec.commit} failed "
            f"(exit {result.returncode}): {result.stderr.strip()}"
        )
    return ClangVolume(name=name, config_digest=digest, built=True)


def build_clang_volume(
    runtime: Runtime,
    *,
    commit: str,
    source: str,
    commit_title: str,
    preset: str,
    user_overlay_json: Optional[str],
    builder_image: str,
    created: str,
    memory: str = "24G",
    cpus: str = "8",
) -> ClangVolume:
    """Resolve (or build) the Clang Volume for ``commit`` using ``builder_image``.

    Assembles ``aqb-base`` + the user's preset overlay into a canonical
    CMakeUserPresets.json, resolves the builder image's content digest
    (``{{.Id}}``), and delegates to ``resolve_or_build_clang``. ``memory``/``cpus``
    cap the builder container's resources (building LLVM is memory-hungry).
    """
    user_presets_json = assemble_user_presets(user_overlay_json)
    builder_image_id = runtime.image_id(builder_image)
    extra_mounts: List[str] = []
    if _is_local_source(source):
        worktree_mount = _worktree_common_dir_mount(source)
        if worktree_mount:
            extra_mounts.append(worktree_mount)
    spec = ClangBuildSpec(
        commit=commit,
        source=source,
        commit_title=commit_title,
        preset=preset,
        user_presets_json=user_presets_json,
        builder_image=builder_image,
        builder_image_id=builder_image_id,
        created=created,
        build_config=f"preset={preset}",
        extra_mounts=extra_mounts,
        builder_memory=memory,
        builder_cpus=cpus,
    )
    return resolve_or_build_clang(runtime, spec)
