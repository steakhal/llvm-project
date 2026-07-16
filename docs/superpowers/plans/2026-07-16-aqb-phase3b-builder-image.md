# AQB Phase 3b — Clang Builder Image + Volume Hardening (Preset-Based)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Make AQB build a Clang Volume for real, with the build configuration expressed as **CMake presets** (no raw `-D` flags): a built-in `aqb-base` preset, optional user preset overlays that can inherit it, a real builder image, and an `aqb build-clang` CLI verb.

**Status note:** The first hardening task of this phase is already committed — `ClangVolume(name, config_digest, built)`, the `.aqb-complete` completion marker, and `_clang_volume_status` (three-state: complete / incomplete / raise-and-keep-on-runtime-error) in `aqb/volume.py`. Those are **preset-agnostic and retained.** This plan reworks the *build-configuration* layer from raw cmake flags to presets and adds the builder image + CLI.

**Empirically-verified cmake facts this design relies on** (tested with `xcrun cmake` 4.0.3; also in memory `aqb-cmake-presets.md`):
- `CMakePresets.json` must live in the source root (the `llvm/` dir for LLVM); `sourceDir` is not a valid configure-preset field.
- `cmake --preset X -B <dir>` overrides the preset's `binaryDir` (no error); `-D` overrides preset cache vars. AQB uses this to force `-B /tmp/build -D CMAKE_INSTALL_PREFIX=/opt/aqb/clang`.
- `CMakeUserPresets.json` (gitignored) coexists with LLVM's tracked `llvm/CMakePresets.json` (v6); a v3 user file works alongside it, and user presets may inherit `aqb-base` or LLVM's own `llvm-*` presets.

**Architecture:** AQB assembles a `CMakeUserPresets.json` (aqb-base + the user's presets) in Python (canonical JSON, also the digest input), passes it via env, and the builder writes it into `$SRC/llvm/` and runs `cmake --preset`. Mount contract is hard-coded on both sides: clang volume → `/opt/aqb/clang`, ccache → `/ccache`.

**Tech Stack:** Python ≥3.8 (stdlib `json`/`dataclasses`/`hashlib`/`datetime`; `unittest`/`unittest.mock`), `bash` builder script, `python3 -m black`.

---

## Conventions (every task)

- Package `clang/utils/analyzer/aqb/`; repo root `/Users/benics/git/upstream-llvm-ssaf`.
- Run test/black from `clang/utils/analyzer/`. Suite: `python3 -m unittest discover -s aqb/tests -t . -v`. `black` = `python3 -m black`; confirm `--check` clean.
- Python 3.8 compat: `from __future__ import annotations`; `typing.*`, no PEP 604.
- Commit to `bb/aqb-design`; git from repo root or `git -C <root>`; no `cd` in compound git commands. Stage only each task's files.
- No daemon in tests: fake `Runtime` runner; `build.sh` validated via `bash -n` only.

## File Structure (this phase, remaining work)

- Create `clang/utils/analyzer/aqb/presets.py` — `AQB_BASE_PRESET`, `AQB_PRESET_VERSION`, `assemble_user_presets()`.
- Modify `clang/utils/analyzer/aqb/volume.py` — `ClangBuildSpec` (preset fields), canonical `build_config_digest`, `_builder_run_argv`, `resolve_or_build_clang` digest call, `build_clang_volume`.
- Modify `clang/utils/analyzer/aqb/tests/test_volume.py` — reworked `_spec`, digest tests, `ResolveOrBuildTest`, `BuildClangVolumeTest`.
- Modify `clang/utils/analyzer/aqb/cli.py` + `tests/test_cli.py` — `build-clang` verb.
- Create `clang/utils/analyzer/aqb/builder/Dockerfile`, `build.sh`; `tests/test_builder.py`.
- Modify `clang/docs/analyzer/developer-docs/AQB-design.rst`.

---

## Task 2: Preset model (`aqb-base` + assembly)

**Files:** Create `clang/utils/analyzer/aqb/presets.py`; Test `clang/utils/analyzer/aqb/tests/test_presets.py`.

- [ ] **Step 1: Write the failing test**

Create `clang/utils/analyzer/aqb/tests/test_presets.py`:

```python
from __future__ import annotations

import json
import unittest

from aqb.presets import AQB_BASE_PRESET_NAME, assemble_user_presets


class AssembleUserPresetsTest(unittest.TestCase):
    def test_base_only_when_no_overlay(self):
        doc = json.loads(assemble_user_presets(None))
        names = [p["name"] for p in doc["configurePresets"]]
        self.assertEqual(names, [AQB_BASE_PRESET_NAME])
        base = doc["configurePresets"][0]
        self.assertEqual(base["cacheVariables"]["CMAKE_BUILD_TYPE"], "Release")
        self.assertEqual(base["cacheVariables"]["LLVM_ENABLE_PROJECTS"], "clang")

    def test_overlay_presets_appended_after_base(self):
        overlay = json.dumps(
            {
                "version": 6,
                "configurePresets": [
                    {"name": "mine", "inherits": "aqb-base",
                     "cacheVariables": {"LLVM_ENABLE_ASSERTIONS": "OFF"}}
                ],
            }
        )
        doc = json.loads(assemble_user_presets(overlay))
        names = [p["name"] for p in doc["configurePresets"]]
        self.assertEqual(names, ["aqb-base", "mine"])
        # Assembled file's version is at least the overlay's.
        self.assertGreaterEqual(doc["version"], 6)

    def test_output_is_canonical_and_stable(self):
        a = assemble_user_presets(None)
        b = assemble_user_presets(None)
        self.assertEqual(a, b)
        # sorted keys -> deterministic bytes for the digest.
        self.assertEqual(a, json.dumps(json.loads(a), sort_keys=True, indent=2))
```

- [ ] **Step 2: Run — expect FAIL** (`No module named 'aqb.presets'`).
`python3 -m unittest aqb.tests.test_presets -v`

- [ ] **Step 3: Implement** — create `clang/utils/analyzer/aqb/presets.py`:

```python
from __future__ import annotations

import json
from typing import Optional

AQB_PRESET_VERSION = 3
AQB_BASE_PRESET_NAME = "aqb-base"

# Self-contained so it works at any pinned commit (does not depend on LLVM's own
# presets existing). AQB always overrides binaryDir (-B) and CMAKE_INSTALL_PREFIX
# (-D) on the command line, but they are set here too as sane defaults.
AQB_BASE_PRESET = {
    "name": AQB_BASE_PRESET_NAME,
    "displayName": "AQB base clang build",
    "generator": "Ninja",
    "binaryDir": "/tmp/build",
    "cacheVariables": {
        "CMAKE_BUILD_TYPE": "Release",
        "LLVM_ENABLE_PROJECTS": "clang",
        "LLVM_ENABLE_ASSERTIONS": "ON",
        "LLVM_CCACHE_BUILD": "ON",
        "CMAKE_INSTALL_PREFIX": "/opt/aqb/clang",
    },
}


def assemble_user_presets(user_overlay_json: Optional[str]) -> str:
    """Return the ``CMakeUserPresets.json`` content = ``aqb-base`` + the user's
    presets, as canonical JSON (sorted keys) so identical inputs yield identical
    bytes (this is the volume-digest input).

    The user's configure presets are appended after ``aqb-base`` and may
    ``inherits: aqb-base``. LLVM's own tracked ``CMakePresets.json`` is left
    untouched, so a user preset may also inherit ``llvm-*`` presets.
    """
    version = AQB_PRESET_VERSION
    configure_presets = [dict(AQB_BASE_PRESET)]
    if user_overlay_json:
        overlay = json.loads(user_overlay_json)
        version = max(version, int(overlay.get("version", version)))
        configure_presets.extend(overlay.get("configurePresets", []))
    doc = {"version": version, "configurePresets": configure_presets}
    return json.dumps(doc, sort_keys=True, indent=2)
```

- [ ] **Step 4: Run — expect PASS (3).** `python3 -m unittest aqb.tests.test_presets -v`

- [ ] **Step 5: Format + commit.**
```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb/presets.py clang/utils/analyzer/aqb/tests/test_presets.py
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "feat(aqb): built-in aqb-base preset + user-preset assembly"
```

---

## Task 3: Rework `volume.py` to presets

Replaces `ClangBuildSpec`'s `cmake_args`/`assertions` with a preset name + assembled presets JSON; makes the digest canonical over the presets; and passes presets to the builder (dropping the hard-coded install/ccache dir env vars — those become build.sh contract constants).

**Files:** Modify `clang/utils/analyzer/aqb/volume.py`; `clang/utils/analyzer/aqb/tests/test_volume.py`.

- [ ] **Step 1: Rewrite the failing tests.** In `test_volume.py`:

(a) Replace the `DigestTest` class with:

```python
class DigestTest(unittest.TestCase):
    _PRESETS = '{"version": 3, "configurePresets": [{"name": "aqb-base"}]}'

    def test_digest_is_stable(self):
        a = build_config_digest(self._PRESETS, "aqb-base", "sha256:img")
        b = build_config_digest(self._PRESETS, "aqb-base", "sha256:img")
        self.assertEqual(a, b)

    def test_digest_changes_with_preset_name(self):
        a = build_config_digest(self._PRESETS, "aqb-base", "sha256:img")
        b = build_config_digest(self._PRESETS, "other", "sha256:img")
        self.assertNotEqual(a, b)

    def test_digest_changes_with_presets_content(self):
        other = '{"version": 3, "configurePresets": [{"name": "aqb-base", "x": 1}]}'
        a = build_config_digest(self._PRESETS, "aqb-base", "sha256:img")
        b = build_config_digest(other, "aqb-base", "sha256:img")
        self.assertNotEqual(a, b)

    def test_digest_changes_with_builder_image(self):
        a = build_config_digest(self._PRESETS, "aqb-base", "sha256:one")
        b = build_config_digest(self._PRESETS, "aqb-base", "sha256:two")
        self.assertNotEqual(a, b)
```

(b) Replace the `_spec()` helper with:

```python
def _spec() -> ClangBuildSpec:
    return ClangBuildSpec(
        commit="349146dabe4b07651d02fb",
        source="/work/llvm-project",
        commit_title="do the thing",
        preset="aqb-base",
        user_presets_json='{"version": 3, "configurePresets": [{"name": "aqb-base"}]}',
        builder_image="aqb-clang-builder:latest",
        builder_image_id="sha256:img",
        created="2026-07-16T13:15:00+00:00",
        build_config="preset=aqb-base",
    )
```

(c) Replace the `_expected_name()` helper with:

```python
def _expected_name(spec: ClangBuildSpec) -> str:
    from aqb.volume import build_config_digest, clang_volume_name

    digest = build_config_digest(
        spec.user_presets_json, spec.preset, spec.builder_image_id
    )
    return clang_volume_name(spec.commit, digest)
```

(d) In `ResolveOrBuildTest.test_builds_when_absent`, replace the two mount assertions:

```python
        joined = " ".join(builds[0])
        self.assertIn(f"{name}:", joined)  # clang volume mounted
        self.assertIn(f"{CCACHE_VOLUME}:", joined)  # ccache mounted
```

with (add the preset assertions):

```python
        joined = " ".join(builds[0])
        self.assertIn(f"{name}:", joined)  # clang volume mounted
        self.assertIn(f"{CCACHE_VOLUME}:", joined)  # ccache mounted
        self.assertIn("AQB_PRESET=aqb-base", builds[0])  # preset name passed
        self.assertTrue(
            any(a.startswith("AQB_USER_PRESETS_JSON=") for a in builds[0])
        )
```

Leave the rest of `ResolveOrBuildTest` (reuse/incomplete/failure/keeps-volume-on-check-error) and `NameAndLabelTest`/`CacheVolumeTest`/`ScriptedRunner` unchanged. **Delete** the `test_cmake_args_passed_as_lossless_json` method if present (it was never added — the lossless-transport task was dropped in favor of presets; if it exists, remove it).

- [ ] **Step 2: Run — expect FAIL** (`ClangBuildSpec` has no `preset`/`user_presets_json`; `build_config_digest` signature mismatch). `python3 -m unittest aqb.tests.test_volume -v`

- [ ] **Step 3: Rework `volume.py`.**

(a) Ensure `import json` is present at the top (add it alphabetically after `import hashlib` if missing).

(b) Replace the `build_config_digest` function with:

```python
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
```

(c) Replace the `ClangBuildSpec` dataclass with:

```python
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
```

(d) Replace the `_builder_run_argv` function with (drops `AQB_INSTALL_DIR`/`AQB_CCACHE_DIR`/`AQB_CMAKE_ARGS` — those are build.sh contract constants now — and passes the preset + assembled presets):

```python
def _builder_run_argv(volume: str, spec: ClangBuildSpec) -> List[str]:
    """Container invocation that builds clang from ``spec.commit`` and installs
    it into ``volume``.

    Mount targets are fixed contract constants also hard-coded in build.sh:
    the clang volume at ``/opt/aqb/clang`` and the ccache volume at ``/ccache``.
    The builder image is expected to read: ``AQB_COMMIT``, ``AQB_SOURCE``,
    ``AQB_PRESET`` (configure-preset name), and ``AQB_USER_PRESETS_JSON`` (the
    assembled CMakeUserPresets.json content, written into ``$SRC/llvm/``).
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
        f"AQB_PRESET={spec.preset}",
        "-e",
        f"AQB_USER_PRESETS_JSON={spec.user_presets_json}",
        spec.builder_image,
    ]
```

(e) In `resolve_or_build_clang`, replace the digest call:

```python
    digest = build_config_digest(
        spec.cmake_args, spec.assertions, spec.builder_image_id
    )
```

with:

```python
    digest = build_config_digest(
        spec.user_presets_json, spec.preset, spec.builder_image_id
    )
```

Leave `ClangVolume`, `COMPLETE_MARKER`, `_clang_volume_status`, the rest of `resolve_or_build_clang`, `clang_volume_name`, `clang_volume_labels`, `ensure_cache_volume`, and the constants unchanged.

- [ ] **Step 4: Run — expect PASS.** `python3 -m unittest aqb.tests.test_volume -v`, then full suite `python3 -m unittest discover -s aqb/tests -t . -v`.

- [ ] **Step 5: Format + commit.**
```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb/volume.py clang/utils/analyzer/aqb/tests/test_volume.py
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "feat(aqb): represent clang build config as CMake presets"
```

---

## Task 4: `build_clang_volume` orchestration + `build-clang` CLI

**Files:** Modify `clang/utils/analyzer/aqb/volume.py`, `clang/utils/analyzer/aqb/cli.py`; Tests `tests/test_volume.py`, `tests/test_cli.py`.

- [ ] **Step 1: Write the failing tests.**

(a) Append to `test_volume.py` (add `build_clang_volume` to the `from aqb.volume import (...)` block):

```python
class BuildClangVolumeTest(unittest.TestCase):
    def test_assembles_presets_resolves_image_and_builds(self):
        def handler(argv):
            if argv[1:3] == ["image", "inspect"]:
                return ProcResult(0, "sha256:BUILDERID\n", "")
            if argv[1:3] == ["volume", "inspect"]:
                return ProcResult(1, "", "")  # absent -> build
            return ProcResult(0, "", "")

        runner = ScriptedRunner(handler)
        vol = build_clang_volume(
            Runtime("docker", runner),
            commit="349146dabe4b07651d02fb",
            source="/work/llvm-project",
            commit_title="t",
            preset="aqb-base",
            user_overlay_json=None,
            builder_image="aqb-clang-builder:latest",
            created="2026-07-16T13:15:00+00:00",
        )
        self.assertTrue(vol.name.startswith("aqb-clang-349146dabe4b-"))
        self.assertTrue(vol.built)
        self.assertTrue([c for c in runner.calls if c[1:3] == ["image", "inspect"]])
        # The assembled presets (aqb-base) reached the builder.
        build = [c for c in runner.calls if c[1:2] == ["run"] and "test" not in c][0]
        self.assertTrue(
            any('"aqb-base"' in a for a in build if a.startswith("AQB_USER_PRESETS_JSON="))
        )
```

(b) Append to `test_cli.py` (ensure `from unittest import mock` is imported):

```python
class BuildClangCliTest(unittest.TestCase):
    def test_invokes_orchestration_with_preset(self):
        from aqb.volume import ClangVolume

        captured = {}

        def fake_build(runtime, **kwargs):
            captured.update(kwargs)
            return ClangVolume(name="aqb-clang-x-y", config_digest="y", built=True)

        out = io.StringIO()
        with mock.patch("aqb.cli.build_clang_volume", fake_build), (
            contextlib.redirect_stdout(out)
        ):
            code = main(
                ["build-clang", "--commit", "abc", "--source", "/s", "--preset", "mine"]
            )
        self.assertEqual(code, 0)
        self.assertIn("aqb-clang-x-y", out.getvalue())
        self.assertEqual(captured["commit"], "abc")
        self.assertEqual(captured["preset"], "mine")
        self.assertIsNone(captured["user_overlay_json"])

    def test_reads_preset_file(self):
        import tempfile
        from aqb.volume import ClangVolume

        captured = {}

        def fake_build(runtime, **kwargs):
            captured.update(kwargs)
            return ClangVolume(name="v", config_digest="d", built=False)

        with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False) as handle:
            handle.write('{"version": 6, "configurePresets": []}')
            preset_path = handle.name
        with mock.patch("aqb.cli.build_clang_volume", fake_build):
            code = main(
                ["build-clang", "--commit", "c", "--source", "/s",
                 "--preset", "mine", "--preset-file", preset_path]
            )
        self.assertEqual(code, 0)
        self.assertIn("configurePresets", captured["user_overlay_json"])

    def test_reports_build_error(self):
        from aqb.errors import ClangBuildError

        def fake_build(runtime, **kwargs):
            raise ClangBuildError("boom")

        err = io.StringIO()
        with mock.patch("aqb.cli.build_clang_volume", fake_build), (
            contextlib.redirect_stderr(err)
        ):
            code = main(["build-clang", "--commit", "c", "--source", "/s"])
        self.assertEqual(code, 1)
        self.assertIn("boom", err.getvalue())
```

- [ ] **Step 2: Run — expect FAIL** (no `build_clang_volume`; no `build-clang` verb).

- [ ] **Step 3: Implement.**

(a) Append to `clang/utils/analyzer/aqb/volume.py` (add `from typing import Optional` to the typing import, and `from aqb.presets import assemble_user_presets`):

```python
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
) -> ClangVolume:
    """Resolve (or build) the Clang Volume for ``commit`` using ``builder_image``.

    Assembles ``aqb-base`` + the user's preset overlay into a canonical
    CMakeUserPresets.json, resolves the builder image's content digest
    (``{{.Id}}``), and delegates to ``resolve_or_build_clang``.
    """
    user_presets_json = assemble_user_presets(user_overlay_json)
    builder_image_id = runtime.image_id(builder_image)
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
    )
    return resolve_or_build_clang(runtime, spec)
```

(b) In `clang/utils/analyzer/aqb/cli.py`, add imports below `from aqb.store import RunStore`:

```python
import datetime

from aqb.errors import ClangBuildError, RuntimeCommandError
from aqb.runtime import Runtime, resolve_runtime
from aqb.volume import build_clang_volume
```

Register the verb in `build_parser()` after the `list` subparser:

```python
    build = sub.add_parser(
        "build-clang", help="build (or resolve) a Clang Volume for a commit"
    )
    build.add_argument("--commit", required=True, help="analyzer commit to build")
    build.add_argument(
        "--source", required=True,
        help="git remote URL or absolute local clone path",
    )
    build.add_argument(
        "--commit-title", default="", help="commit subject line (provenance)"
    )
    build.add_argument(
        "--preset", default="aqb-base",
        help="configure-preset name to build (default: aqb-base)",
    )
    build.add_argument(
        "--preset-file", default=None,
        help="path to a CMakeUserPresets.json overlay (may inherit aqb-base)",
    )
    build.add_argument(
        "--builder-image", default="aqb-clang-builder:latest",
        help="builder image ref",
    )
    build.add_argument(
        "--runtime", default=None,
        help="container runtime (default: $AQB_RUNTIME or docker)",
    )
    build.set_defaults(func=cmd_build_clang)
```

Add the handler:

```python
def cmd_build_clang(args: argparse.Namespace) -> int:
    overlay = None
    if args.preset_file:
        with open(args.preset_file) as handle:
            overlay = handle.read()
    runtime = Runtime(resolve_runtime(args.runtime))
    created = datetime.datetime.now(datetime.timezone.utc).isoformat()
    try:
        volume = build_clang_volume(
            runtime,
            commit=args.commit,
            source=args.source,
            commit_title=args.commit_title,
            preset=args.preset,
            user_overlay_json=overlay,
            builder_image=args.builder_image,
            created=created,
        )
    except (ClangBuildError, RuntimeCommandError) as exc:
        print(f"aqb build-clang: {exc}", file=sys.stderr)
        return 1
    print(volume.name)
    print(
        "built" if volume.built else "cached (reused existing volume)",
        file=sys.stderr,
    )
    return 0
```

- [ ] **Step 4: Run — expect PASS.** `python3 -m unittest aqb.tests.test_volume aqb.tests.test_cli -v`, then full suite.

- [ ] **Step 5: Format + commit.**
```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb/volume.py clang/utils/analyzer/aqb/cli.py clang/utils/analyzer/aqb/tests/test_volume.py clang/utils/analyzer/aqb/tests/test_cli.py
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "feat(aqb): build_clang_volume + 'build-clang' CLI verb (preset-based)"
```

---

## Task 5: Builder image (preset-based)

**Files:** Create `clang/utils/analyzer/aqb/builder/Dockerfile`, `clang/utils/analyzer/aqb/builder/build.sh`; Test `clang/utils/analyzer/aqb/tests/test_builder.py`.

- [ ] **Step 1: Write the failing test.** Create `clang/utils/analyzer/aqb/tests/test_builder.py`:

```python
from __future__ import annotations

import os
import subprocess
import unittest

BUILDER_DIR = os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "builder"
)


class BuilderImageTest(unittest.TestCase):
    def test_build_script_has_valid_bash_syntax(self):
        result = subprocess.run(
            ["bash", "-n", os.path.join(BUILDER_DIR, "build.sh")],
            capture_output=True, text=True,
        )
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_build_script_contract(self):
        with open(os.path.join(BUILDER_DIR, "build.sh")) as handle:
            body = handle.read()
        # Hard-coded mount contract (must match volume.py constants).
        self.assertIn("/opt/aqb/clang", body)
        self.assertIn("/ccache", body)
        # Preset-driven, writes the assembled user presets into llvm/, marks done.
        self.assertIn("CMakeUserPresets.json", body)
        self.assertIn("AQB_USER_PRESETS_JSON", body)
        self.assertIn("cmake --preset", body)
        self.assertIn(".aqb-complete", body)

    def test_dockerfile_wires_the_build_script(self):
        with open(os.path.join(BUILDER_DIR, "Dockerfile")) as handle:
            body = handle.read()
        self.assertIn("build.sh", body)
        self.assertIn("ENTRYPOINT", body)
```

- [ ] **Step 2: Run — expect FAIL** (`FileNotFoundError`, builder dir absent).

- [ ] **Step 3: Create the files.**

Create `clang/utils/analyzer/aqb/builder/build.sh`:

```bash
#!/usr/bin/env bash
# AQB Clang Volume builder. Reads the AQB_* env contract (see aqb/volume.py):
# builds clang from AQB_COMMIT (fetched from AQB_SOURCE) using the CMake preset
# AQB_PRESET, and installs it into the mounted clang volume. AQB_USER_PRESETS_JSON
# is the assembled CMakeUserPresets.json (aqb-base + any user overlay). The mount
# paths are fixed contract constants shared with volume.py. A completion marker
# is written last so an interrupted build is never mistaken for a valid cache.
set -euo pipefail

: "${AQB_COMMIT:?AQB_COMMIT is required}"
: "${AQB_SOURCE:?AQB_SOURCE is required}"
: "${AQB_PRESET:?AQB_PRESET is required}"
: "${AQB_USER_PRESETS_JSON:?AQB_USER_PRESETS_JSON is required}"

# Contract constants (must match volume.py CLANG_INSTALL_MOUNT / CCACHE_MOUNT).
INSTALL_DIR=/opt/aqb/clang
export CCACHE_DIR=/ccache

SRC=/tmp/llvm-project
git clone "$AQB_SOURCE" "$SRC"
git -C "$SRC" checkout --detach "$AQB_COMMIT"

# LLVM's top-level CMakeLists.txt (and its tracked CMakePresets.json) live in
# llvm/. Drop the assembled user presets alongside them; a fresh clone has no
# CMakeUserPresets.json, so this never clobbers LLVM's tracked file.
printf '%s' "$AQB_USER_PRESETS_JSON" > "$SRC/llvm/CMakeUserPresets.json"

cd "$SRC/llvm"
# Force AQB's build dir and install prefix regardless of preset content
# (command-line -B / -D override preset values).
cmake --preset "$AQB_PRESET" -B /tmp/build \
    -D CMAKE_INSTALL_PREFIX="$INSTALL_DIR" \
    -D LLVM_CCACHE_BUILD=ON
ninja -C /tmp/build install-clang install-clang-resource-headers

# Mark the install tree complete only after a fully successful install.
touch "$INSTALL_DIR/.aqb-complete"
```

Create `clang/utils/analyzer/aqb/builder/Dockerfile`:

```dockerfile
# Builder image for AQB Clang Volumes. Given the AQB_* env contract (see
# aqb/volume.py), it fetches a commit, builds clang via a CMake preset, and
# installs it into the mounted clang volume (/opt/aqb/clang), caching objects in
# /ccache.
#
# Build once:  docker build -t aqb-clang-builder:latest clang/utils/analyzer/aqb/builder
FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates git cmake ninja-build clang lld ccache python3 \
    && rm -rf /var/lib/apt/lists/*

COPY build.sh /usr/local/bin/aqb-build-clang
RUN chmod +x /usr/local/bin/aqb-build-clang

ENTRYPOINT ["/usr/local/bin/aqb-build-clang"]
```

- [ ] **Step 4: Run — expect PASS (3).** `python3 -m unittest aqb.tests.test_builder -v`. `bash -n` must be clean; fix script syntax if not (don't weaken the test).

- [ ] **Step 5: Format + commit.**
```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb/builder/Dockerfile clang/utils/analyzer/aqb/builder/build.sh clang/utils/analyzer/aqb/tests/test_builder.py
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "feat(aqb): preset-based clang builder image"
```

---

## Task 6: Green check + design-doc updates

- [ ] **Step 1: Full suite + formatting + CLI smoke.**
`python3 -m unittest discover -s aqb/tests -t . -v` (all pass); `python3 -m black --check aqb/` (clean); `python3 -m aqb build-clang --help` (usage lists `--commit`/`--source`/`--preset`/`--preset-file`/`--builder-image`/`--runtime`, exit 0).

- [ ] **Step 2: Update `clang/docs/analyzer/developer-docs/AQB-design.rst`:**
- *Clang Build Artifacts / AQB owns the build recipe*: state the build config is a **CMake preset** — a built-in `aqb-base` plus an optional user overlay (`CMakeUserPresets.json`, may inherit `aqb-base` or LLVM's `llvm-*` presets), assembled into canonical JSON that is the digest input; the builder runs `cmake --preset` with AQB forcing `-B` and `-D CMAKE_INSTALL_PREFIX`.
- *Resolve-or-build workflow*: keep the completion-marker bullet; note the mount contract paths (`/opt/aqb/clang`, `/ccache`) are hard-coded on both sides.
- *Runtime*: keep the `.Id` image-digest note.
- *CLI Surface*: add the `build-clang` verb row.

```bash
git -C /Users/benics/git/upstream-llvm-ssaf add clang/docs/analyzer/developer-docs/AQB-design.rst
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "docs(aqb): document preset-based clang builds and build-clang verb"
```

- [ ] **Step 3: Commit any formatting fixes (only if needed).**

---

## How to build a clang volume (after this phase)

```bash
docker build -t aqb-clang-builder:latest clang/utils/analyzer/aqb/builder   # once
cd clang/utils/analyzer
python3 -m aqb build-clang --commit <sha> --source /path/to/llvm-project      # default aqb-base
python3 -m aqb build-clang --commit <sha> --source /path/to/llvm-project \
    --preset my-analyzer --preset-file ./my-presets.json                      # user overlay
# --runtime=podman / AQB_RUNTIME=podman to switch runtimes
```

## Self-Review

**Spec coverage:** built-in `aqb-base` + user-overlay assembly (canonical) → Task 2; preset-based `ClangBuildSpec` + canonical digest + preset env transport + hard-coded mount contract → Task 3; orchestration + `build-clang` CLI (`--preset`/`--preset-file`) → Task 4; real preset-driven builder image with completion marker → Task 5; docs → Task 6. The completion-marker + `ClangVolume` + three-state `_clang_volume_status` hardening is already committed and retained. Raw `-D` flags are intentionally absent (user decision: presets only).

**Placeholder scan:** every task ships full test + code, including the complete `Dockerfile`/`build.sh`; the only un-run-in-suite part (a real clang build) is a documented manual smoke behind a daemon, with `bash -n` + content checks standing in.

**Type consistency:** `assemble_user_presets(user_overlay_json)->str` (Task 2) is consumed by `build_clang_volume` (Task 4). `ClangBuildSpec(commit, source, commit_title, preset, user_presets_json, builder_image, builder_image_id, created, build_config)` (Task 3) is built by `build_clang_volume` and consumed by `_builder_run_argv`/`resolve_or_build_clang`. `build_config_digest(user_presets_json, preset, builder_image_id)` signature matches its callers and `_expected_name`. `build_clang_volume(runtime, *, commit, source, commit_title, preset, user_overlay_json, builder_image, created)` matches `cmd_build_clang`'s call and both CLI/volume tests. `AQB_PRESET` + `AQB_USER_PRESETS_JSON` produced by `_builder_run_argv` (Task 3) are consumed by `build.sh` (Task 5); the `/opt/aqb/clang` + `/ccache` + `.aqb-complete` constants match `volume.py`'s `CLANG_INSTALL_MOUNT`/`CCACHE_MOUNT`/`COMPLETE_MARKER`.
