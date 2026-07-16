# AQB Phase 3b — Clang Builder Image + Volume Hardening Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make AQB able to actually build a Clang Volume: a real builder image + build script honoring the `AQB_*` env contract, the volume-layer hardening the reviews flagged (build-completion marker, `(name, digest, built)` result, lossless cmake args, documented image digest), and an `aqb build-clang` CLI verb to trigger it.

**Architecture:** Hardening changes to `aqb/volume.py` and a new `build-clang` verb in `aqb/cli.py` — both unit-tested with the existing fake `Runtime` runner (no daemon). A new `aqb/builder/` directory holds the builder image (`Dockerfile` + `build.sh`); the script is validated daemon-free via `bash -n` and a real build is left to a documented, manually-run smoke. Design source of truth: `clang/docs/analyzer/developer-docs/AQB-design.rst` (*Clang Build Artifacts*, *Runtime*).

**Tech Stack:** Python ≥3.8 (stdlib: `json`, `dataclasses`, `datetime`, `subprocess` in tests; `unittest`/`unittest.mock`), `bash` for the builder script, formatted with `python3 -m black`.

---

## Conventions (apply to every task)

- **Package location:** `clang/utils/analyzer/aqb/`. Repo root `/Users/benics/git/upstream-llvm-ssaf`.
- **Run all test/black commands from `clang/utils/analyzer/`.** Whole suite: `python3 -m unittest discover -s aqb/tests -t . -v`.
- **`black` is `python3 -m black`** (25.11.0). Run `python3 -m black aqb/` before each commit; confirm `python3 -m black --check aqb/` clean. (black only formats `*.py`; it ignores `builder/Dockerfile` and `builder/build.sh`.)
- **Python 3.8 compat:** `from __future__ import annotations`; `typing.*`, never PEP 604.
- **Commit to `bb/aqb-design`.** Run git from repo root or with `git -C <root>`; never `cd` in a compound git command. Stage only each task's named files.
- **No daemon in tests.** All runtime interactions go through the injected fake runner; the builder script is validated with `bash -n` (syntax only), never executed against a daemon in the suite.

## File Structure (Phase 3b)

- Modify `clang/utils/analyzer/aqb/volume.py` — `ClangVolume` result, `COMPLETE_MARKER` + `_volume_complete`, reworked `resolve_or_build_clang`, `AQB_CMAKE_ARGS_JSON`, `build_clang_volume`.
- Modify `clang/utils/analyzer/aqb/tests/test_volume.py` — reworked `ResolveOrBuildTest`, cmake-args-JSON test, `BuildClangVolumeTest`.
- Modify `clang/utils/analyzer/aqb/cli.py` — `build-clang` verb + `cmd_build_clang`.
- Modify `clang/utils/analyzer/aqb/tests/test_cli.py` — `build-clang` CLI tests.
- Create `clang/utils/analyzer/aqb/builder/Dockerfile`, `clang/utils/analyzer/aqb/builder/build.sh`.
- Create `clang/utils/analyzer/aqb/tests/test_builder.py`.
- Modify `clang/docs/analyzer/developer-docs/AQB-design.rst` — document the marker, lossless cmake args, `.Id` digest, and the `build-clang` verb.

---

## Task 1: `ClangVolume` result + build-completion marker

Reworks `resolve_or_build_clang` to return a structured result and to treat an existing-but-incomplete volume (interrupted build) as rebuild. The completeness check runs the builder image with `test -e <marker>`.

**Files:**
- Modify: `clang/utils/analyzer/aqb/volume.py`
- Test: `clang/utils/analyzer/aqb/tests/test_volume.py`

- [ ] **Step 1: Rewrite `ResolveOrBuildTest` (the failing test)**

In `clang/utils/analyzer/aqb/tests/test_volume.py`, replace the ENTIRE `class ResolveOrBuildTest(unittest.TestCase):` block (currently the last class in the file, lines 122–173) with:

```python
class ResolveOrBuildTest(unittest.TestCase):
    @staticmethod
    def _build_runs(runner):
        # A builder *build* run (not the `test -e` completeness check).
        return [c for c in runner.calls if c[1:2] == ["run"] and "test" not in c]

    @staticmethod
    def _check_runs(runner):
        # The `test -e <marker>` completeness-check run.
        return [c for c in runner.calls if c[1:2] == ["run"] and "test" in c]

    def test_reuses_existing_complete_volume(self):
        spec = _spec()
        name = _expected_name(spec)
        # volume inspect -> present; completeness `run ... test` -> rc 0 (complete).
        runner = ScriptedRunner(lambda argv: ProcResult(0, "", ""))
        result = resolve_or_build_clang(Runtime("docker", runner), spec)
        self.assertEqual(result.name, name)
        self.assertFalse(result.built)
        self.assertFalse([c for c in runner.calls if c[1:3] == ["volume", "create"]])
        self.assertFalse(self._build_runs(runner))  # no build
        self.assertTrue(self._check_runs(runner))  # completeness was checked

    def test_rebuilds_incomplete_volume(self):
        spec = _spec()
        name = _expected_name(spec)

        def handler(argv):
            if argv[1:3] == ["volume", "inspect"]:
                return ProcResult(0, "", "")  # clang + ccache present
            if argv[1:2] == ["run"] and "test" in argv:
                return ProcResult(1, "", "")  # marker absent -> incomplete
            return ProcResult(0, "", "")  # create / rm / build succeed

        runner = ScriptedRunner(handler)
        result = resolve_or_build_clang(Runtime("docker", runner), spec)
        self.assertEqual(result.name, name)
        self.assertTrue(result.built)
        # Incomplete volume was discarded, then rebuilt.
        self.assertTrue(
            [c for c in runner.calls if c[1:3] == ["volume", "rm"] and name in c]
        )
        self.assertEqual(len(self._build_runs(runner)), 1)

    def test_builds_when_absent(self):
        spec = _spec()
        name = _expected_name(spec)

        def handler(argv):
            if argv[1:3] == ["volume", "inspect"]:
                return ProcResult(1 if argv[3] == name else 0, "", "")
            return ProcResult(0, "", "")

        runner = ScriptedRunner(handler)
        result = resolve_or_build_clang(Runtime("docker", runner), spec)
        self.assertEqual(result.name, name)
        self.assertTrue(result.built)
        create = [c for c in runner.calls if c[1:3] == ["volume", "create"]]
        self.assertTrue(
            any(name in c and "aqb.commit=" + spec.commit in c for c in create)
        )
        builds = self._build_runs(runner)
        self.assertEqual(len(builds), 1)
        joined = " ".join(builds[0])
        self.assertIn(f"{name}:", joined)  # clang volume mounted
        self.assertIn(f"{CCACHE_VOLUME}:", joined)  # ccache mounted
        # Volume was absent, so no completeness check ran.
        self.assertFalse(self._check_runs(runner))

    def test_build_failure_removes_volume_and_raises(self):
        spec = _spec()
        name = _expected_name(spec)

        def handler(argv):
            if argv[1:3] == ["volume", "inspect"]:
                return ProcResult(1 if argv[3] == name else 0, "", "")
            if argv[1:2] == ["run"] and "test" not in argv:
                return ProcResult(2, "", "compile error")
            return ProcResult(0, "", "")

        runner = ScriptedRunner(handler)
        with self.assertRaises(ClangBuildError):
            resolve_or_build_clang(Runtime("docker", runner), spec)
        removed = [c for c in runner.calls if c[1:3] == ["volume", "rm"]]
        self.assertTrue(any(name in c for c in removed))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python3 -m unittest aqb.tests.test_volume -v`
Expected: FAIL — `resolve_or_build_clang` still returns a `str`, so `result.name` raises `AttributeError` (and `test_reuses_existing_complete_volume` expects a completeness `run` that doesn't happen yet).

- [ ] **Step 3: Rework `volume.py`**

In `clang/utils/analyzer/aqb/volume.py`:

(a) Add the marker constant next to the existing constants (after `CCACHE_MOUNT = "/ccache"` on line 15):

```python
COMPLETE_MARKER = f"{CLANG_INSTALL_MOUNT}/.aqb-complete"
```

(b) Add a `ClangVolume` result dataclass immediately after the `ClangBuildSpec` dataclass (after line 90):

```python
@dataclass
class ClangVolume:
    """The result of resolving (or building) a Clang Volume."""

    name: str
    config_digest: str
    built: bool  # True if this call built it; False if an existing volume was reused
```

(c) Add the completeness helper immediately before `resolve_or_build_clang`:

```python
def _volume_complete(runtime: Runtime, volume: str, builder_image: str) -> bool:
    """Return True if ``volume`` holds a *completed* clang install.

    A successful build writes ``COMPLETE_MARKER`` into the install tree as its
    final step, so a volume that exists but lacks the marker is the residue of an
    interrupted build and must not be reused. The check runs the builder image
    with ``test -e`` since reading a file inside a volume requires a container.
    """
    check = runtime.run(
        [
            "run",
            "--rm",
            "-v",
            f"{volume}:{CLANG_INSTALL_MOUNT}",
            builder_image,
            "test",
            "-e",
            COMPLETE_MARKER,
        ]
    )
    return check.returncode == 0
```

(d) Replace the entire `resolve_or_build_clang` function (lines 125–159) with:

```python
def resolve_or_build_clang(runtime: Runtime, spec: ClangBuildSpec) -> ClangVolume:
    """Resolve the Clang Volume for ``spec``, building it if necessary.

    If the volume exists *and* is complete, it is reused (``built=False``). A
    volume that exists but is incomplete (an interrupted build) is discarded and
    rebuilt. Otherwise the volume is created with its immutable labels, the
    ccache volume is ensured, and the builder container is run; on build failure
    the partial volume is removed and ``ClangBuildError`` is raised.
    """
    digest = build_config_digest(
        spec.cmake_args, spec.assertions, spec.builder_image_id
    )
    name = clang_volume_name(spec.commit, digest)

    if runtime.volume_exists(name):
        if _volume_complete(runtime, name, spec.builder_image):
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
        runtime.remove_volume(name)
        raise ClangBuildError(
            f"building clang for {spec.commit} failed "
            f"(exit {result.returncode}): {result.stderr.strip()}"
        )
    return ClangVolume(name=name, config_digest=digest, built=True)
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `python3 -m unittest aqb.tests.test_volume -v`
Expected: PASS — the 6 digest/name/label tests, 2 cache tests, and the 4 reworked `ResolveOrBuildTest` cases (reuse-complete, rebuild-incomplete, builds-absent, failure).

- [ ] **Step 5: Format + commit**

```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb/volume.py clang/utils/analyzer/aqb/tests/test_volume.py
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "feat(aqb): ClangVolume result + build-completion marker"
```

---

## Task 2: Lossless cmake-arg transport

Change the builder env var from space-joined `AQB_CMAKE_ARGS` to `AQB_CMAKE_ARGS_JSON` (a JSON list), which the build script parses without word-splitting on spaces.

**Files:**
- Modify: `clang/utils/analyzer/aqb/volume.py`
- Test: `clang/utils/analyzer/aqb/tests/test_volume.py`

- [ ] **Step 1: Add the failing test**

Append this method to the `ResolveOrBuildTest` class in `clang/utils/analyzer/aqb/tests/test_volume.py`:

```python
    def test_cmake_args_passed_as_lossless_json(self):
        import json

        spec = ClangBuildSpec(
            commit="deadbeefcafe0000",
            source="/work/llvm-project",
            commit_title="t",
            cmake_args=["-DCMAKE_CXX_FLAGS=-O2 -g"],  # contains a space
            assertions=True,
            builder_image="aqb-clang-builder:latest",
            builder_image_id="sha256:img",
            created="2026-07-16T13:15:00+00:00",
            build_config="c",
        )
        name = _expected_name(spec)

        def handler(argv):
            if argv[1:3] == ["volume", "inspect"]:
                return ProcResult(1 if argv[3] == name else 0, "", "")
            return ProcResult(0, "", "")

        runner = ScriptedRunner(handler)
        resolve_or_build_clang(Runtime("docker", runner), spec)
        build = [c for c in runner.calls if c[1:2] == ["run"] and "test" not in c][0]
        expected = "AQB_CMAKE_ARGS_JSON=" + json.dumps(["-DCMAKE_CXX_FLAGS=-O2 -g"])
        # The multi-word arg travels as ONE argv element (no space-splitting).
        self.assertIn(expected, build)
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python3 -m unittest aqb.tests.test_volume -v`
Expected: FAIL — the build argv currently contains `AQB_CMAKE_ARGS=...`, not `AQB_CMAKE_ARGS_JSON=...`.

- [ ] **Step 3: Update `_builder_run_argv`**

In `clang/utils/analyzer/aqb/volume.py`, add `import json` to the imports at the top (alongside `import hashlib` — keep alphabetical: `import hashlib`, `import json`). Then in `_builder_run_argv`, replace this pair of lines:

```python
        "-e",
        "AQB_CMAKE_ARGS=" + " ".join(spec.cmake_args),
```

with:

```python
        "-e",
        "AQB_CMAKE_ARGS_JSON=" + json.dumps(spec.cmake_args),
```

And update the `_builder_run_argv` docstring line that reads:

```python
    ``AQB_COMMIT``, ``AQB_SOURCE``, ``AQB_CMAKE_ARGS`` (space-joined),
```

to:

```python
    ``AQB_COMMIT``, ``AQB_SOURCE``, ``AQB_CMAKE_ARGS_JSON`` (a JSON list, parsed
    losslessly by the builder),
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `python3 -m unittest aqb.tests.test_volume -v`
Expected: PASS (all volume tests, including the new lossless-json case).

- [ ] **Step 5: Format + commit**

```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb/volume.py clang/utils/analyzer/aqb/tests/test_volume.py
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "feat(aqb): pass cmake args to builder as lossless JSON"
```

---

## Task 3: `build_clang_volume` orchestration

A helper that resolves the builder image's content digest (`{{.Id}}`), assembles a `ClangBuildSpec`, and delegates to `resolve_or_build_clang` — the seam the CLI (and later Phase 3c) call.

**Files:**
- Modify: `clang/utils/analyzer/aqb/volume.py`
- Test: `clang/utils/analyzer/aqb/tests/test_volume.py`

- [ ] **Step 1: Write the failing test**

Add `build_clang_volume` to the `from aqb.volume import (...)` block at the top of `clang/utils/analyzer/aqb/tests/test_volume.py` (insert it alphabetically in the import list). Then append this test class to the file:

```python
class BuildClangVolumeTest(unittest.TestCase):
    def test_resolves_image_id_and_builds(self):
        def handler(argv):
            if argv[1:3] == ["image", "inspect"]:
                return ProcResult(0, "sha256:BUILDERID\n", "")
            if argv[1:3] == ["volume", "inspect"]:
                return ProcResult(1, "", "")  # clang + ccache absent -> build path
            return ProcResult(0, "", "")

        runner = ScriptedRunner(handler)
        vol = build_clang_volume(
            Runtime("docker", runner),
            commit="349146dabe4b07651d02fb",
            source="/work/llvm-project",
            commit_title="t",
            cmake_args=["-DX=1"],
            assertions=True,
            builder_image="aqb-clang-builder:latest",
            created="2026-07-16T13:15:00+00:00",
        )
        self.assertTrue(vol.name.startswith("aqb-clang-349146dabe4b-"))
        self.assertTrue(vol.built)
        # The builder image's .Id was resolved and folded into the labels.
        self.assertTrue(
            [c for c in runner.calls if c[1:3] == ["image", "inspect"]]
        )
        create = [c for c in runner.calls if c[1:3] == ["volume", "create"]]
        self.assertTrue(
            any("aqb.builder_image=sha256:BUILDERID" in c for c in create)
        )
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python3 -m unittest aqb.tests.test_volume -v`
Expected: FAIL — `ImportError: cannot import name 'build_clang_volume'`.

- [ ] **Step 3: Implement**

Append to `clang/utils/analyzer/aqb/volume.py`:

```python
def _describe_build_config(cmake_args: List[str], assertions: bool) -> str:
    """A short human-readable recipe string for the ``aqb.build_config`` label."""
    parts = list(cmake_args)
    parts.append("assertions=on" if assertions else "assertions=off")
    return "cmake: " + " ".join(parts)


def build_clang_volume(
    runtime: Runtime,
    *,
    commit: str,
    source: str,
    commit_title: str,
    cmake_args: List[str],
    assertions: bool,
    builder_image: str,
    created: str,
) -> ClangVolume:
    """Resolve (or build) the Clang Volume for ``commit`` using ``builder_image``.

    Resolves the builder image's content digest (``{{.Id}}``) — which is part of
    the volume's identity — assembles a ``ClangBuildSpec``, and delegates to
    ``resolve_or_build_clang``.
    """
    builder_image_id = runtime.image_id(builder_image)
    spec = ClangBuildSpec(
        commit=commit,
        source=source,
        commit_title=commit_title,
        cmake_args=cmake_args,
        assertions=assertions,
        builder_image=builder_image,
        builder_image_id=builder_image_id,
        created=created,
        build_config=_describe_build_config(cmake_args, assertions),
    )
    return resolve_or_build_clang(runtime, spec)
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `python3 -m unittest aqb.tests.test_volume -v`
Expected: PASS (adds `BuildClangVolumeTest`).

- [ ] **Step 5: Format + commit**

```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb/volume.py clang/utils/analyzer/aqb/tests/test_volume.py
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "feat(aqb): build_clang_volume orchestration (resolve image id + spec)"
```

---

## Task 4: `aqb build-clang` CLI verb

**Files:**
- Modify: `clang/utils/analyzer/aqb/cli.py`
- Test: `clang/utils/analyzer/aqb/tests/test_cli.py`

- [ ] **Step 1: Write the failing tests**

In `clang/utils/analyzer/aqb/tests/test_cli.py`, add `from unittest import mock` to the imports at the top (alongside the existing `import contextlib`, `import io`, `import tempfile`, `import unittest`). Then append this test class:

```python
class BuildClangCliTest(unittest.TestCase):
    def test_build_clang_invokes_orchestration_and_prints_name(self):
        from aqb.volume import ClangVolume

        captured = {}

        def fake_build(runtime, **kwargs):
            captured.update(kwargs)
            return ClangVolume(name="aqb-clang-abc-def", config_digest="def", built=True)

        out = io.StringIO()
        with mock.patch("aqb.cli.build_clang_volume", fake_build), (
            contextlib.redirect_stdout(out)
        ):
            code = main(
                [
                    "build-clang",
                    "--commit", "abc123",
                    "--source", "/src",
                    "--cmake-arg", "-DX=1",
                    "--cmake-arg", "-DY=2",
                ]
            )
        self.assertEqual(code, 0)
        self.assertIn("aqb-clang-abc-def", out.getvalue())
        self.assertEqual(captured["commit"], "abc123")
        self.assertEqual(captured["source"], "/src")
        self.assertEqual(captured["cmake_args"], ["-DX=1", "-DY=2"])
        self.assertTrue(captured["assertions"])

    def test_build_clang_no_assertions_flag(self):
        from aqb.volume import ClangVolume

        captured = {}

        def fake_build(runtime, **kwargs):
            captured.update(kwargs)
            return ClangVolume(name="v", config_digest="d", built=False)

        with mock.patch("aqb.cli.build_clang_volume", fake_build):
            code = main(
                ["build-clang", "--commit", "c", "--source", "/s", "--no-assertions"]
            )
        self.assertEqual(code, 0)
        self.assertFalse(captured["assertions"])
        self.assertEqual(captured["cmake_args"], [])

    def test_build_clang_reports_build_error(self):
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

- [ ] **Step 2: Run test to verify it fails**

Run: `python3 -m unittest aqb.tests.test_cli -v`
Expected: FAIL — `build-clang` is not a registered command (argparse `SystemExit`).

- [ ] **Step 3: Implement in `cli.py`**

In `clang/utils/analyzer/aqb/cli.py`, add these imports below the existing `from aqb.store import RunStore` line:

```python
import datetime

from aqb.errors import ClangBuildError, RuntimeCommandError
from aqb.runtime import Runtime, resolve_runtime
from aqb.volume import build_clang_volume
```

Register the verb inside `build_parser()`, immediately after the `list` subparser is wired (after the `list_parser.set_defaults(func=cmd_list)` line):

```python
    build = sub.add_parser(
        "build-clang", help="build (or resolve) a Clang Volume for a commit"
    )
    build.add_argument("--commit", required=True, help="analyzer commit to build")
    build.add_argument(
        "--source",
        required=True,
        help="git remote URL or absolute local clone path",
    )
    build.add_argument(
        "--commit-title", default="", help="commit subject line (provenance)"
    )
    build.add_argument(
        "--builder-image",
        default="aqb-clang-builder:latest",
        help="builder image ref",
    )
    build.add_argument(
        "--cmake-arg",
        action="append",
        default=None,
        dest="cmake_args",
        metavar="ARG",
        help="extra cmake flag, repeatable",
    )
    build.add_argument(
        "--no-assertions",
        dest="assertions",
        action="store_false",
        help="build without LLVM assertions",
    )
    build.add_argument(
        "--runtime",
        default=None,
        help="container runtime (default: $AQB_RUNTIME or docker)",
    )
    build.set_defaults(func=cmd_build_clang, assertions=True)
```

Add the handler (next to the other `cmd_*` functions):

```python
def cmd_build_clang(args: argparse.Namespace) -> int:
    runtime = Runtime(resolve_runtime(args.runtime))
    created = datetime.datetime.now(datetime.timezone.utc).isoformat()
    try:
        volume = build_clang_volume(
            runtime,
            commit=args.commit,
            source=args.source,
            commit_title=args.commit_title,
            cmake_args=args.cmake_args or [],
            assertions=args.assertions,
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

- [ ] **Step 4: Run tests to verify they pass**

Run: `python3 -m unittest aqb.tests.test_cli -v`
Expected: PASS (the 3 existing CLI tests + 3 new `BuildClangCliTest` cases).

- [ ] **Step 5: Format + commit**

```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb/cli.py clang/utils/analyzer/aqb/tests/test_cli.py
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "feat(aqb): 'build-clang' CLI verb"
```

---

## Task 5: Builder image (Dockerfile + build.sh)

The image that actually compiles clang, honoring the `AQB_*` contract and writing the completion marker last. Validated daemon-free with `bash -n`; a real build is a documented manual smoke.

**Files:**
- Create: `clang/utils/analyzer/aqb/builder/Dockerfile`
- Create: `clang/utils/analyzer/aqb/builder/build.sh`
- Test: `clang/utils/analyzer/aqb/tests/test_builder.py`

- [ ] **Step 1: Write the failing test**

Create `clang/utils/analyzer/aqb/tests/test_builder.py`:

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
        script = os.path.join(BUILDER_DIR, "build.sh")
        result = subprocess.run(
            ["bash", "-n", script], capture_output=True, text=True
        )
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_build_script_writes_marker_and_parses_json_args(self):
        with open(os.path.join(BUILDER_DIR, "build.sh")) as handle:
            body = handle.read()
        # Completion marker must match volume.COMPLETE_MARKER's filename.
        self.assertIn(".aqb-complete", body)
        # cmake args are consumed from the JSON env var, not the space-joined one.
        self.assertIn("AQB_CMAKE_ARGS_JSON", body)

    def test_dockerfile_wires_the_build_script(self):
        with open(os.path.join(BUILDER_DIR, "Dockerfile")) as handle:
            body = handle.read()
        self.assertIn("build.sh", body)
        self.assertIn("ENTRYPOINT", body)
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python3 -m unittest aqb.tests.test_builder -v`
Expected: FAIL — `FileNotFoundError` for `builder/build.sh` (the builder dir doesn't exist yet).

- [ ] **Step 3: Create the builder image files**

Create `clang/utils/analyzer/aqb/builder/build.sh`:

```bash
#!/usr/bin/env bash
# AQB Clang Volume builder. Reads the AQB_* env contract (see aqb/volume.py):
# builds clang from AQB_COMMIT (fetched from AQB_SOURCE) and installs it into
# AQB_INSTALL_DIR, using AQB_CCACHE_DIR to speed up rebuilds. Writes a completion
# marker as the final step so an interrupted build is not mistaken for a valid
# cached volume.
set -euo pipefail

: "${AQB_COMMIT:?AQB_COMMIT is required}"
: "${AQB_SOURCE:?AQB_SOURCE is required}"
: "${AQB_INSTALL_DIR:?AQB_INSTALL_DIR is required}"
: "${AQB_CCACHE_DIR:?AQB_CCACHE_DIR is required}"
AQB_ASSERTIONS="${AQB_ASSERTIONS:-1}"
AQB_CMAKE_ARGS_JSON="${AQB_CMAKE_ARGS_JSON:-[]}"

export CCACHE_DIR="$AQB_CCACHE_DIR"

# Parse the JSON array of extra cmake args losslessly (handles embedded spaces).
mapfile -d '' -t EXTRA_CMAKE_ARGS < <(
    python3 -c 'import json, os, sys; sys.stdout.write("\0".join(json.loads(os.environ["AQB_CMAKE_ARGS_JSON"])))'
)

SRC=/tmp/llvm-project
git clone "$AQB_SOURCE" "$SRC"
git -C "$SRC" checkout --detach "$AQB_COMMIT"

ASSERTIONS=OFF
if [ "$AQB_ASSERTIONS" = "1" ]; then
    ASSERTIONS=ON
fi

cmake -G Ninja -S "$SRC/llvm" -B /tmp/build \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_ENABLE_PROJECTS=clang \
    -DLLVM_ENABLE_ASSERTIONS="$ASSERTIONS" \
    -DLLVM_CCACHE_BUILD=ON \
    -DCMAKE_INSTALL_PREFIX="$AQB_INSTALL_DIR" \
    "${EXTRA_CMAKE_ARGS[@]}"

ninja -C /tmp/build install-clang install-clang-resource-headers

# Mark the install tree complete only after a fully successful install.
touch "$AQB_INSTALL_DIR/.aqb-complete"
```

Create `clang/utils/analyzer/aqb/builder/Dockerfile`:

```dockerfile
# Builder image for AQB Clang Volumes. Given the AQB_* env contract (see
# aqb/volume.py), it fetches a commit, builds clang, and installs it into the
# mounted clang volume (AQB_INSTALL_DIR), caching objects in AQB_CCACHE_DIR.
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

- [ ] **Step 4: Run tests to verify they pass**

Run: `python3 -m unittest aqb.tests.test_builder -v`
Expected: PASS (3 tests). `bash -n` must report clean syntax; if it errors, fix the script syntax (do not weaken the test).

- [ ] **Step 5: Format + commit**

```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb/builder/Dockerfile clang/utils/analyzer/aqb/builder/build.sh clang/utils/analyzer/aqb/tests/test_builder.py
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "feat(aqb): clang builder image (Dockerfile + build script)"
```

---

## Task 6: Green check + design-doc updates

- [ ] **Step 1: Run the whole suite**

Run: `python3 -m unittest discover -s aqb/tests -t . -v`
Expected: PASS — all prior tests plus the reworked volume tests, the CLI build-clang tests, and the builder tests.

- [ ] **Step 2: Confirm formatting + CLI**

Run: `python3 -m black --check aqb/` (expect clean) and `python3 -m aqb build-clang --help` (expect usage listing `--commit`/`--source`/`--cmake-arg`/`--no-assertions`/`--runtime`, exit 0).

- [ ] **Step 3: Update the design doc**

In `clang/docs/analyzer/developer-docs/AQB-design.rst`, make these edits so the design matches what was built:

- In the **Resolve-or-build workflow** list (under *Clang Build Artifacts*), add a bullet after the "Absent: create the volume …" bullet:

  ```
   - **Exists but incomplete:** a successful build writes a completion marker
     (``.aqb-complete``) into the install tree as its final step. A volume that
     exists but lacks the marker is the residue of an interrupted build; it is
     discarded and rebuilt rather than reused.
  ```

- In the **AQB owns the build recipe** paragraph, change the phrase describing how cmake args reach the builder so it states they are passed as a JSON list (``AQB_CMAKE_ARGS_JSON``) and parsed losslessly (no space-splitting).

- In the **Runtime** section, add a sentence: "The recorded image digest is the runtime's content id (``<runtime> image inspect --format {{.Id}}``), which is present for both locally-built and pulled images; AQB does not rely on registry ``RepoDigests``."

- In the **CLI Surface** verb table, add a row:

  ```
   * - ``build-clang``
     - Build (or resolve) a Clang Volume for ``--commit`` using the builder
       image, printing the volume name. A utility verb; ``run`` builds volumes
       implicitly.
  ```

Then commit:

```bash
git -C /Users/benics/git/upstream-llvm-ssaf add clang/docs/analyzer/developer-docs/AQB-design.rst
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "docs(aqb): document completion marker, JSON cmake args, image digest, build-clang"
```

- [ ] **Step 4: Commit any formatting fixes (only if `black --check` reported changes)**

```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "style(aqb): black formatting"
```

---

## How to actually build a clang volume (after this phase)

```bash
# One-time: build the builder image.
docker build -t aqb-clang-builder:latest clang/utils/analyzer/aqb/builder

# Build a clang volume for a commit (from a local clone or a git URL):
cd clang/utils/analyzer
python3 -m aqb build-clang --commit <sha> --source /path/to/llvm-project
# prints the volume name (e.g. aqb-clang-<short>-<digest>); "built" or "cached" on stderr
# use --runtime=podman / AQB_RUNTIME=podman to switch runtimes
```

---

## Self-Review

**Spec coverage / Phase-2 backlog closed:** build-completion marker so interrupted builds aren't cache-valid → Task 1; `resolve_or_build_clang` returns `(name, config_digest, built)` → Task 1; lossless cmake-arg transport → Task 2; `.Id` image-digest decision documented → Tasks 3/6; builder image honoring the `AQB_*` contract → Task 5; a user-facing way to build a volume (`aqb build-clang`) → Task 4. Design doc kept current → Task 6.

**Placeholder scan:** No "TBD"/vague steps — every task ships full test + implementation code, including the complete builder `Dockerfile`/`build.sh`. The one thing not executed in-suite (a real clang build) is explicitly a documented manual smoke, with a daemon-free `bash -n` + content check standing in.

**Type consistency:** `ClangVolume(name, config_digest, built)` defined in Task 1 and consumed by `build_clang_volume` (Task 3), `cmd_build_clang` (Task 4), and tests. `_volume_complete(runtime, volume, builder_image)`, `COMPLETE_MARKER`, and the reworked `resolve_or_build_clang -> ClangVolume` are consistent across Tasks 1–3. `build_clang_volume(runtime, *, commit, source, commit_title, cmake_args, assertions, builder_image, created)` keyword signature matches `cmd_build_clang`'s call and the Task 3/4 tests. `AQB_CMAKE_ARGS_JSON` is produced in `_builder_run_argv` (Task 2) and consumed in `build.sh` (Task 5). The `.aqb-complete` marker filename matches between `COMPLETE_MARKER` (Task 1) and `build.sh`'s `touch` (Task 5).
