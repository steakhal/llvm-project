# AQB Phase 2 — Runtime & Clang Volumes Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give `aqb` the ability to (1) select a docker-compatible container runtime and (2) resolve-or-build a content-addressed **Clang Volume** for a `(commit, build config)`, plus manage the shared `aqb-ccache` volume — all as importable, unit-tested library code (no daemon required for tests).

**Architecture:** Two new focused modules — `aqb/runtime.py` (runtime-name resolution + a thin wrapper over the docker-compatible CLI with an injectable *runner* for tests) and `aqb/volume.py` (build-config digest, volume naming/labels, the ccache volume, and the resolve-or-build orchestration). A new `aqb/errors.py` holds shared domain exceptions. This phase also folds in three Phase-1 review follow-ups (UTC-normalized run ids, the shared errors module, and a golden end-to-end metadata test). The design source of truth is `clang/docs/analyzer/developer-docs/AQB-design.rst` (sections *Runtime*, *Clang Build Artifacts*, *The Shared Cache Volume*).

**Tech Stack:** Python ≥3.8 (stdlib only — `argparse`, `dataclasses`, `hashlib`, `subprocess`, `os`; `unittest`/`unittest.mock` for tests), formatted with `python3 -m black`, per the repo `pyproject.toml`.

---

## Conventions (apply to every task)

- **Package location:** `clang/utils/analyzer/aqb/`. Repo root is `/Users/benics/git/upstream-llvm-ssaf`.
- **Run all test/black commands from `clang/utils/analyzer/`** so `aqb` and its sibling SATest modules are importable. Every command below assumes that working directory.
- **Test framework:** stdlib `unittest`. Run a module with `python3 -m unittest aqb.tests.<name> -v`; the whole suite with `python3 -m unittest discover -s aqb/tests -t . -v`.
- **Formatting:** `black` is available as **`python3 -m black`** (version 25.11.0). Run `python3 -m black aqb/` before each commit and confirm `python3 -m black --check aqb/` is clean.
- **Python 3.8 compatibility:** every new module starts with `from __future__ import annotations`; use `typing.List`/`Optional`/`Dict`/`Callable`, never PEP 604 `X | None`.
- **Commit to the current branch `bb/aqb-design`.** Run git from the repo root or with `git -C <root>`; never put `cd` inside a compound git command. Stage only the files each task names.
- **No daemon in tests.** All runtime interactions go through an injectable `runner` callable; tests supply a fake that records argv and returns programmed results. Never invoke a real `docker`/`podman` in a unit test.

## File Structure (Phase 2)

- Create `clang/utils/analyzer/aqb/errors.py` — shared domain-exception hierarchy (`AqbError` + subclasses).
- Create `clang/utils/analyzer/aqb/runtime.py` — `resolve_runtime()`, `ProcResult`, the default subprocess runner, and the `Runtime` wrapper (`run`/`volume_exists`/`create_volume`/`remove_volume`/`image_id`).
- Create `clang/utils/analyzer/aqb/volume.py` — digest/naming/labels, `ensure_cache_volume()`, `ClangBuildSpec`, and `resolve_or_build_clang()`.
- Modify `clang/utils/analyzer/aqb/runid.py` — normalize `now` to UTC.
- Modify `clang/utils/analyzer/aqb/store.py` — import `RunNotFoundError` from `aqb.errors` instead of defining it locally.
- Create tests: `test_errors.py`, `test_runtime.py`, `test_volume.py`, `test_integration.py`; modify `test_runid.py` (add a UTC case).

## Scope note

Phase 2 delivers the runtime/volume **library** with a daemon-free unit-test suite; it does not add a new CLI verb. The user-facing wiring (`aqb run --commit … --runtime …`) and the actual **builder image + build script** that the resolve-or-build flow invokes are Phase 3. Phase 2 assembles and tests the *Python side* of the build recipe (the container invocation and its env-var contract); it does not compile clang. The env-var contract the builder image must honor is documented in `volume.py` so Phase 3 can implement it.

---

## Task 1: UTC-normalize run ids (Phase-1 follow-up)

Guarantees the lexicographic run-id sort equals chronological order even if a caller ever injects a timezone-aware or local-time `datetime`.

**Files:**
- Modify: `clang/utils/analyzer/aqb/runid.py`
- Test: `clang/utils/analyzer/aqb/tests/test_runid.py`

- [ ] **Step 1: Add the failing test**

Append this method to the existing `RunIdTest` class in `clang/utils/analyzer/aqb/tests/test_runid.py` (leave the two existing tests and the existing `import datetime` unchanged):

```python
    def test_aware_datetime_is_normalized_to_utc(self):
        aware = datetime.datetime(
            2026, 7, 16, 13, 15, 0,
            tzinfo=datetime.timezone(datetime.timedelta(hours=2)),
        )
        # 13:15 at +02:00 is 11:15 UTC.
        self.assertEqual(new_run_id(now=aware, suffix="a1b2"), "r-20260716-111500-a1b2")
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python3 -m unittest aqb.tests.test_runid -v`
Expected: FAIL — the new test gets `r-20260716-131500-a1b2` (offset not applied), not `...-111500-...`.

- [ ] **Step 3: Implement the UTC normalization**

Replace the body of `new_run_id` in `clang/utils/analyzer/aqb/runid.py` so the whole file reads:

```python
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
    The timestamp is always normalized to UTC so lexicographic ordering of ids
    matches chronological order. A naive ``now`` is treated as already-UTC (not
    shifted by the local offset); an aware ``now`` is converted to UTC.
    """
    if now is None:
        now = datetime.datetime.now(datetime.timezone.utc)
    elif now.tzinfo is None:
        now = now.replace(tzinfo=datetime.timezone.utc)
    else:
        now = now.astimezone(datetime.timezone.utc)
    if suffix is None:
        suffix = secrets.token_hex(2)
    stamp = now.strftime("%Y%m%d-%H%M%S")
    return f"{prefix}-{stamp}-{suffix}"
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `python3 -m unittest aqb.tests.test_runid -v`
Expected: PASS (3 tests — the original naive case still yields `...-131500-...`, the new aware case yields `...-111500-...`).

- [ ] **Step 5: Format + commit**

```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb/runid.py clang/utils/analyzer/aqb/tests/test_runid.py
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "feat(aqb): normalize run-id timestamps to UTC"
```

---

## Task 2: Shared errors module (Phase-1 follow-up)

Introduce `AqbError` and move `RunNotFoundError` into it before runtime/volume add more domain exceptions.

**Files:**
- Create: `clang/utils/analyzer/aqb/errors.py`
- Modify: `clang/utils/analyzer/aqb/store.py`
- Test: `clang/utils/analyzer/aqb/tests/test_errors.py`

- [ ] **Step 1: Write the failing test**

Create `clang/utils/analyzer/aqb/tests/test_errors.py`:

```python
from __future__ import annotations

import unittest

from aqb.errors import AqbError, ClangBuildError, RunNotFoundError, RuntimeCommandError
from aqb.store import RunNotFoundError as StoreRunNotFoundError


class ErrorsTest(unittest.TestCase):
    def test_all_domain_errors_derive_from_aqb_error(self):
        for exc in (RunNotFoundError, RuntimeCommandError, ClangBuildError):
            self.assertTrue(issubclass(exc, AqbError))

    def test_store_reexports_the_same_run_not_found_error(self):
        # store must re-export the shared class, not define its own.
        self.assertIs(StoreRunNotFoundError, RunNotFoundError)
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python3 -m unittest aqb.tests.test_errors -v`
Expected: FAIL — `ModuleNotFoundError: No module named 'aqb.errors'`.

- [ ] **Step 3: Create the errors module and rewire the store**

Create `clang/utils/analyzer/aqb/errors.py`:

```python
from __future__ import annotations


class AqbError(Exception):
    """Base class for all AQB domain errors."""


class RunNotFoundError(AqbError):
    """A run id (or unique prefix) could not be resolved to a stored run."""


class RuntimeCommandError(AqbError):
    """A container-runtime command exited non-zero."""


class ClangBuildError(AqbError):
    """Building a Clang Volume failed; the partial volume has been removed."""
```

In `clang/utils/analyzer/aqb/store.py`, delete the local `RunNotFoundError` class and import it instead. The top of the file becomes:

```python
from __future__ import annotations

import os
from typing import List

from aqb.errors import RunNotFoundError
from aqb.metadata import Metadata

RUN_SUBDIRS = ("reports", "metrics", "logs")
```

(Delete these lines:)

```python
class RunNotFoundError(Exception):
    pass
```

Leave the rest of `store.py` (the `RunStore` class) exactly as-is — it still raises `RunNotFoundError`, now the imported one.

- [ ] **Step 4: Run tests to verify they pass**

Run: `python3 -m unittest aqb.tests.test_errors aqb.tests.test_store -v`
Expected: PASS — `test_errors` (2 tests) plus all 5 `test_store` tests still pass (the store now raises the shared class; `test_store`'s `from aqb.store import RunNotFoundError` still resolves).

- [ ] **Step 5: Format + commit**

```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb/errors.py clang/utils/analyzer/aqb/store.py clang/utils/analyzer/aqb/tests/test_errors.py
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "feat(aqb): shared error hierarchy (AqbError)"
```

---

## Task 3: Golden end-to-end metadata test (Phase-1 follow-up)

Locks the store ↔ metadata ↔ CLI seam with a fully-populated record before Phase 3 starts writing real runs.

**Files:**
- Test: `clang/utils/analyzer/aqb/tests/test_integration.py`

- [ ] **Step 1: Write the failing test**

Create `clang/utils/analyzer/aqb/tests/test_integration.py`:

```python
from __future__ import annotations

import contextlib
import io
import tempfile
import unittest

from aqb.cli import main
from aqb.metadata import (
    AnalyzerProvenance,
    ContainerProvenance,
    ExecutionProvenance,
    Metadata,
    ProjectProvenance,
)
from aqb.store import RunStore

RUN_ID = "r-20260716-131500-abcd"


def _full_metadata() -> Metadata:
    return Metadata(
        run_id=RUN_ID,
        kind="functional",
        created="2026-07-16T13:15:00+00:00",
        analyzer=AnalyzerProvenance(
            commit="349146dabe4b07651d02",
            commit_title="do the thing",
            config_digest="cfg0a1b2c3d4",
            volume="aqb-clang-349146dabe4b-cfg0a1b2c3d4",
        ),
        container=ContainerProvenance(runtime="podman", image_digest="sha256:deadbeef"),
        execution=ExecutionProvenance(
            n=1,
            analyzer_args=["-Xclang", "-analyzer-stats"],
            note="with workaround X",
        ),
        corpus=[
            ProjectProvenance(
                name="curl",
                source="https://github.com/curl/curl",
                commit="aaaa1111",
                commit_title="curl 8.9",
            ),
            ProjectProvenance(
                name="redis",
                source="https://github.com/redis/redis",
                commit="bbbb2222",
                commit_title="7.4.0",
            ),
        ],
    )


class GoldenMetadataTest(unittest.TestCase):
    def test_full_metadata_survives_store_round_trip_and_is_listed(self):
        with tempfile.TemporaryDirectory() as root:
            store = RunStore(root)
            store.create_run(_full_metadata())

            # Round-trips losslessly (all nested provenance + collections).
            self.assertEqual(store.get(RUN_ID), _full_metadata())

            # And is visible through the CLI list command against the same home.
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                code = main(["--home", root, "list"])
            self.assertEqual(code, 0)
            self.assertIn(RUN_ID, out.getvalue())
```

- [ ] **Step 2: Run test to verify it fails, then passes**

This test exercises only already-built code (Metadata/RunStore/CLI), so it should pass immediately. First confirm it is *collected and runs*:
Run: `python3 -m unittest aqb.tests.test_integration -v`
Expected: PASS (1 test). If it fails, that indicates a real round-trip/seam defect — STOP and report it rather than editing the test to match buggy behavior.

- [ ] **Step 3: Format + commit**

```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb/tests/test_integration.py
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "test(aqb): golden end-to-end metadata round-trip + list"
```

---

## Task 4: `resolve_runtime()` — runtime name resolution

**Files:**
- Create: `clang/utils/analyzer/aqb/runtime.py`
- Test: `clang/utils/analyzer/aqb/tests/test_runtime.py`

- [ ] **Step 1: Write the failing test**

Create `clang/utils/analyzer/aqb/tests/test_runtime.py`:

```python
from __future__ import annotations

import os
import unittest
from unittest import mock

from aqb.runtime import resolve_runtime


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
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python3 -m unittest aqb.tests.test_runtime -v`
Expected: FAIL — `ModuleNotFoundError: No module named 'aqb.runtime'`.

- [ ] **Step 3: Write minimal implementation**

Create `clang/utils/analyzer/aqb/runtime.py`:

```python
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
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `python3 -m unittest aqb.tests.test_runtime -v`
Expected: PASS (3 tests).

- [ ] **Step 5: Format + commit**

```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb/runtime.py clang/utils/analyzer/aqb/tests/test_runtime.py
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "feat(aqb): resolve container runtime (flag > env > docker)"
```

---

## Task 5: `Runtime` wrapper over the docker-compatible CLI

Adds the command wrapper with an injectable runner so volume/image operations are unit-testable without a daemon.

**Files:**
- Modify: `clang/utils/analyzer/aqb/runtime.py`
- Test: `clang/utils/analyzer/aqb/tests/test_runtime.py` (add cases)

- [ ] **Step 1: Write the failing tests**

Append to `clang/utils/analyzer/aqb/tests/test_runtime.py` — add these imports at the top alongside the existing ones:

```python
from typing import Callable, List

from aqb.errors import RuntimeCommandError
from aqb.runtime import ProcResult, Runtime
```

Then add this recording fake and test class to the file:

```python
class RecordingRunner:
    """A fake runner: records each argv and returns a programmed ProcResult.

    ``handler`` maps an argv list to a ProcResult; defaults to success/empty.
    """

    def __init__(self, handler: Callable[[List[str]], ProcResult] = None):
        self.calls: List[List[str]] = []
        self._handler = handler or (lambda argv: ProcResult(0, "", ""))

    def __call__(self, argv: List[str]) -> ProcResult:
        self.calls.append(list(argv))
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

    def test_remove_volume_forces(self):
        runner = RecordingRunner()
        Runtime("docker", runner).remove_volume("v")
        self.assertEqual(runner.calls[0], ["docker", "volume", "rm", "-f", "v"])

    def test_image_id_strips_output(self):
        runner = RecordingRunner(lambda argv: ProcResult(0, "sha256:abc\n", ""))
        self.assertEqual(Runtime("docker", runner).image_id("img"), "sha256:abc")
        self.assertEqual(
            runner.calls[0],
            ["docker", "image", "inspect", "--format", "{{.Id}}", "img"],
        )
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `python3 -m unittest aqb.tests.test_runtime -v`
Expected: FAIL — `ImportError: cannot import name 'ProcResult'` (and `Runtime`).

- [ ] **Step 3: Extend `runtime.py`**

Append to `clang/utils/analyzer/aqb/runtime.py` (keep `resolve_runtime`/`DEFAULT_RUNTIME`). Add `import subprocess`, `from dataclasses import dataclass`, and widen the typing import to `from typing import Callable, Dict, List, Optional`, plus `from aqb.errors import RuntimeCommandError`. Then add:

```python
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

    def create_volume(self, volume: str, labels: Optional[Dict[str, str]] = None) -> None:
        args = ["volume", "create"]
        for key, value in sorted((labels or {}).items()):
            args += ["--label", f"{key}={value}"]
        args.append(volume)
        self.run(args, check=True)

    def remove_volume(self, volume: str) -> None:
        self.run(["volume", "rm", "-f", volume])

    def image_id(self, image: str) -> str:
        result = self.run(
            ["image", "inspect", "--format", "{{.Id}}", image], check=True
        )
        return result.stdout.strip()
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `python3 -m unittest aqb.tests.test_runtime -v`
Expected: PASS (3 resolve tests + 6 Runtime tests).

- [ ] **Step 5: Format + commit**

```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb/runtime.py clang/utils/analyzer/aqb/tests/test_runtime.py
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "feat(aqb): Runtime wrapper for docker-compatible CLI"
```

---

## Task 6: Clang Volume digest, naming, and labels

**Files:**
- Create: `clang/utils/analyzer/aqb/volume.py`
- Test: `clang/utils/analyzer/aqb/tests/test_volume.py`

- [ ] **Step 1: Write the failing test**

Create `clang/utils/analyzer/aqb/tests/test_volume.py`:

```python
from __future__ import annotations

import unittest

from aqb.volume import (
    build_config_digest,
    clang_volume_labels,
    clang_volume_name,
)


class DigestTest(unittest.TestCase):
    def test_digest_is_stable(self):
        a = build_config_digest(["-DA=1", "-DB=2"], True, "sha256:img")
        b = build_config_digest(["-DA=1", "-DB=2"], True, "sha256:img")
        self.assertEqual(a, b)

    def test_digest_is_order_independent_over_cmake_args(self):
        a = build_config_digest(["-DA=1", "-DB=2"], True, "sha256:img")
        b = build_config_digest(["-DB=2", "-DA=1"], True, "sha256:img")
        self.assertEqual(a, b)

    def test_digest_changes_with_assertions(self):
        on = build_config_digest(["-DA=1"], True, "sha256:img")
        off = build_config_digest(["-DA=1"], False, "sha256:img")
        self.assertNotEqual(on, off)

    def test_digest_changes_with_builder_image(self):
        a = build_config_digest(["-DA=1"], True, "sha256:one")
        b = build_config_digest(["-DA=1"], True, "sha256:two")
        self.assertNotEqual(a, b)


class NameAndLabelTest(unittest.TestCase):
    def test_volume_name_uses_short_commit_and_digest(self):
        self.assertEqual(
            clang_volume_name("349146dabe4b07651d02fb", "cfg0a1b2c3d4"),
            "aqb-clang-349146dabe4b-cfg0a1b2c3d4",
        )

    def test_labels_carry_full_commit_role_and_created(self):
        labels = clang_volume_labels(
            commit="349146dabe4b07651d02fb",
            commit_title="do the thing",
            source="/work/llvm-project",
            build_config="cmake: -DA=1 (asserts)",
            builder_image_id="sha256:img",
            created="2026-07-16T13:15:00+00:00",
        )
        self.assertEqual(labels["aqb.role"], "clang")
        self.assertEqual(labels["aqb.commit"], "349146dabe4b07651d02fb")  # full, not short
        self.assertEqual(labels["aqb.source"], "/work/llvm-project")
        self.assertEqual(labels["aqb.builder_image"], "sha256:img")
        self.assertEqual(labels["aqb.created"], "2026-07-16T13:15:00+00:00")
```

- [ ] **Step 2: Run test to verify it fails**

Run: `python3 -m unittest aqb.tests.test_volume -v`
Expected: FAIL — `ModuleNotFoundError: No module named 'aqb.volume'`.

- [ ] **Step 3: Write minimal implementation**

Create `clang/utils/analyzer/aqb/volume.py`:

```python
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
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `python3 -m unittest aqb.tests.test_volume -v`
Expected: PASS (6 tests).

- [ ] **Step 5: Format + commit**

```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb/volume.py clang/utils/analyzer/aqb/tests/test_volume.py
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "feat(aqb): Clang Volume digest, naming, and labels"
```

---

## Task 7: ccache volume + resolve-or-build orchestration

**Files:**
- Modify: `clang/utils/analyzer/aqb/volume.py`
- Test: `clang/utils/analyzer/aqb/tests/test_volume.py` (add cases)

- [ ] **Step 1: Write the failing tests**

Append to `clang/utils/analyzer/aqb/tests/test_volume.py` — add these imports at the top alongside the existing ones:

```python
from typing import Callable, List

from aqb.errors import ClangBuildError
from aqb.runtime import ProcResult, Runtime
from aqb.volume import (
    CCACHE_VOLUME,
    ClangBuildSpec,
    ensure_cache_volume,
    resolve_or_build_clang,
)
```

Then add this fake runner and the two test classes:

```python
class ScriptedRunner:
    """Fake runner routing by argv; records every call."""

    def __init__(self, handler: Callable[[List[str]], ProcResult]):
        self.calls: List[List[str]] = []
        self._handler = handler

    def __call__(self, argv: List[str]) -> ProcResult:
        self.calls.append(list(argv))
        return self._handler(argv)


def _spec() -> ClangBuildSpec:
    return ClangBuildSpec(
        commit="349146dabe4b07651d02fb",
        source="/work/llvm-project",
        commit_title="do the thing",
        cmake_args=["-DLLVM_ENABLE_ASSERTIONS=ON"],
        assertions=True,
        builder_image="aqb-clang-builder:latest",
        builder_image_id="sha256:img",
        created="2026-07-16T13:15:00+00:00",
        build_config="cmake: -DLLVM_ENABLE_ASSERTIONS=ON",
    )


def _expected_name(spec: ClangBuildSpec) -> str:
    from aqb.volume import build_config_digest, clang_volume_name

    digest = build_config_digest(spec.cmake_args, spec.assertions, spec.builder_image_id)
    return clang_volume_name(spec.commit, digest)


class CacheVolumeTest(unittest.TestCase):
    def test_creates_cache_volume_when_absent(self):
        runner = ScriptedRunner(
            lambda argv: ProcResult(1, "", "") if argv[1:3] == ["volume", "inspect"]
            else ProcResult(0, "", "")
        )
        ensure_cache_volume(Runtime("docker", runner))
        create = [c for c in runner.calls if c[1:3] == ["volume", "create"]]
        self.assertEqual(len(create), 1)
        self.assertIn("aqb.role=cache", create[0])
        self.assertIn(CCACHE_VOLUME, create[0])

    def test_noop_when_cache_volume_present(self):
        runner = ScriptedRunner(lambda argv: ProcResult(0, "", ""))
        ensure_cache_volume(Runtime("docker", runner))
        self.assertFalse([c for c in runner.calls if c[1:3] == ["volume", "create"]])


class ResolveOrBuildTest(unittest.TestCase):
    def test_reuses_existing_volume_without_building(self):
        spec = _spec()
        name = _expected_name(spec)
        runner = ScriptedRunner(
            lambda argv: ProcResult(0, "", "")  # every inspect succeeds -> present
        )
        result = resolve_or_build_clang(Runtime("docker", runner), spec)
        self.assertEqual(result, name)
        # No create and no builder run when the clang volume already exists.
        self.assertFalse([c for c in runner.calls if c[1:3] == ["volume", "create"]])
        self.assertFalse([c for c in runner.calls if c[1:2] == ["run"]])

    def test_builds_when_absent(self):
        spec = _spec()
        name = _expected_name(spec)

        def handler(argv):
            if argv[1:3] == ["volume", "inspect"]:
                # clang volume absent; ccache present.
                return ProcResult(1 if argv[3] == name else 0, "", "")
            return ProcResult(0, "", "")

        runner = ScriptedRunner(handler)
        result = resolve_or_build_clang(Runtime("docker", runner), spec)
        self.assertEqual(result, name)
        create = [c for c in runner.calls if c[1:3] == ["volume", "create"]]
        self.assertTrue(any(name in c and "aqb.commit=" + spec.commit in c for c in create))
        build = [c for c in runner.calls if c[1:2] == ["run"]]
        self.assertEqual(len(build), 1)
        joined = " ".join(build[0])
        self.assertIn(f"{name}:", joined)  # clang volume mounted
        self.assertIn(f"{CCACHE_VOLUME}:", joined)  # ccache mounted

    def test_build_failure_removes_volume_and_raises(self):
        spec = _spec()
        name = _expected_name(spec)

        def handler(argv):
            if argv[1:3] == ["volume", "inspect"]:
                return ProcResult(1 if argv[3] == name else 0, "", "")
            if argv[1:2] == ["run"]:
                return ProcResult(2, "", "compile error")
            return ProcResult(0, "", "")

        runner = ScriptedRunner(handler)
        with self.assertRaises(ClangBuildError):
            resolve_or_build_clang(Runtime("docker", runner), spec)
        removed = [c for c in runner.calls if c[1:3] == ["volume", "rm"]]
        self.assertTrue(any(name in c for c in removed))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `python3 -m unittest aqb.tests.test_volume -v`
Expected: FAIL — `ImportError: cannot import name 'ClangBuildSpec'` (and `ensure_cache_volume`, `resolve_or_build_clang`).

- [ ] **Step 3: Extend `volume.py`**

Append to `clang/utils/analyzer/aqb/volume.py`. Add these imports at the top alongside the existing ones: `from dataclasses import dataclass`, `from aqb.errors import ClangBuildError`, `from aqb.runtime import Runtime`. Then add the mount-path constants next to the existing constants and the code below:

```python
CLANG_INSTALL_MOUNT = "/opt/aqb/clang"
CCACHE_MOUNT = "/ccache"


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
    digest = build_config_digest(spec.cmake_args, spec.assertions, spec.builder_image_id)
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
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `python3 -m unittest aqb.tests.test_volume -v`
Expected: PASS (6 digest/name/label tests + 2 cache tests + 3 resolve-or-build tests = 11).

- [ ] **Step 5: Format + commit**

```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb/volume.py clang/utils/analyzer/aqb/tests/test_volume.py
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "feat(aqb): resolve-or-build Clang Volume + shared ccache volume"
```

---

## Task 8: Phase 2 green check

- [ ] **Step 1: Run the whole suite**

Run: `python3 -m unittest discover -s aqb/tests -t . -v`
Expected: PASS — all Phase 1 + Phase 2 tests (test_cli, test_runid ×3, test_metadata, test_store, test_satest, test_errors, test_integration, test_runtime ×9, test_volume ×11).

- [ ] **Step 2: Confirm formatting is clean**

Run: `python3 -m black --check aqb/`
Expected: "All done!" — no files reformatted.

- [ ] **Step 3: Smoke-check the CLI still works**

Run: `python3 -m aqb --help` and confirm it prints usage and exits 0 (no import errors from the new modules).

- [ ] **Step 4: Commit any formatting fixes (only if `black --check` reported changes)**

```bash
python3 -m black aqb/
git -C /Users/benics/git/upstream-llvm-ssaf add clang/utils/analyzer/aqb
git -C /Users/benics/git/upstream-llvm-ssaf commit -m "style(aqb): black formatting"
```

---

## Self-Review

**Spec coverage (Phase 2 scope):** runtime name resolution (flag > env > docker) → Task 4; docker-compatible CLI wrapper incl. image-digest query → Task 5; build-config digest excluding timestamp/note → Task 6; volume naming `aqb-clang-<shortcommit>-<configdigest>` + immutable labels incl. non-digested `aqb.created` and full-hash `aqb.commit` and local-path-capable `aqb.source` → Task 6; resolve-or-build flow with partial-volume cleanup on failure → Task 7; shared `aqb-ccache` volume mounted into the builder → Task 7. Folded Phase-1 follow-ups: UTC run ids → Task 1; shared `errors` module → Task 2; golden end-to-end test → Task 3. Deferred by design (documented in Scope note): the builder image + build script and the CLI wiring (`aqb run --commit`) → Phase 3.

**Placeholder scan:** No "TBD"/"handle errors"/"write tests for the above" — every task ships full test + implementation code. The builder image is explicitly deferred to Phase 3 with a documented env-var contract, not left as a vague placeholder.

**Type consistency:** `ProcResult(returncode, stdout, stderr)` is defined in Task 5 and used identically by the fakes in Task 5 and Task 7. `Runtime(name, runner)` with methods `run(args, check=)`, `volume_exists`, `create_volume(volume, labels)`, `remove_volume`, `image_id` — names match across Tasks 5 and 7. `build_config_digest(cmake_args, assertions, builder_image_id)`, `clang_volume_name(commit, config_digest)`, `clang_volume_labels(**kwargs)`, `CCACHE_VOLUME`, `ClangBuildSpec` fields, `ensure_cache_volume`, `resolve_or_build_clang` — names/signatures match between Tasks 6, 7 and their tests. `AqbError`/`RunNotFoundError`/`RuntimeCommandError`/`ClangBuildError` from Task 2 are used by Tasks 5 and 7. `resolve_runtime(cli_value)` from Task 4 is unchanged by Task 5.
