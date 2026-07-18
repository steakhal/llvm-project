from __future__ import annotations

import argparse
import datetime
import json
import os
import sys
from typing import Dict, List, Optional

from aqb.diff import diff_runs, summarize, verdict
from aqb.errors import ClangBuildError, RunNotFoundError, RuntimeCommandError
from aqb.normalize import Finding
from aqb.run import perform_run
from aqb.runtime import Runtime, resolve_runtime
from aqb.store import RunStore
from aqb.volume import build_clang_volume

STUB_COMMANDS = ("plot", "report", "promote")


def default_home() -> str:
    return os.environ.get("AQB_HOME", os.path.join(os.getcwd(), ".aqb"))


def _analyzer_dir() -> str:
    """The in-tree ``clang/utils/analyzer`` dir (parent of the ``aqb`` package):
    it holds ``SATest`` modules + ``aqb/`` (mounted at ``/scripts``) and the
    ``projects/`` corpus recipes + ``projects.json``."""
    return os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="aqb", description="Analysis Qualification Bench"
    )
    parser.add_argument(
        "--home",
        default=None,
        help="AQB data directory (default: $AQB_HOME or ./.aqb)",
    )
    sub = parser.add_subparsers(dest="command")

    list_parser = sub.add_parser("list", help="list stored runs")
    list_parser.set_defaults(func=cmd_list)

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
        "--preset",
        default="aqb-base",
        help="configure-preset name to build (default: aqb-base)",
    )
    build.add_argument(
        "--preset-file",
        default=None,
        help="path to a CMakeUserPresets.json overlay (may inherit aqb-base)",
    )
    build.add_argument(
        "--builder-image",
        default="aqb-clang-builder:latest",
        help="builder image ref",
    )
    build.add_argument(
        "--runtime",
        default=None,
        help="container runtime (default: $AQB_RUNTIME or docker)",
    )
    build.add_argument(
        "--memory", default="24G", help="builder container memory limit (default: 24G)"
    )
    build.add_argument(
        "--cpus", default="8", help="builder container CPU limit (default: 8)"
    )
    build.set_defaults(func=cmd_build_clang)

    run = sub.add_parser(
        "run", help="analyze the pinned corpus with a Clang Volume and store a run"
    )
    run.add_argument("--commit", required=True, help="analyzer commit to analyze with")
    run.add_argument(
        "--source", required=True, help="git remote URL or absolute local clone path"
    )
    run.add_argument(
        "--commit-title", default="", help="commit subject line (provenance)"
    )
    run.add_argument(
        "--preset",
        default="aqb-base",
        help="configure-preset name to build (default: aqb-base)",
    )
    run.add_argument(
        "--preset-file",
        default=None,
        help="path to a CMakeUserPresets.json overlay (may inherit aqb-base)",
    )
    run.add_argument(
        "--builder-image", default="aqb-clang-builder:latest", help="builder image ref"
    )
    run.add_argument(
        "--projects", default="", help="comma-separated project names (default: all)"
    )
    run.add_argument(
        "--extra-analyzer-config",
        dest="extra_analyzer_config",
        default="",
        help="extra -analyzer-config options (comma-separated key=val)",
    )
    run.add_argument(
        "--runtime",
        default=None,
        help="container runtime (default: $AQB_RUNTIME or docker)",
    )
    run.add_argument(
        "--memory", default="24G", help="analyze container memory limit (default: 24G)"
    )
    run.add_argument(
        "--cpus", default="8", help="analyze container CPU limit (default: 8)"
    )
    run.add_argument(
        "--bench",
        action="store_true",
        help="benchmark run: N analyze iterations, metric distributions, no reports",
    )
    run.add_argument(
        "-n",
        "--iterations",
        type=int,
        default=1,
        help="benchmark iteration count (requires --bench; must be >= 2)",
    )
    run.set_defaults(func=cmd_run)

    diff = sub.add_parser(
        "diff", help="compare two stored runs' reports and apply an --expect verdict"
    )
    diff.add_argument("--base", required=True, help="base run id (or id prefix)")
    diff.add_argument("--new", required=True, help="new run id (or id prefix)")
    diff.add_argument(
        "--expect",
        choices=["no-crashes", "same-count", "same-reports"],
        default="no-crashes",
        help="verdict policy for the exit code (default: no-crashes)",
    )
    diff.add_argument(
        "--format", choices=["text", "json"], default="text", help="output format"
    )
    diff.set_defaults(func=cmd_diff)

    for name in STUB_COMMANDS:
        stub = sub.add_parser(name, help=f"{name} (not yet implemented)")
        stub.set_defaults(func=cmd_not_implemented, command_name=name)

    return parser


def cmd_list(args: argparse.Namespace) -> int:
    store = RunStore(args.home or default_home())
    for run_id in store.list_runs():
        print(run_id)
    return 0


def cmd_not_implemented(args: argparse.Namespace) -> int:
    print(f"aqb {args.command_name}: not yet implemented", file=sys.stderr)
    return 2


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
            memory=args.memory,
            cpus=args.cpus,
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


def cmd_run(args: argparse.Namespace) -> int:
    overlay = None
    if args.preset_file:
        with open(args.preset_file) as handle:
            overlay = handle.read()
    if args.bench and args.iterations < 2:
        print("aqb run: --bench requires -n/--iterations >= 2", file=sys.stderr)
        return 2
    kind = "benchmark" if args.bench else "functional"
    iterations = args.iterations if args.bench else 1
    runtime = Runtime(resolve_runtime(args.runtime))
    analyzer_dir = _analyzer_dir()
    names = [n for n in args.projects.split(",") if n]
    try:
        run_path = perform_run(
            runtime=runtime,
            home=args.home or default_home(),
            commit=args.commit,
            source=args.source,
            projects_src=os.path.join(analyzer_dir, "projects"),
            scripts_dir=analyzer_dir,
            project_names=names,
            commit_title=args.commit_title,
            preset=args.preset,
            user_overlay_json=overlay,
            builder_image=args.builder_image,
            memory=args.memory,
            cpus=args.cpus,
            extra_config=args.extra_analyzer_config,
            kind=kind,
            iterations=iterations,
        )
    except (ClangBuildError, RuntimeCommandError) as exc:
        print(f"aqb run: {exc}", file=sys.stderr)
        return 1
    print(os.path.basename(run_path))
    print(run_path, file=sys.stderr)
    return 0


def _load_findings_json(store: RunStore, run_id: str) -> Dict[str, List[Finding]]:
    """Load a stored run's ``reports/findings.json`` back into ``Finding`` objects,
    keyed by project."""
    path = os.path.join(store.runs_dir, run_id, "reports", "findings.json")
    with open(path) as handle:
        raw = json.load(handle)
    return {project: [Finding(**row) for row in rows] for project, rows in raw.items()}


def cmd_diff(args: argparse.Namespace) -> int:
    store = RunStore(args.home or default_home())
    try:
        base_id = store.resolve(args.base)
        new_id = store.resolve(args.new)
    except RunNotFoundError as exc:
        print(f"aqb diff: {exc}", file=sys.stderr)
        return 1

    deltas = diff_runs(
        _load_findings_json(store, base_id), _load_findings_json(store, new_id)
    )
    summary = summarize(deltas)
    passed, reason = verdict(summary, args.expect)

    if args.format == "json":
        print(
            json.dumps(
                {
                    "base": base_id,
                    "new": new_id,
                    "summary": summary,
                    "expect": args.expect,
                    "passed": passed,
                    "reason": reason,
                },
                indent=2,
                sort_keys=True,
            )
        )
    else:
        print(f"diff {base_id} -> {new_id}")
        print(
            f"  common={summary['common']} added={summary['added']} "
            f"removed={summary['removed']} changed={summary['changed']}"
        )
        print(f"  expect {args.expect}: {'PASS' if passed else 'FAIL'} ({reason})")
    return 0 if passed else 1


def main(argv: Optional[List[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    if not getattr(args, "command", None):
        parser.print_help()
        return 0
    return args.func(args)
