from __future__ import annotations

import argparse
import datetime
import os
import sys
from typing import List, Optional

from aqb.errors import ClangBuildError, RuntimeCommandError
from aqb.runtime import Runtime, resolve_runtime
from aqb.store import RunStore
from aqb.volume import build_clang_volume

STUB_COMMANDS = ("run", "diff", "plot", "report", "promote")


def default_home() -> str:
    return os.environ.get("AQB_HOME", os.path.join(os.getcwd(), ".aqb"))


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
    build.set_defaults(func=cmd_build_clang)

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


def main(argv: Optional[List[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    if not getattr(args, "command", None):
        parser.print_help()
        return 0
    return args.func(args)
