from __future__ import annotations

import argparse
import os
import sys
from typing import List, Optional

from aqb.store import RunStore

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


def main(argv: Optional[List[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    if not getattr(args, "command", None):
        parser.print_help()
        return 0
    return args.func(args)
