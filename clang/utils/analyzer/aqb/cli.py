from __future__ import annotations

import argparse
import sys
from typing import List, Optional

STUB_COMMANDS = ("run", "diff", "plot", "report", "promote")


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

    for name in STUB_COMMANDS:
        stub = sub.add_parser(name, help=f"{name} (not yet implemented)")
        stub.set_defaults(func=cmd_not_implemented, command_name=name)

    return parser


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
