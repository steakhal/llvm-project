from __future__ import annotations

import csv
import io
import json
from dataclasses import dataclass
from typing import Dict, Iterable, List

# The fixed leading columns of the per-entry-point CSV (see AQB-design.rst /
# EntryPointStats.cpp dumpStatsAsCSV).
EP_CSV_FIXED_COLUMNS = ("USR", "File", "DebugName")


@dataclass
class EntryPointMetrics:
    """Per-entry-point statistics for one analyzed function.

    ``stats`` holds only the columns that had a value; unset ``UnsignedEPStat``
    columns (empty CSV cells) are omitted.
    """

    usr: str
    file: str
    debug_name: str
    stats: Dict[str, int]


def parse_entry_point_csv(text: str) -> List[EntryPointMetrics]:
    """Parse ``dump-entry-point-stats-to-csv`` output into a list of rows."""
    rows = list(csv.reader(io.StringIO(text)))
    if not rows:
        return []
    stat_names = rows[0][len(EP_CSV_FIXED_COLUMNS) :]
    result: List[EntryPointMetrics] = []
    for row in rows[1:]:
        if not row:
            continue
        usr, file, debug_name = row[0], row[1], row[2]
        stats: Dict[str, int] = {}
        for name, cell in zip(stat_names, row[len(EP_CSV_FIXED_COLUMNS) :]):
            if cell != "":
                stats[name] = int(cell)
        result.append(
            EntryPointMetrics(usr=usr, file=file, debug_name=debug_name, stats=stats)
        )
    return result


def dedup_entry_points(
    rows: Iterable[EntryPointMetrics],
) -> List[EntryPointMetrics]:
    """Keep one row per USR (first wins) so a header analyzed through many TUs
    is not counted multiple times."""
    seen: Dict[str, EntryPointMetrics] = {}
    for row in rows:
        if row.usr not in seen:
            seen[row.usr] = row
    return list(seen.values())


def parse_tu_stats_json(text: str) -> Dict[str, float]:
    """Parse ``serialize-stats=true`` / ``PrintStatisticsJSON`` output.

    The output is a JSON object mapping ``"<debugtype>.<name>"`` to a numeric
    value. Most are integer counters, but ``PrintStatisticsJSON`` also appends
    ``TimerGroup`` values (keys like ``"time.<group>.<name>.wall"``) whose
    values are floating-point. Values are therefore preserved with their JSON
    numeric type (``int`` for counters, ``float`` for timers) rather than
    coerced to ``int`` — coercing would silently truncate sub-second timings
    to zero.
    """
    raw = json.loads(text)
    return dict(raw)
