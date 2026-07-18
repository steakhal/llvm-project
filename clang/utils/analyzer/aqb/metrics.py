from __future__ import annotations

import io
import json
from typing import Dict, List

import pandas as pd

# The fixed leading columns of the per-entry-point CSV (see AQB-design.rst /
# EntryPointStats.cpp dumpStatsAsCSV).
EP_CSV_FIXED_COLUMNS = ("USR", "File", "DebugName")


def parse_entry_point_csv(text: str) -> pd.DataFrame:
    """Parse ``dump-entry-point-stats-to-csv`` output into a wide DataFrame: one
    row per entry point, columns ``USR``, ``File``, ``DebugName`` plus one per
    stat. Empty cells become ``NaN`` (an unset ``UnsignedEPStat``). Empty input
    yields an empty frame with just the fixed columns."""
    text = text.strip()
    if not text:
        return pd.DataFrame(columns=list(EP_CSV_FIXED_COLUMNS))
    return pd.read_csv(io.StringIO(text))


def dedup_entry_points(df: pd.DataFrame) -> pd.DataFrame:
    """Keep one row per ``USR`` (first wins) so a header analyzed through many
    TUs is not counted multiple times."""
    if df.empty:
        return df
    return df.drop_duplicates(subset="USR", keep="first").reset_index(drop=True)


def to_sample_records(df: pd.DataFrame) -> List[dict]:
    """Serialize an entry-point DataFrame to the ``samples.json`` record shape
    (``{usr, file, debug_name, stats}``), omitting unset (NaN) stats and coercing
    present stats to ``int``."""
    metric_cols = [c for c in df.columns if c not in EP_CSV_FIXED_COLUMNS]
    records: List[dict] = []
    for row in df.itertuples(index=False):
        d = row._asdict()
        stats = {m: int(d[m]) for m in metric_cols if pd.notna(d[m])}
        records.append(
            {
                "usr": d["USR"],
                "file": d["File"],
                "debug_name": d["DebugName"],
                "stats": stats,
            }
        )
    return records


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
