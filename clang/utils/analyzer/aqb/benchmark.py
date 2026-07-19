from __future__ import annotations

from collections import namedtuple
from typing import Dict, List, Tuple

import pandas as pd

RUN_ENTITY = "(all)"
LEVELS = ("run", "tu", "entry-point")

# The tidy long-form columns AQB manipulates benchmark data in.
SAMPLE_COLUMNS = ["run", "iteration", "usr", "file", "debug_name", "metric", "value"]

# Lightweight value carrier handed to the SVG renderer (data manipulation is all
# pandas; this is just the drawing interface).
Candle = namedtuple("Candle", "min q1 median q3 max n")


def frame_from_samples(samples_by_run: Dict[str, List[List[dict]]]) -> pd.DataFrame:
    """Build one tidy long DataFrame from stored ``samples.json`` records across
    runs: a row per ``(run, iteration, usr, file, debug_name, metric, value)``.
    Preserves ``samples_by_run`` order in the ``run`` column's categories."""
    records = []
    for run, iterations in samples_by_run.items():
        for i, iteration in enumerate(iterations):
            for ep in iteration:
                for metric, value in ep["stats"].items():
                    records.append(
                        (run, i, ep["usr"], ep["file"], ep["debug_name"], metric, value)
                    )
    return pd.DataFrame.from_records(records, columns=SAMPLE_COLUMNS)


def inner_join_runs(df: pd.DataFrame) -> Tuple[pd.DataFrame, List[Tuple[str, str]]]:
    """Keep only ``(file, USR)`` entry points present in EVERY run (an inner join
    across runs). Transient build artifacts — CMake ``TryCompile`` probes,
    compiler-id/ABI checks — get a fresh random path each build, so they are
    never shared across runs and fall out of the join; it also keeps multi-run
    comparisons apples-to-apples.

    Returns ``(filtered_df, dropped)`` where ``dropped`` is the sorted list of
    ``(file, USR)`` removed from at least one run (so the caller can log them)."""
    if df.empty:
        return df, []

    n_runs = df["run"].nunique()
    runs_per_key = df.groupby(["file", "usr"])["run"].nunique()
    common = set(runs_per_key[runs_per_key == n_runs].index)
    dropped = sorted(set(runs_per_key.index) - common)

    keys = list(zip(df["file"], df["usr"]))
    mask = pd.Series([k in common for k in keys], index=df.index)
    return df[mask].reset_index(drop=True), dropped


def pretty_names(df: pd.DataFrame) -> Dict[str, str]:
    """Map each ``USR`` to its human-readable ``debug_name`` (first seen), for
    labelling entry-point candles in tooltips."""
    if df.empty:
        return {}
    unique = df.drop_duplicates(subset="usr")
    return dict(zip(unique["usr"], unique["debug_name"]))


def _level_values(df: pd.DataFrame, level: str) -> pd.DataFrame:
    """Per-``(run, entity, metric, iteration)`` value for a granularity: the
    USR's own value (entry-point), or the sum over entry points sharing a file
    (tu) / over all entry points (run)."""
    if level == "entry-point":
        return df.rename(columns={"usr": "entity"})[
            ["run", "entity", "metric", "iteration", "value"]
        ]
    if level == "tu":
        out = df.groupby(["run", "file", "metric", "iteration"], as_index=False)[
            "value"
        ].sum()
        return out.rename(columns={"file": "entity"})
    # run
    out = df.groupby(["run", "metric", "iteration"], as_index=False)["value"].sum()
    out["entity"] = RUN_ENTITY
    return out[["run", "entity", "metric", "iteration", "value"]]


def candle_frames(df: pd.DataFrame) -> Dict[str, pd.DataFrame]:
    """For each granularity, the five-number summary per ``(run, entity, metric)``
    over its per-iteration values. Columns: ``run, entity, metric, min, q1,
    median, q3, max, n``. Uses pandas quantiles (linear interpolation == the
    inclusive method), so a single sample collapses the box to a point."""
    empty = pd.DataFrame(
        columns=["run", "entity", "metric", "min", "q1", "median", "q3", "max", "n"]
    )
    frames: Dict[str, pd.DataFrame] = {}
    for level in LEVELS:
        values = _level_values(df, level)
        if values.empty:
            frames[level] = empty.copy()
            continue
        grouped = values.groupby(["run", "entity", "metric"])["value"]
        frames[level] = grouped.agg(
            min="min",
            q1=lambda s: s.quantile(0.25),
            median="median",
            q3=lambda s: s.quantile(0.75),
            max="max",
            n="count",
        ).reset_index()
    return frames
