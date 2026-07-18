from __future__ import annotations

import statistics
from dataclasses import dataclass
from typing import Dict, List, Optional, Tuple

from aqb.metrics import EntryPointMetrics

RUN_ENTITY = "(all)"


@dataclass
class Candle:
    min: float
    q1: float
    median: float
    q3: float
    max: float
    n: int


def candlestick(samples: List[float]) -> Optional[Candle]:
    """Five-number summary of a sample list; None if empty. A single sample
    collapses all five numbers to that value (``statistics.quantiles`` needs
    n>=2)."""
    if not samples:
        return None
    data = sorted(samples)
    lo, hi, med = data[0], data[-1], statistics.median(data)
    if len(data) == 1:
        return Candle(lo, lo, lo, lo, lo, 1)
    q1, _, q3 = statistics.quantiles(data, n=4, method="inclusive")
    return Candle(lo, q1, med, q3, hi, len(data))


# level -> entity -> metric -> [one value per iteration]
Aggregated = Dict[str, Dict[str, Dict[str, List[float]]]]


def aggregate_samples(iterations: List[List[EntryPointMetrics]]) -> Aggregated:
    """Aggregate per-iteration entry-point metrics up to per-run, per-TU (by
    file), and per-entry-point (by USR). Each leaf is one value per iteration
    (summed over the relevant entry points for run/TU; the USR's own value for
    entry-point). A metric absent in an iteration simply contributes no sample
    for that iteration at that level/entity."""
    agg: Aggregated = {"run": {RUN_ENTITY: {}}, "tu": {}, "entry-point": {}}

    for ep_list in iterations:
        run_totals: Dict[str, float] = {}
        tu_totals: Dict[str, Dict[str, float]] = {}
        for ep in ep_list:
            for metric, value in ep.stats.items():
                run_totals[metric] = run_totals.get(metric, 0) + value
                tu_totals.setdefault(ep.file, {})
                tu_totals[ep.file][metric] = tu_totals[ep.file].get(metric, 0) + value
                epd = agg["entry-point"].setdefault(ep.usr, {})
                epd.setdefault(metric, []).append(value)
        for metric, total in run_totals.items():
            agg["run"][RUN_ENTITY].setdefault(metric, []).append(total)
        for file, metrics in tu_totals.items():
            fd = agg["tu"].setdefault(file, {})
            for metric, total in metrics.items():
                fd.setdefault(metric, []).append(total)
    return agg


# run id -> per-iteration lists of EntryPointMetrics
RunSamples = Dict[str, List[List[EntryPointMetrics]]]


def inner_join_runs(
    samples_by_run: RunSamples,
) -> Tuple[RunSamples, List[Tuple[str, str]]]:
    """Keep only ``(file, USR)`` entry points present in EVERY run (an inner
    join across runs). Transient build artifacts — CMake ``TryCompile`` probes,
    compiler-id/ABI checks — get a fresh random path each build, so they are
    never shared across runs and fall out of the join. It also keeps multi-run
    comparisons apples-to-apples: an entry point analyzed in only some runs is
    dropped.

    Returns ``(filtered_by_run, dropped)`` where ``filtered_by_run`` preserves
    the input order/keys and ``dropped`` is the sorted list of ``(file, USR)``
    removed from at least one run (so the caller can log them)."""
    if not samples_by_run:
        return {}, []

    key_sets = []
    for iterations in samples_by_run.values():
        keys = {(ep.file, ep.usr) for iteration in iterations for ep in iteration}
        key_sets.append(keys)

    common = set.intersection(*key_sets)
    dropped = sorted(set().union(*key_sets) - common)

    filtered: RunSamples = {}
    for run_id, iterations in samples_by_run.items():
        filtered[run_id] = [
            [ep for ep in iteration if (ep.file, ep.usr) in common]
            for iteration in iterations
        ]
    return filtered, dropped
