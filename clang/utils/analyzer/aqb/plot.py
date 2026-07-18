from __future__ import annotations

import html
from typing import Dict, List

from aqb.benchmark import Aggregated, Candle, candlestick

# Distinct, print-safe colors assigned to runs by order.
_PALETTE = [
    "#1f77b4",
    "#d62728",
    "#2ca02c",
    "#9467bd",
    "#ff7f0e",
    "#17becf",
    "#8c564b",
    "#e377c2",
]

_LEVELS = [
    ("run", "Per-run"),
    ("tu", "Per-TU"),
    ("entry-point", "Per-entry-point"),
]

# Entities in the per-TU / per-entry-point charts are ordered by this metric
# (from the oldest run) so the busiest files/functions lead and the x-axis order
# is stable across every metric's chart.
ORDER_METRIC = "PathRunningTime"

_PLOT_H = 200
_PAD_LEFT = 44
_PAD_TOP = 24
_PAD_BOTTOM = 70
_CANDLE_W = 8


def _color(run_index: int) -> str:
    return _PALETTE[run_index % len(_PALETTE)]


def svg_chart(
    entities: List[str], series_by_run: Dict[str, Dict[str, Candle]], title: str
) -> str:
    """One candlestick chart: x-axis lists ``entities``; at each entity, one
    candle per run (color-coded), overlaid. ``series_by_run`` maps a run name to
    its ``{entity: Candle}``. Candles are drawn on a shared linear y-scale
    (0..max)."""
    runs = list(series_by_run.keys())
    all_candles = [c for r in runs for c in series_by_run[r].values() if c is not None]
    ymax = max((c.max for c in all_candles), default=1.0) or 1.0

    slot_w = max(44, len(runs) * (_CANDLE_W + 4) + 16)
    width = _PAD_LEFT + slot_w * max(1, len(entities)) + 16
    height = _PAD_TOP + _PLOT_H + _PAD_BOTTOM

    def y(v: float) -> float:
        return _PAD_TOP + _PLOT_H - (v / ymax) * _PLOT_H

    parts: List[str] = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" '
        f'height="{height}" class="chart">',
        f'<text x="{_PAD_LEFT}" y="14" class="title">{html.escape(title)}</text>',
        # y-axis min/max ticks
        f'<text x="4" y="{y(ymax):.1f}" class="tick">{ymax:g}</text>',
        f'<text x="4" y="{y(0):.1f}" class="tick">0</text>',
        f'<line x1="{_PAD_LEFT}" y1="{_PAD_TOP}" x2="{_PAD_LEFT}" '
        f'y2="{_PAD_TOP + _PLOT_H}" class="axis"/>',
    ]

    for ei, entity in enumerate(entities):
        slot_x = _PAD_LEFT + ei * slot_w
        for ri, run in enumerate(runs):
            candle = series_by_run[run].get(entity)
            if candle is None:
                continue
            cx = slot_x + 12 + ri * (_CANDLE_W + 4)
            color = _color(ri)
            # wick (min..max)
            parts.append(
                f'<line x1="{cx + _CANDLE_W / 2:.1f}" y1="{y(candle.max):.1f}" '
                f'x2="{cx + _CANDLE_W / 2:.1f}" y2="{y(candle.min):.1f}" '
                f'stroke="{color}"/>'
            )
            # box (q1..q3)
            box_top = y(candle.q3)
            box_h = max(1.0, y(candle.q1) - y(candle.q3))
            parts.append(
                f'<rect x="{cx:.1f}" y="{box_top:.1f}" width="{_CANDLE_W}" '
                f'height="{box_h:.1f}" fill="{color}" fill-opacity="0.35" '
                f'stroke="{color}"/>'
            )
            # median tick
            parts.append(
                f'<line x1="{cx:.1f}" y1="{y(candle.median):.1f}" '
                f'x2="{cx + _CANDLE_W:.1f}" y2="{y(candle.median):.1f}" '
                f'stroke="{color}" stroke-width="2"/>'
            )
        # x label (truncated), rotated for density
        label = entity if len(entity) <= 28 else "…" + entity[-27:]
        lx = slot_x + slot_w / 2
        parts.append(
            f'<text x="{lx:.1f}" y="{_PAD_TOP + _PLOT_H + 12:.1f}" '
            f'class="xlabel" transform="rotate(35 {lx:.1f} '
            f'{_PAD_TOP + _PLOT_H + 12:.1f})">{html.escape(label)}</text>'
        )

    # legend
    for ri, run in enumerate(runs):
        ly = _PAD_TOP + _PLOT_H + 40 + ri * 14
        parts.append(
            f'<rect x="{_PAD_LEFT}" y="{ly - 8}" width="10" height="10" '
            f'fill="{_color(ri)}"/>'
        )
        parts.append(
            f'<text x="{_PAD_LEFT + 14}" y="{ly}" class="legend">'
            f"{html.escape(run)}</text>"
        )

    parts.append("</svg>")
    return "".join(parts)


_STYLE = """
body { font-family: -apple-system, sans-serif; margin: 1.5rem; color: #222; }
h1 { font-size: 1.4rem; } h2 { font-size: 1.1rem; margin-top: 1.5rem; }
.toc a { display: inline-block; margin-right: 1rem; }
details { margin: 0.3rem 0; }
summary { cursor: pointer; font-family: monospace; }
.chartbox { overflow-x: auto; border: 1px solid #eee; margin: 0.3rem 0; }
.title { font-size: 12px; font-weight: bold; }
.tick, .xlabel, .legend { font-size: 10px; fill: #555; }
.axis { stroke: #ccc; }
"""


def _metric_union(runs: Dict[str, Aggregated], level: str) -> List[str]:
    metrics = set()
    for agg in runs.values():
        for entity_metrics in agg.get(level, {}).values():
            metrics.update(entity_metrics.keys())
    return sorted(metrics)


def _entity_has(
    runs: Dict[str, Aggregated], level: str, entity: str, metric: str
) -> bool:
    return any(metric in agg.get(level, {}).get(entity, {}) for agg in runs.values())


def _ordered_entities(runs: Dict[str, Aggregated], level: str) -> List[str]:
    """All entities at ``level`` (union across runs), ordered by the OLDEST
    run's ``ORDER_METRIC`` (median) descending; entities without that metric in
    the oldest run sort last, by name. ``runs`` must be ordered oldest-first
    (dict insertion order), which ``cmd_plot`` guarantees by sorting on
    ``metadata.created``."""
    entities = set()
    for agg in runs.values():
        entities.update(agg.get(level, {}).keys())

    oldest = next(iter(runs.values()), {})
    old_level = oldest.get(level, {})
    medians = {}
    for entity in entities:
        candle = candlestick(old_level.get(entity, {}).get(ORDER_METRIC, []))
        if candle is not None:
            medians[entity] = candle.median

    return sorted(
        entities,
        key=lambda e: (0 if e in medians else 1, -medians.get(e, 0.0), e),
    )


def render_html(runs: Dict[str, Aggregated]) -> str:
    """Render one self-contained HTML bundling candlestick charts for every
    metric at all three granularities (per-run, per-TU, per-entry-point),
    overlaying every run in ``runs``. No external assets."""
    body: List[str] = []
    toc: List[str] = []

    for level, heading in _LEVELS:
        anchor = f"level-{level}"
        toc.append(f'<a href="#{anchor}">{html.escape(heading)}</a>')
        body.append(f'<h2 id="{anchor}">{html.escape(heading)}</h2>')
        metrics = _metric_union(runs, level)
        if not metrics:
            body.append("<p><em>no data</em></p>")
            continue
        # One entity order per level (by the oldest run's ORDER_METRIC), reused
        # across every metric chart so the x-axis stays stable.
        ordered = _ordered_entities(runs, level)
        for metric in metrics:
            entities = [e for e in ordered if _entity_has(runs, level, e, metric)]
            series_by_run: Dict[str, Dict[str, Candle]] = {}
            for run_name, agg in runs.items():
                level_data = agg.get(level, {})
                series_by_run[run_name] = {
                    entity: candlestick(level_data.get(entity, {}).get(metric, []))
                    for entity in entities
                    if metric in level_data.get(entity, {})
                }
            chart = svg_chart(entities, series_by_run, title=metric)
            body.append(
                f"<details><summary>{html.escape(metric)}</summary>"
                f'<div class="chartbox">{chart}</div></details>'
            )

    run_names = ", ".join(html.escape(r) for r in runs)
    return (
        "<!doctype html><html><head><meta charset='utf-8'>"
        f"<title>AQB benchmark plot</title><style>{_STYLE}</style></head><body>"
        f"<h1>AQB benchmark plot</h1><p>Runs: {run_names}</p>"
        f'<div class="toc">{"".join(toc)}</div>'
        f'{"".join(body)}'
        "</body></html>"
    )
