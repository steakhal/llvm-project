from __future__ import annotations

import html
from typing import Dict, List

import pandas as pd

from aqb.benchmark import Candle

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


def _fmt(v: float) -> str:
    """Format a stat value plainly (never scientific notation); integers show
    without a trailing ``.0``."""
    v = float(v)
    if v.is_integer():
        return str(int(v))
    return f"{v:.6f}".rstrip("0").rstrip(".")


def svg_chart(
    entities: List[str],
    series_by_run: Dict[str, Dict[str, Candle]],
    title: str,
    labels: Dict[str, str] = None,
) -> str:
    """One candlestick chart: x-axis lists ``entities``; at each entity, one
    candle per run (color-coded), overlaid. ``series_by_run`` maps a run name to
    its ``{entity: Candle}``. Candles are drawn on a linear y-scale (0..max) by
    default; each candle carries its raw five-number values (``data-*``) and the
    chart carries its geometry (``data-ph``/``data-pt``/``data-ymax``) so the
    inline log-toggle script can recompute y client-side. Each candle also
    carries ``data-tip`` (styled multi-line hover tooltip HTML) and ``data-copy``
    (plain text copied to the clipboard on click)."""
    runs = list(series_by_run.keys())
    all_candles = [c for r in runs for c in series_by_run[r].values() if c is not None]
    ymax = max((c.max for c in all_candles), default=1.0) or 1.0
    labels = labels or {}

    slot_w = max(44, len(runs) * (_CANDLE_W + 4) + 16)
    width = _PAD_LEFT + slot_w * max(1, len(entities)) + 16
    height = _PAD_TOP + _PLOT_H + _PAD_BOTTOM

    def y(v: float) -> float:
        return _PAD_TOP + _PLOT_H - (v / ymax) * _PLOT_H

    parts: List[str] = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" '
        f'height="{height}" class="chart" data-ph="{_PLOT_H}" '
        f'data-pt="{_PAD_TOP}" data-ymax="{_fmt(ymax)}">',
        f'<text x="{_PAD_LEFT}" y="14" class="title">{html.escape(title)}</text>',
        # y-axis min/max ticks
        f'<text x="4" y="{y(ymax):.1f}" class="tick">{_fmt(ymax)}</text>',
        f'<text x="4" y="{y(0):.1f}" class="tick">0</text>',
        f'<line x1="{_PAD_LEFT}" y1="{_PAD_TOP}" x2="{_PAD_LEFT}" '
        f'y2="{_PAD_TOP + _PLOT_H}" class="axis"/>',
    ]

    for ei, entity in enumerate(entities):
        slot_x = _PAD_LEFT + ei * slot_w
        label = labels.get(entity, entity)
        for ri, run in enumerate(runs):
            candle = series_by_run[run].get(entity)
            if candle is None:
                continue
            cx = slot_x + 12 + ri * (_CANDLE_W + 4)
            color = _color(ri)
            mid = cx + _CANDLE_W / 2
            # Hover tooltip content: the entity's pretty name, which run, the
            # metric, then the five-number summary ordered max..min. Values are
            # split into integer/fraction cells so decimal points align across
            # rows (right-aligned int column flush against a left-aligned frac
            # column). Pre-escaped; the tooltip script renders data-tip as
            # innerHTML and copies data-copy (plain text) on click.
            esc = html.escape
            stats = [
                ("max", candle.max),
                ("q3", candle.q3),
                ("median", candle.median),
                ("q1", candle.q1),
                ("min", candle.min),
            ]
            rows = []
            for k, v in stats:
                intp, dot, frac = _fmt(v).partition(".")
                # Single-quoted class attrs: this markup is embedded inside the
                # double-quoted data-tip attribute, so double quotes here would
                # terminate it and corrupt the tooltip.
                rows.append(
                    f"<tr><td class='lbl'>{k}</td><td class='int'>{intp}</td>"
                    f"<td class='frac'>{dot + frac}</td></tr>"
                )
            tip = (
                f"<b>{esc(label)}</b><br>run {esc(run)}<br>{esc(title)}"
                f'<table>{"".join(rows)}</table>'
            )
            copy = "&#10;".join(
                [esc(label), f"run {esc(run)}", esc(title)]
                + [f"{k} {_fmt(v)}" for k, v in stats]
            )
            # Raw values on the group so the log-toggle script can reposition
            # (plain decimals, JS-parseable — never scientific).
            parts.append(
                f'<g class="candle" data-lo="{_fmt(candle.min)}" '
                f'data-q1="{_fmt(candle.q1)}" data-md="{_fmt(candle.median)}" '
                f'data-q3="{_fmt(candle.q3)}" data-hi="{_fmt(candle.max)}" '
                f'data-tip="{tip}" data-copy="{copy}">'
            )
            # wick (min..max)
            parts.append(
                f'<line class="wick" x1="{mid:.1f}" y1="{y(candle.max):.1f}" '
                f'x2="{mid:.1f}" y2="{y(candle.min):.1f}" stroke="{color}"/>'
            )
            # box (q1..q3)
            box_top = y(candle.q3)
            box_h = max(1.0, y(candle.q1) - y(candle.q3))
            parts.append(
                f'<rect class="box" x="{cx:.1f}" y="{box_top:.1f}" '
                f'width="{_CANDLE_W}" height="{box_h:.1f}" fill="{color}" '
                f'fill-opacity="0.35" stroke="{color}"/>'
            )
            # median tick
            parts.append(
                f'<line class="median" x1="{cx:.1f}" y1="{y(candle.median):.1f}" '
                f'x2="{cx + _CANDLE_W:.1f}" y2="{y(candle.median):.1f}" '
                f'stroke="{color}" stroke-width="2"/>'
            )
            parts.append("</g>")
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
.logtoggle { font-size: 11px; margin-left: 0.5rem; cursor: pointer; color: #555; }
.candle { cursor: pointer; }
#aqb-tip {
  position: fixed; display: none; pointer-events: none; z-index: 10;
  background: #222d; color: #fff; padding: 5px 8px; border-radius: 4px;
  font: 11px/1.5 ui-monospace, monospace; white-space: nowrap;
  box-shadow: 0 1px 6px rgba(0, 0, 0, 0.35);
}
#aqb-tip b { font-size: 12px; }
#aqb-tip table { border-collapse: collapse; margin-top: 3px; }
#aqb-tip td { padding: 0; }
#aqb-tip td.lbl { color: #bbb; padding-right: 14px; }
#aqb-tip td.int { text-align: right; }
#aqb-tip td.frac { text-align: left; }
#aqb-tip.copied { outline: 2px solid #4caf50; }
"""

# Inline (self-contained) script: link horizontal scrolling of all charts that
# share a data-sync group (one per level), so scrolling any per-TU chart scrolls
# every per-TU chart in lockstep and their columns stay aligned (same for
# per-entry-point and per-run). Charts within a level share an identical x-axis,
# so equal scrollLeft lines the columns up.
#
# Propagation is coalesced into ONE requestAnimationFrame per frame (not per
# scroll event) and skips panels already at the target offset — otherwise
# writing scrollLeft to dozens of very wide panels on every scroll tick thrashes
# layout and makes scrolling stutter. Listeners are passive so scrolling is
# never blocked on JS.
_SYNC_SCRIPT = """
<script>
(function () {
  function link(group) {
    var boxes = Array.prototype.slice.call(
      document.querySelectorAll('.chartbox[data-sync="' + group + '"]'));
    var active = null, target = 0, pending = false;
    function apply() {
      pending = false;
      for (var i = 0; i < boxes.length; i++) {
        if (boxes[i] !== active && boxes[i].scrollLeft !== target) {
          boxes[i].scrollLeft = target;
        }
      }
    }
    boxes.forEach(function (box) {
      box.addEventListener('scroll', function () {
        active = box;
        target = box.scrollLeft;
        if (!pending) {
          pending = true;
          requestAnimationFrame(apply);
        }
      }, { passive: true });
    });
  }
  ['run', 'tu', 'entry-point'].forEach(link);
})();
</script>
"""

# Inline (self-contained) script: each chart's "log scale" checkbox recomputes
# its candles' y-coordinates client-side. Python renders the linear scale; the
# raw five-number values live on each `.candle` (data-lo/q1/md/q3/hi) and the
# chart geometry on the `<svg>` (data-ph/pt/ymax). Log uses log1p so zeros map to
# the baseline. Unchecking restores linear.
_LOG_SCRIPT = """
<script>
(function () {
  function rescale(svg, useLog) {
    var ph = +svg.dataset.ph, pt = +svg.dataset.pt, ymax = +svg.dataset.ymax || 1;
    function Y(v) {
      if (useLog) return pt + ph - (Math.log(v + 1) / Math.log(ymax + 1)) * ph;
      return pt + ph - (v / ymax) * ph;
    }
    svg.querySelectorAll('.candle').forEach(function (g) {
      var d = g.dataset;
      var wick = g.querySelector('.wick'),
          box = g.querySelector('.box'),
          med = g.querySelector('.median');
      wick.setAttribute('y1', Y(+d.hi).toFixed(1));
      wick.setAttribute('y2', Y(+d.lo).toFixed(1));
      var yt = Y(+d.q3), yb = Y(+d.q1);
      box.setAttribute('y', yt.toFixed(1));
      box.setAttribute('height', Math.max(1, yb - yt).toFixed(1));
      med.setAttribute('y1', Y(+d.md).toFixed(1));
      med.setAttribute('y2', Y(+d.md).toFixed(1));
    });
  }
  document.querySelectorAll('.logtoggle input').forEach(function (cb) {
    cb.addEventListener('change', function () {
      var svg = cb.closest('details').querySelector('svg');
      if (svg) rescale(svg, cb.checked);
    });
  });
})();
</script>
"""

# Inline (self-contained) instant tooltip: a single floating box (one delegated
# listener set, not per-candle) that shows a candle's ``data-tip`` on hover and
# follows the cursor. Rendered as innerHTML — safe because the server pre-escapes
# every field and only injects literal <b>/<br> tags.
_TIP_SCRIPT = """
<script>
(function () {
  var tip = document.getElementById('aqb-tip');
  var current = null;
  function place(x, y) {
    var w = tip.offsetWidth, h = tip.offsetHeight;
    var nx = x + 14, ny = y + 14;
    if (nx + w > window.innerWidth) nx = x - w - 14;
    if (ny + h > window.innerHeight) ny = y - h - 14;
    tip.style.left = nx + 'px';
    tip.style.top = ny + 'px';
  }
  function candleOf(t) {
    return t && t.closest ? t.closest('.candle') : null;
  }
  function copy(text) {
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(text).catch(fallback);
    } else {
      fallback();
    }
    function fallback() {
      var ta = document.createElement('textarea');
      ta.value = text;
      ta.style.position = 'fixed';
      ta.style.opacity = '0';
      document.body.appendChild(ta);
      ta.select();
      try { document.execCommand('copy'); } catch (e) {}
      document.body.removeChild(ta);
    }
  }
  document.addEventListener('mouseover', function (e) {
    var g = candleOf(e.target);
    if (!g) return;
    current = g;
    tip.innerHTML = g.getAttribute('data-tip') || '';
    tip.style.display = 'block';
    place(e.clientX, e.clientY);
  });
  document.addEventListener('mousemove', function (e) {
    if (current) place(e.clientX, e.clientY);
  });
  document.addEventListener('mouseout', function (e) {
    if (candleOf(e.target)) {
      current = null;
      tip.style.display = 'none';
    }
  });
  document.addEventListener('click', function (e) {
    var g = candleOf(e.target);
    if (!g) return;
    copy(g.getAttribute('data-copy') || '');
    tip.classList.add('copied');
    setTimeout(function () { tip.classList.remove('copied'); }, 600);
  });
})();
</script>
"""


def _ordered_entities(cf: pd.DataFrame, oldest_run: str) -> List[str]:
    """Entities in a level's candle frame ``cf``, ordered by ``oldest_run``'s
    ``ORDER_METRIC`` median descending; entities without that metric in the
    oldest run sort last, by name. One order per level, reused across every
    metric chart so the columns line up."""
    entities = list(cf["entity"].unique())
    pt = cf[(cf["run"] == oldest_run) & (cf["metric"] == ORDER_METRIC)]
    medians = dict(zip(pt["entity"], pt["median"]))
    return sorted(
        entities,
        key=lambda e: (0 if e in medians else 1, -float(medians.get(e, 0.0)), e),
    )


def _series_by_run(
    sub: pd.DataFrame, run_order: List[str]
) -> Dict[str, Dict[str, Candle]]:
    """From one metric's candle rows, build ``{run: {entity: Candle}}`` in
    ``run_order`` (so candle colors stay stable across charts)."""
    series: Dict[str, Dict[str, Candle]] = {run: {} for run in run_order}
    for row in sub.itertuples(index=False):
        series.setdefault(row.run, {})[row.entity] = Candle(
            row.min, row.q1, row.median, row.q3, row.max, int(row.n)
        )
    return series


def render_html(
    frames: Dict[str, pd.DataFrame],
    run_order: List[str],
    names: Dict[str, str] = None,
) -> str:
    """Render one self-contained HTML bundling candlestick charts for every
    metric at all three granularities (per-run, per-TU, per-entry-point),
    overlaying the runs in ``run_order``. ``frames`` maps each level to its
    candle DataFrame (see ``benchmark.candle_frames``). ``names`` maps a USR to
    its pretty ``debug_name`` for entry-point hover tooltips. No external
    assets."""
    body: List[str] = []
    toc: List[str] = []
    oldest_run = run_order[0] if run_order else ""
    names = names or {}

    for level, heading in _LEVELS:
        anchor = f"level-{level}"
        toc.append(f'<a href="#{anchor}">{html.escape(heading)}</a>')
        body.append(f'<h2 id="{anchor}">{html.escape(heading)}</h2>')
        cf = frames.get(level)
        if cf is None or cf.empty:
            body.append("<p><em>no data</em></p>")
            continue
        # Entry points get pretty names in tooltips; TU/run entities are already
        # human-readable (the file path / the whole-run marker).
        labels = names if level == "entry-point" else None
        # One entity order per level (by the oldest run's ORDER_METRIC), and the
        # SAME full entity list for every metric chart at this level — so slots
        # line up column-for-column and horizontal scrolling can be synced.
        ordered = _ordered_entities(cf, oldest_run)
        for metric in sorted(cf["metric"].unique()):
            series_by_run = _series_by_run(cf[cf["metric"] == metric], run_order)
            chart = svg_chart(ordered, series_by_run, title=metric, labels=labels)
            body.append(
                f"<details><summary>{html.escape(metric)}</summary>"
                '<label class="logtoggle"><input type="checkbox"> log scale</label>'
                f'<div class="chartbox" data-sync="{level}">{chart}</div></details>'
            )

    run_names = ", ".join(html.escape(r) for r in run_order)
    return (
        "<!doctype html><html><head><meta charset='utf-8'>"
        f"<title>AQB benchmark plot</title><style>{_STYLE}</style></head><body>"
        f"<h1>AQB benchmark plot</h1><p>Runs: {run_names}</p>"
        f'<div class="toc">{"".join(toc)}</div>'
        f'{"".join(body)}'
        '<div id="aqb-tip"></div>'
        f"{_SYNC_SCRIPT}"
        f"{_LOG_SCRIPT}"
        f"{_TIP_SCRIPT}"
        "</body></html>"
    )
