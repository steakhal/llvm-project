#!/usr/bin/env python3
"""Interactive CallGraph Visualizer for SSAF-extracted call graphs.

Usage:
    python3 visualize_callgraph.py <callgraph.json> [--port PORT]

Opens a browser with an interactive Cytoscape.js-based graph explorer.
No external Python dependencies required.
"""

import argparse
import json
import os
import sys
import threading
import webbrowser
from http.server import HTTPServer, BaseHTTPRequestHandler
from typing import Dict, List, Optional, Set, Tuple
from urllib.parse import urlparse, parse_qs


# ---------------------------------------------------------------------------
# Data model
# ---------------------------------------------------------------------------

class CallGraph:
    """In-memory call graph built from an SSAF JSON export."""

    def __init__(self, path: str):
        print(f"Loading {path} ...", end=" ", flush=True)
        with open(path) as f:
            raw = json.load(f)
        print("done.")

        # id -> pretty_name, def, direct_callees, virtual_callees
        self.nodes = {}  # type: Dict[int, dict]
        # id -> usr
        self.id_to_usr = {}  # type: Dict[int, str]
        # id -> linkage type
        self.id_to_linkage = {}  # type: Dict[int, str]
        # reverse index: callee_id -> set of caller ids
        self.callers = {}  # type: Dict[int, Set[int]]
        # search index: lowered pretty_name fragments
        self._search_index = []  # type: List[Tuple[str, int]]

        # Build id -> usr mapping
        for entry in raw.get("id_table", []):
            self.id_to_usr[entry["id"]] = entry["name"].get("usr", "")

        # Build id -> linkage mapping
        for entry in raw.get("linkage_table", []):
            self.id_to_linkage[entry["id"]] = entry["linkage"].get("type", "Unknown")

        # Build node data
        for tu in raw["data"]:
            for entity in tu["summary_data"]:
                eid = entity["entity_id"]
                summary = entity["entity_summary"]
                direct = [c["@"] for c in summary.get("direct_callees", [])]
                virtual = [c["@"] for c in summary.get("virtual_callees", [])]
                self.nodes[eid] = {
                    "id": eid,
                    "pretty_name": summary.get("pretty_name", f"<entity {eid}>"),
                    "def": summary.get("def"),
                    "direct_callees": direct,
                    "virtual_callees": virtual,
                }
                # Reverse index
                for cid in direct:
                    self.callers.setdefault(cid, set()).add(eid)
                for cid in virtual:
                    self.callers.setdefault(cid, set()).add(eid)

        # Search index
        for eid, node in self.nodes.items():
            self._search_index.append((node["pretty_name"].lower(), eid))

        # Collect unique top-level directories for color coding
        dirs = set()
        for node in self.nodes.values():
            d = node.get("def")
            if d and d.get("file"):
                parts = d["file"].split("/")
                # Use a short prefix: first 3 meaningful path components
                dirs.add("/".join(parts[:5]))
        self.directories = sorted(dirs)

        print(f"Graph loaded: {len(self.nodes)} nodes, "
              f"{sum(len(n['direct_callees']) + len(n['virtual_callees']) for n in self.nodes.values())} edges")

    def search(self, query: str, limit: int = 50) -> list:
        q = query.lower()
        # Rank: 0 = exact match, 1 = prefix, 2 = word-boundary match, 3 = substring
        buckets = ([], [], [], [])  # type: tuple
        for name_lower, eid in self._search_index:
            if q not in name_lower:
                continue
            entry = self._node_brief(self.nodes[eid])
            if name_lower == q:
                buckets[0].append(entry)
            elif name_lower.startswith(q):
                buckets[1].append(entry)
            elif self._at_word_boundary(name_lower, q):
                buckets[2].append(entry)
            else:
                buckets[3].append(entry)
        results = []
        for bucket in buckets:
            remaining = limit - len(results)
            if remaining <= 0:
                break
            results.extend(bucket[:remaining])
        return results

    @staticmethod
    def _at_word_boundary(haystack: str, needle: str) -> bool:
        """Check if needle appears at a word boundary (after ::, space, <, (, etc.)."""
        idx = haystack.find(needle)
        while idx >= 0:
            if idx == 0:
                return True
            prev = haystack[idx - 1]
            if prev in (':', ' ', '<', '(', ',', '*', '&', '['):
                return True
            idx = haystack.find(needle, idx + 1)
        return False

    def get_node(self, eid: int) -> Optional[dict]:
        node = self.nodes.get(eid)
        if node is None:
            return None
        return self._node_detail(node)

    def expand(self, eid: int) -> Optional[dict]:
        """Return the node + all immediate neighbors as a Cytoscape.js elements blob."""
        node = self.nodes.get(eid)
        if node is None:
            return None

        cy_nodes = []
        cy_edges = []
        seen_nodes = set()

        def add_cy_node(nid):
            if nid in seen_nodes:
                return
            seen_nodes.add(nid)
            n = self.nodes.get(nid)
            if n is None:
                # Reference to entity not in this TU -- create a placeholder
                cy_nodes.append({
                    "data": {
                        "id": str(nid),
                        "label": self.id_to_usr.get(nid, f"?{nid}"),
                        "short_label": _short_name(self.id_to_usr.get(nid, f"?{nid}")),
                        "file": "",
                        "line": 0,
                        "linkage": self.id_to_linkage.get(nid, ""),
                        "direct_count": 0,
                        "virtual_count": 0,
                        "caller_count": 0,
                        "is_placeholder": True,
                    }
                })
                return
            d = n.get("def") or {}
            cy_nodes.append({
                "data": {
                    "id": str(nid),
                    "label": n["pretty_name"],
                    "short_label": _short_name(n["pretty_name"]),
                    "file": d.get("file", ""),
                    "line": d.get("line", 0),
                    "linkage": self.id_to_linkage.get(nid, ""),
                    "direct_count": len(n["direct_callees"]),
                    "virtual_count": len(n["virtual_callees"]),
                    "caller_count": len(self.callers.get(nid, set())),
                    "is_placeholder": False,
                }
            })

        add_cy_node(eid)

        # Callees
        for cid in node["direct_callees"]:
            add_cy_node(cid)
            cy_edges.append({
                "data": {"source": str(eid), "target": str(cid), "edge_type": "direct"}
            })
        for cid in node["virtual_callees"]:
            add_cy_node(cid)
            cy_edges.append({
                "data": {"source": str(eid), "target": str(cid), "edge_type": "virtual"}
            })

        # Callers
        for caller_id in self.callers.get(eid, set()):
            add_cy_node(caller_id)
            caller_node = self.nodes.get(caller_id)
            if caller_node:
                edge_type = "virtual" if eid in caller_node["virtual_callees"] else "direct"
            else:
                edge_type = "direct"
            cy_edges.append({
                "data": {"source": str(caller_id), "target": str(eid), "edge_type": edge_type}
            })

        return {"nodes": cy_nodes, "edges": cy_edges}

    def stats(self) -> dict:
        total_direct = sum(len(n["direct_callees"]) for n in self.nodes.values())
        total_virtual = sum(len(n["virtual_callees"]) for n in self.nodes.values())
        return {
            "total_nodes": len(self.nodes),
            "total_direct_edges": total_direct,
            "total_virtual_edges": total_virtual,
            "total_edges": total_direct + total_virtual,
        }

    def _node_brief(self, node: dict) -> dict:
        d = node.get("def") or {}
        return {
            "id": node["id"],
            "pretty_name": node["pretty_name"],
            "file": d.get("file", ""),
            "line": d.get("line", 0),
        }

    def _node_detail(self, node: dict) -> dict:
        d = node.get("def") or {}
        return {
            "id": node["id"],
            "pretty_name": node["pretty_name"],
            "file": d.get("file", ""),
            "line": d.get("line", 0),
            "col": d.get("col", 0),
            "usr": self.id_to_usr.get(node["id"], ""),
            "linkage": self.id_to_linkage.get(node["id"], ""),
            "direct_callees": len(node["direct_callees"]),
            "virtual_callees": len(node["virtual_callees"]),
            "callers": len(self.callers.get(node["id"], set())),
        }


def _short_name(name: str) -> str:
    """Shorten a pretty name for display as a node label."""
    # Remove parameter list if present for shorter labels
    paren = name.find("(")
    if paren > 0:
        base = name[:paren]
    else:
        base = name
    # Take last component if qualified
    if "::" in base:
        base = base.rsplit("::", 1)[-1]
    if len(base) > 40:
        base = base[:37] + "..."
    return base


# ---------------------------------------------------------------------------
# HTTP handler
# ---------------------------------------------------------------------------

class Handler(BaseHTTPRequestHandler):
    graph: CallGraph  # set on the class before starting the server

    def do_GET(self):
        parsed = urlparse(self.path)
        path = parsed.path
        params = parse_qs(parsed.query)

        if path == "/":
            self._serve_html()
        elif path == "/api/search":
            q = params.get("q", [""])[0]
            limit = int(params.get("limit", ["50"])[0])
            self._json_response(self.graph.search(q, limit))
        elif path.startswith("/api/node/"):
            try:
                eid = int(path.split("/")[-1])
            except ValueError:
                self._json_response({"error": "bad id"}, 400)
                return
            result = self.graph.get_node(eid)
            if result is None:
                self._json_response({"error": "not found"}, 404)
            else:
                self._json_response(result)
        elif path.startswith("/api/expand/"):
            try:
                eid = int(path.split("/")[-1])
            except ValueError:
                self._json_response({"error": "bad id"}, 400)
                return
            result = self.graph.expand(eid)
            if result is None:
                self._json_response({"error": "not found"}, 404)
            else:
                self._json_response(result)
        elif path == "/api/stats":
            self._json_response(self.graph.stats())
        else:
            self.send_error(404)

    def _json_response(self, data, code=200):
        body = json.dumps(data).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def _serve_html(self):
        body = HTML_PAGE.encode()
        self.send_response(200)
        self.send_header("Content-Type", "text/html; charset=utf-8")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, format, *args):
        # Suppress per-request logging
        pass


# ---------------------------------------------------------------------------
# Embedded HTML / JS / CSS
# ---------------------------------------------------------------------------

HTML_PAGE = r"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>SSAF Call Graph Explorer</title>
<script src="https://unpkg.com/cytoscape@3.30.4/dist/cytoscape.min.js"></script>
<script src="https://unpkg.com/dagre@0.8.5/dist/dagre.min.js"></script>
<script src="https://unpkg.com/cytoscape-dagre@2.5.0/cytoscape-dagre.js"></script>
<style>
* { margin: 0; padding: 0; box-sizing: border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, 'SF Pro Text', 'Helvetica Neue', sans-serif; background: #1a1a2e; color: #e0e0e0; display: flex; height: 100vh; overflow: hidden; }

/* Sidebar */
#sidebar { width: 350px; min-width: 350px; background: #16213e; display: flex; flex-direction: column; border-right: 1px solid #0f3460; }
#sidebar h1 { font-size: 15px; padding: 14px 16px; background: #0f3460; letter-spacing: 0.5px; }
#search-box { padding: 10px 12px; border-bottom: 1px solid #0f3460; }
#search-input { width: 100%; padding: 8px 12px; border: 1px solid #0f3460; border-radius: 6px; background: #1a1a2e; color: #e0e0e0; font-size: 14px; outline: none; }
#search-input:focus { border-color: #e94560; }
#search-input::placeholder { color: #555; }
#search-results { flex: 1; overflow-y: auto; padding: 4px 0; }
.search-result { padding: 8px 14px; cursor: pointer; border-bottom: 1px solid #0f3460; font-size: 13px; transition: background 0.1s; }
.search-result:hover { background: #1a1a2e; }
.search-result .name { color: #e94560; font-weight: 500; word-break: break-all; }
.search-result .loc { color: #888; font-size: 11px; margin-top: 2px; }

/* Info panel */
#info-panel { max-height: 280px; overflow-y: auto; border-top: 1px solid #0f3460; padding: 12px 14px; font-size: 13px; background: #0f3460; }
#info-panel h3 { color: #e94560; margin-bottom: 8px; font-size: 14px; word-break: break-all; }
#info-panel .field { margin-bottom: 4px; }
#info-panel .label { color: #888; }
#info-panel .value { color: #e0e0e0; }
#info-panel a { color: #53a8b6; text-decoration: none; }
#info-panel a:hover { text-decoration: underline; }

/* Graph area */
#main { flex: 1; display: flex; flex-direction: column; position: relative; }
#toolbar { display: flex; align-items: center; gap: 8px; padding: 8px 14px; background: #16213e; border-bottom: 1px solid #0f3460; flex-wrap: wrap; }
#toolbar button { padding: 5px 12px; border: 1px solid #0f3460; border-radius: 4px; background: #1a1a2e; color: #e0e0e0; cursor: pointer; font-size: 12px; transition: background 0.15s; }
#toolbar button:hover { background: #e94560; border-color: #e94560; }
#toolbar button.active { background: #e94560; border-color: #e94560; }
#toolbar .sep { width: 1px; height: 20px; background: #0f3460; }
#toolbar label { font-size: 12px; color: #888; }
#toolbar select { padding: 4px 8px; border: 1px solid #0f3460; border-radius: 4px; background: #1a1a2e; color: #e0e0e0; font-size: 12px; }
#stats-bar { font-size: 11px; color: #666; margin-left: auto; }
#cy { flex: 1; background: #1a1a2e; }

/* Tooltip */
#tooltip { position: absolute; display: none; background: #16213e; border: 1px solid #e94560; border-radius: 6px; padding: 8px 12px; font-size: 12px; max-width: 400px; pointer-events: none; z-index: 100; box-shadow: 0 4px 12px rgba(0,0,0,0.5); }
#tooltip .tt-name { color: #e94560; font-weight: 600; word-break: break-all; }
#tooltip .tt-loc { color: #888; font-size: 11px; }
#tooltip .tt-hint { color: #555; font-size: 11px; margin-top: 4px; }

/* Legend */
#legend { position: absolute; bottom: 12px; right: 12px; background: rgba(22,33,62,0.92); border: 1px solid #0f3460; border-radius: 6px; padding: 10px 14px; font-size: 11px; z-index: 50; }
#legend div { margin-bottom: 4px; display: flex; align-items: center; gap: 6px; }
#legend .swatch { width: 24px; height: 3px; border-radius: 2px; }
#legend .node-swatch { width: 10px; height: 10px; border-radius: 50%; }
</style>
</head>
<body>

<div id="sidebar">
  <h1>SSAF Call Graph Explorer</h1>
  <div id="search-box">
    <input id="search-input" type="text" placeholder="Search functions... (e.g. main, clang::Sema)" autofocus>
  </div>
  <div id="search-results"></div>
  <div id="info-panel">
    <p style="color:#555">Click a node to see details.</p>
  </div>
</div>

<div id="main">
  <div id="toolbar">
    <button id="btn-layout-dagre" class="active" onclick="relayout('dagre')">Hierarchical</button>
    <button id="btn-layout-cose" onclick="relayout('cose')">Force</button>
    <button id="btn-layout-concentric" onclick="relayout('concentric')">Concentric</button>
    <div class="sep"></div>
    <button onclick="cy.fit(undefined, 40)">Fit</button>
    <button onclick="clearGraph()">Clear</button>
    <button onclick="removeSelectedNodes()">Delete selected</button>
    <div class="sep"></div>
    <label>Show:</label>
    <button id="btn-callees" class="active" onclick="toggleDir('callees')" title="Toggle whether expanding a node loads its callees (functions it calls)">Callees</button>
    <button id="btn-callers" class="active" onclick="toggleDir('callers')" title="Toggle whether expanding a node loads its callers (functions that call it)">Callers</button>
    <span id="stats-bar"></span>
  </div>
  <div id="cy"></div>
  <div id="tooltip"></div>
  <div id="legend">
    <div><span class="swatch" style="background:#e94560"></span> Direct call</div>
    <div><span class="swatch" style="background:#e94560; border-top: 2px dashed #e94560; background: none;"></span> Virtual call</div>
    <div><span class="node-swatch" style="background:#e94560"></span> Expanded node</div>
    <div><span class="node-swatch" style="background:#53a8b6"></span> Callee</div>
    <div><span class="node-swatch" style="background:#a8e6cf"></span> Caller</div>
    <div><span class="node-swatch" style="background:#555; border: 1px dashed #888;"></span> External (placeholder)</div>
  </div>
</div>

<script>
// ---- State ----
let currentLayout = 'dagre';
let showCallees = true;
let showCallers = true;
let expandedNodes = new Set();  // ids we've expanded

// ---- Cytoscape init ----
const cy = cytoscape({
  container: document.getElementById('cy'),
  style: [
    { selector: 'node',
      style: {
        'label': 'data(short_label)',
        'font-size': '11px',
        'color': '#e0e0e0',
        'text-valign': 'bottom',
        'text-margin-y': 4,
        'text-outline-width': 2,
        'text-outline-color': '#1a1a2e',
        'background-color': '#53a8b6',
        'width': 24, 'height': 24,
        'border-width': 2,
        'border-color': '#0f3460',
        'text-max-width': '120px',
        'text-wrap': 'ellipsis',
      }
    },
    { selector: 'node[?is_placeholder]',
      style: {
        'background-color': '#555',
        'border-style': 'dashed',
        'border-color': '#888',
      }
    },
    { selector: 'node.expanded',
      style: {
        'background-color': '#e94560',
        'border-color': '#ff6b81',
        'width': 30, 'height': 30,
      }
    },
    { selector: 'node.caller-node',
      style: {
        'background-color': '#a8e6cf',
        'border-color': '#6bcf9f',
      }
    },
    { selector: 'node.root',
      style: {
        'background-color': '#ffd700',
        'border-color': '#ffa500',
        'width': 36, 'height': 36,
        'font-size': '13px',
        'font-weight': 'bold',
      }
    },
    { selector: 'edge',
      style: {
        'width': 1.5,
        'line-color': '#e94560',
        'target-arrow-color': '#e94560',
        'target-arrow-shape': 'triangle',
        'curve-style': 'bezier',
        'arrow-scale': 0.8,
        'opacity': 0.6,
      }
    },
    { selector: 'edge[edge_type="virtual"]',
      style: {
        'line-style': 'dashed',
        'line-dash-pattern': [6, 3],
        'line-color': '#ffa500',
        'target-arrow-color': '#ffa500',
      }
    },
    { selector: 'node:selected',
      style: {
        'border-color': '#ffd700',
        'border-width': 3,
        'overlay-color': '#ffd700',
        'overlay-opacity': 0.15,
      }
    },
    { selector: '.faded',
      style: { 'opacity': 0.15 }
    },
  ],
  layout: { name: 'preset' },
  wheelSensitivity: 0.3,
});

// ---- Events ----
cy.on('tap', 'node', async function(evt) {
  const id = evt.target.id();
  const resp = await fetch('/api/node/' + id);
  if (!resp.ok) return;
  const node = await resp.json();
  showInfoPanel(node);
});

cy.on('dbltap', 'node', async function(evt) {
  const id = evt.target.id();
  await expandNode(parseInt(id));
});

// Tooltip on hover
const tooltip = document.getElementById('tooltip');
cy.on('mouseover', 'node', function(evt) {
  const d = evt.target.data();
  const loc = d.file ? shortPath(d.file) + ':' + d.line : '';
  tooltip.innerHTML = `<div class="tt-name">${escHtml(d.label)}</div>`
    + (loc ? `<div class="tt-loc">${escHtml(loc)}</div>` : '')
    + `<div class="tt-hint">Double-click to expand</div>`;
  tooltip.style.display = 'block';
  updateTooltipPos(evt.renderedPosition);
});
cy.on('mousemove', 'node', function(evt) {
  updateTooltipPos(evt.renderedPosition);
});
cy.on('mouseout', 'node', function() {
  tooltip.style.display = 'none';
});

function updateTooltipPos(pos) {
  const rect = document.getElementById('cy').getBoundingClientRect();
  tooltip.style.left = (pos.x + rect.left + 15) + 'px';
  tooltip.style.top = (pos.y + rect.top - 10) + 'px';
}

// ---- Search ----
let searchTimeout = null;
const searchInput = document.getElementById('search-input');
const searchResults = document.getElementById('search-results');

searchInput.addEventListener('input', () => {
  clearTimeout(searchTimeout);
  searchTimeout = setTimeout(doSearch, 200);
});
searchInput.addEventListener('keydown', (e) => {
  if (e.key === 'Enter') {
    clearTimeout(searchTimeout);
    doSearch();
  }
});

async function doSearch() {
  const q = searchInput.value.trim();
  if (!q) { searchResults.innerHTML = ''; return; }
  const resp = await fetch('/api/search?q=' + encodeURIComponent(q));
  if (!resp.ok) return;
  const results = await resp.json();
  searchResults.innerHTML = results.map(r => {
    const loc = r.file ? shortPath(r.file) + ':' + r.line : '';
    return `<div class="search-result" onclick="selectResult(${r.id})">
      <div class="name">${escHtml(r.pretty_name)}</div>
      <div class="loc">${escHtml(loc)}</div>
    </div>`;
  }).join('');
}

async function selectResult(id) {
  // If node already in graph, just center on it
  const existing = cy.getElementById(String(id));
  if (existing.length) {
    cy.animate({ center: { eles: existing }, zoom: cy.zoom() }, { duration: 300 });
    existing.select();
    // Fetch details
    const resp = await fetch('/api/node/' + id);
    if (resp.ok) showInfoPanel(await resp.json());
    return;
  }
  // Otherwise expand it
  await expandNode(id, true);
}

// ---- Graph operations ----
async function expandNode(id, isRoot = false) {
  if (expandedNodes.has(id)) return;  // already expanded

  // Check node size and warn if large
  const infoResp = await fetch('/api/node/' + id);
  if (!infoResp.ok) return;
  const nodeInfo = await infoResp.json();
  const totalNeighbors = nodeInfo.direct_callees + nodeInfo.virtual_callees + nodeInfo.callers;
  if (totalNeighbors > 50) {
    const proceed = confirm(
      `"${nodeInfo.pretty_name}" has ${nodeInfo.direct_callees + nodeInfo.virtual_callees} callees and ${nodeInfo.callers} callers (${totalNeighbors} total neighbors).\n\nExpanding may clutter the graph. Continue?`
    );
    if (!proceed) return;
  }

  expandedNodes.add(id);

  const resp = await fetch('/api/expand/' + id);
  if (!resp.ok) return;
  const data = await resp.json();

  // Collect edges first, filtering by direction toggles
  const newEdges = [];
  const connectedNodeIds = new Set();
  connectedNodeIds.add(String(id));  // always include the expanded node itself
  for (const e of data.edges) {
    const eid = e.data.source + '->' + e.data.target + ':' + e.data.edge_type;
    e.data.id = eid;
    const srcId = parseInt(e.data.source);
    const tgtId = parseInt(e.data.target);
    if (tgtId === id && !showCallers) continue;
    if (srcId === id && !showCallees) continue;
    if (!cy.getElementById(eid).length) {
      newEdges.push(e);
    }
    connectedNodeIds.add(e.data.source);
    connectedNodeIds.add(e.data.target);
  }

  // Only add nodes that have at least one edge or are already in the graph
  const newNodes = [];
  for (const n of data.nodes) {
    if (!cy.getElementById(n.data.id).length && connectedNodeIds.has(n.data.id)) {
      newNodes.push(n);
    }
  }

  cy.add(newNodes);
  cy.add(newEdges);

  // Mark node classes
  const node = cy.getElementById(String(id));
  node.addClass('expanded');
  if (isRoot) node.addClass('root');

  // Mark caller nodes
  for (const e of data.edges) {
    if (parseInt(e.data.target) === id) {
      cy.getElementById(e.data.source).addClass('caller-node');
    }
  }

  runLayout();
  updateStats();

  // Show info panel (reuse already-fetched nodeInfo)
  showInfoPanel(nodeInfo);
}

function clearGraph() {
  cy.elements().remove();
  expandedNodes.clear();
  updateStats();
  document.getElementById('info-panel').innerHTML = '<p style="color:#555">Click a node to see details.</p>';
}

// ---- Layout ----
function relayout(name) {
  currentLayout = name;
  document.querySelectorAll('#toolbar button[id^="btn-layout"]').forEach(b => b.classList.remove('active'));
  document.getElementById('btn-layout-' + name).classList.add('active');
  runLayout();
}

function runLayout() {
  if (cy.nodes().length === 0) return;
  let opts;
  if (currentLayout === 'dagre') {
    opts = { name: 'dagre', rankDir: 'TB', nodeSep: 40, rankSep: 60, animate: true, animationDuration: 300 };
  } else if (currentLayout === 'cose') {
    opts = { name: 'cose', animate: true, animationDuration: 300, nodeRepulsion: 8000, idealEdgeLength: 80 };
  } else {
    opts = { name: 'concentric', animate: true, animationDuration: 300,
             concentric: n => n.hasClass('expanded') ? 10 : (n.hasClass('root') ? 20 : 1),
             levelWidth: () => 2 };
  }
  cy.layout(opts).run();
}

// ---- Direction toggles ----
function toggleDir(dir) {
  if (dir === 'callees') {
    showCallees = !showCallees;
    document.getElementById('btn-callees').classList.toggle('active');
  } else {
    showCallers = !showCallers;
    document.getElementById('btn-callers').classList.toggle('active');
  }
}

// ---- Info panel ----
function showInfoPanel(node) {
  const loc = node.file ? shortPath(node.file) + ':' + node.line : 'unknown';
  document.getElementById('info-panel').innerHTML = `
    <h3>${escHtml(node.pretty_name)}</h3>
    <div class="field"><span class="label">Location: </span><span class="value">${escHtml(loc)}</span></div>
    <div class="field"><span class="label">Entity ID: </span><span class="value">${node.id}</span></div>
    <div class="field"><span class="label">Linkage: </span><span class="value">${escHtml(node.linkage || 'N/A')}</span></div>
    <div class="field"><span class="label">USR: </span><span class="value" style="font-size:11px;word-break:break-all;">${escHtml(node.usr || 'N/A')}</span></div>
    <div class="field"><span class="label">Direct callees: </span><span class="value">${node.direct_callees}</span></div>
    <div class="field"><span class="label">Virtual callees: </span><span class="value">${node.virtual_callees}</span></div>
    <div class="field"><span class="label">Callers: </span><span class="value">${node.callers}</span></div>
    <button onclick="removeNode(${node.id})" style="margin-top:8px;padding:4px 12px;background:#e94560;border:none;border-radius:4px;color:#fff;cursor:pointer;font-size:12px;">Remove node</button>
    <button onclick="removeSelectedNodes()" style="margin-top:8px;margin-left:4px;padding:4px 12px;background:#1a1a2e;border:1px solid #e94560;border-radius:4px;color:#e94560;cursor:pointer;font-size:12px;">Remove selected</button>
  `;
}

// ---- Node removal ----
function removeNode(id) {
  const ele = cy.getElementById(String(id));
  if (ele.length) {
    ele.remove();
    expandedNodes.delete(id);
    updateStats();
    document.getElementById('info-panel').innerHTML = '<p style="color:#555">Click a node to see details.</p>';
  }
}

function removeSelectedNodes() {
  const selected = cy.$(':selected');
  if (selected.length === 0) return;
  selected.forEach(ele => {
    if (ele.isNode()) expandedNodes.delete(parseInt(ele.id()));
  });
  selected.remove();
  updateStats();
  document.getElementById('info-panel').innerHTML = '<p style="color:#555">Click a node to see details.</p>';
}

// Keyboard: Delete/Backspace removes selected nodes
document.addEventListener('keydown', (e) => {
  if (e.target.tagName === 'INPUT' || e.target.tagName === 'TEXTAREA') return;
  if (e.key === 'Delete' || e.key === 'Backspace') {
    e.preventDefault();
    removeSelectedNodes();
  }
});

// ---- Stats ----
async function loadStats() {
  const resp = await fetch('/api/stats');
  if (!resp.ok) return;
  const s = await resp.json();
  document.getElementById('stats-bar').textContent =
    `Graph: ${s.total_nodes.toLocaleString()} nodes, ${s.total_edges.toLocaleString()} edges | Visible: 0 nodes`;
}
function updateStats() {
  const bar = document.getElementById('stats-bar');
  const existing = bar.textContent.split('|')[0] || '';
  bar.textContent = existing.trim() + ` | Visible: ${cy.nodes().length} nodes, ${cy.edges().length} edges`;
}

// ---- Utils ----
function escHtml(s) {
  return s.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;');
}
function shortPath(p) {
  // Show last 3 components
  const parts = p.split('/');
  return parts.length > 3 ? '.../' + parts.slice(-3).join('/') : p;
}

// ---- Init ----
loadStats();
</script>
</body>
</html>
"""


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Interactive SSAF Call Graph Visualizer")
    parser.add_argument("json_file", help="Path to SSAF call graph JSON")
    parser.add_argument("--port", type=int, default=8080, help="Server port (default: 8080)")
    parser.add_argument("--no-browser", action="store_true", help="Don't auto-open browser")
    args = parser.parse_args()

    if not os.path.isfile(args.json_file):
        print(f"Error: {args.json_file} not found", file=sys.stderr)
        sys.exit(1)

    graph = CallGraph(args.json_file)
    Handler.graph = graph

    class ReusableHTTPServer(HTTPServer):
        allow_reuse_address = True

    server = ReusableHTTPServer(("127.0.0.1", args.port), Handler)
    url = f"http://127.0.0.1:{args.port}"
    print(f"Serving at {url}")

    if not args.no_browser:
        threading.Timer(0.5, lambda: webbrowser.open(url)).start()

    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutting down.")
        server.shutdown()


if __name__ == "__main__":
    main()
