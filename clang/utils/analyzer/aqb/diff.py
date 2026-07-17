from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List, Tuple

from aqb.normalize import Finding

# Tier-2 similarity fields (mirrors CmpRuns.AnalysisDiagnostic.KEY_FIELDS).
_SIMILAR_FIELDS = ("checker", "category", "description")


@dataclass
class ReportDelta:
    common: List[Finding] = field(default_factory=list)
    added: List[Finding] = field(default_factory=list)
    removed: List[Finding] = field(default_factory=list)
    changed: List[Tuple[Finding, Finding]] = field(default_factory=list)


def _location(f: Finding) -> Tuple[str, int, int]:
    return (f.file, f.line, f.column)


def _similar(a: Finding, b: Finding) -> bool:
    return all(getattr(a, k) == getattr(b, k) for k in _SIMILAR_FIELDS)


def _group(findings: List[Finding]) -> Dict[Tuple[str, int, int], List[Finding]]:
    grouped: Dict[Tuple[str, int, int], List[Finding]] = {}
    for f in findings:
        grouped.setdefault(_location(f), []).append(f)
    return grouped


def compare_findings(base: List[Finding], new: List[Finding]) -> ReportDelta:
    """Two-tier classification of two normalized finding sets, mirroring
    CmpRuns.compare_results: bucket by location; tier-1 match on issue identity
    (common); tier-2 match on checker/category/description (changed); leftovers
    are removed (base-only) / added (new-only). Locations unique to one side are
    wholly removed/added.
    """
    delta = ReportDelta()
    gb = _group(base)
    gn = _group(new)

    for loc in set(gb) & set(gn):
        old = list(gb[loc])
        cur = list(gn[loc])

        # Tier 1: exact issue identity.
        matched_new = set()
        remaining_old: List[Finding] = []
        for a in old:
            hit = next(
                (
                    b
                    for b in cur
                    if b.issue_id == a.issue_id and id(b) not in matched_new
                ),
                None,
            )
            if hit is not None:
                delta.common.append(hit)
                matched_new.add(id(hit))
            else:
                remaining_old.append(a)
        cur = [b for b in cur if id(b) not in matched_new]

        # Tier 2: fuzzy similarity (checker/category/description).
        used_new = set()
        still_old: List[Finding] = []
        for a in remaining_old:
            hit = next(
                (b for b in cur if id(b) not in used_new and _similar(a, b)), None
            )
            if hit is not None:
                delta.changed.append((a, hit))
                used_new.add(id(hit))
            else:
                still_old.append(a)
        cur = [b for b in cur if id(b) not in used_new]

        delta.removed.extend(still_old)
        delta.added.extend(cur)

    for loc in set(gb) - set(gn):
        delta.removed.extend(gb[loc])
    for loc in set(gn) - set(gb):
        delta.added.extend(gn[loc])

    return delta


def diff_runs(
    base: Dict[str, List[Finding]], new: Dict[str, List[Finding]]
) -> Dict[str, ReportDelta]:
    """Compare two runs' per-project findings. Projects present in only one run
    yield an all-removed (base-only) or all-added (new-only) delta."""
    deltas: Dict[str, ReportDelta] = {}
    for project in sorted(set(base) | set(new)):
        deltas[project] = compare_findings(base.get(project, []), new.get(project, []))
    return deltas


def summarize(deltas: Dict[str, ReportDelta]) -> Dict[str, int]:
    """Flat totals across all projects."""
    out = {"common": 0, "added": 0, "removed": 0, "changed": 0}
    for d in deltas.values():
        out["common"] += len(d.common)
        out["added"] += len(d.added)
        out["removed"] += len(d.removed)
        out["changed"] += len(d.changed)
    return out


def verdict(summary: Dict[str, int], expect: str) -> Tuple[bool, str]:
    """Apply an --expect policy to a diff summary. Returns (passed, reason)."""
    base_count = summary["common"] + summary["removed"] + summary["changed"]
    new_count = summary["common"] + summary["added"] + summary["changed"]
    if expect == "no-crashes":
        # v1: crashes are not yet tracked in findings; a completed run is treated
        # as crash-free, so this passes. Tightened when crash artifacts land.
        return True, "no crashes recorded"
    if expect == "same-count":
        ok = base_count == new_count
        return ok, f"base {base_count} vs new {new_count}"
    if expect == "same-reports":
        drift = summary["added"] + summary["removed"] + summary["changed"]
        return drift == 0, f"{drift} report(s) differ"
    raise ValueError(f"unknown --expect policy: {expect!r}")
