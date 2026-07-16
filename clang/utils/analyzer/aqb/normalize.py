from __future__ import annotations

from dataclasses import dataclass
from typing import List, Set

from aqb._satest import CmpRuns


@dataclass(frozen=True)
class Finding:
    """A normalized, deduplicated analyzer report."""

    issue_id: str
    file: str
    line: int
    column: int
    checker: str
    category: str
    description: str
    path_length: int


def _to_finding(diag) -> Finding:
    raw = diag.get_raw_data()
    return Finding(
        issue_id=diag.get_issue_identifier(),
        file=diag.get_file_name(),
        line=diag.get_line(),
        column=diag.get_column(),
        checker=raw.get("check_name", ""),
        category=diag.get_category(),
        description=diag.get_description(),
        path_length=diag.get_path_length(),
    )


def load_findings(results_dir: str, project_root: str = "") -> List[Finding]:
    """Load, normalize, and deduplicate analyzer findings from a results dir.

    Reuses SATest's ``CmpRuns`` loader, which walks ``results_dir`` for
    ``*.plist`` files and canonicalizes each report. Paths are made relative to
    ``project_root``. Findings are deduplicated by their stable issue identifier
    (first wins), so a header analyzed through many TUs yields one finding per
    distinct issue. ``delete_empty=False`` ensures the input plists are never
    mutated.
    """
    run = CmpRuns.load_results(
        CmpRuns.ResultsDirectory(path=results_dir, root=project_root),
        delete_empty=False,
    )
    findings: List[Finding] = []
    seen: Set[str] = set()
    for diag in run.diagnostics:
        issue_id = diag.get_issue_identifier()
        if issue_id in seen:
            continue
        seen.add(issue_id)
        findings.append(_to_finding(diag))
    return findings
