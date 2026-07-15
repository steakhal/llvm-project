from __future__ import annotations

import dataclasses
import json
from dataclasses import dataclass, field
from typing import List


@dataclass
class AnalyzerProvenance:
    commit: str
    commit_title: str = ""
    config_digest: str = ""
    volume: str = ""


@dataclass
class ProjectProvenance:
    name: str
    source: str = ""
    commit: str = ""
    commit_title: str = ""


@dataclass
class ContainerProvenance:
    runtime: str = "docker"
    image_digest: str = ""


@dataclass
class ExecutionProvenance:
    n: int = 1
    analyzer_args: List[str] = field(default_factory=list)
    note: str = ""


@dataclass
class Metadata:
    run_id: str
    kind: str  # "functional" | "benchmark"
    created: str  # ISO-8601
    analyzer: AnalyzerProvenance
    container: ContainerProvenance
    execution: ExecutionProvenance
    corpus: List[ProjectProvenance] = field(default_factory=list)

    def to_json(self) -> str:
        return json.dumps(dataclasses.asdict(self), indent=2, sort_keys=True)

    @classmethod
    def from_json(cls, text: str) -> "Metadata":
        raw = json.loads(text)
        return cls(
            run_id=raw["run_id"],
            kind=raw["kind"],
            created=raw["created"],
            analyzer=AnalyzerProvenance(**raw["analyzer"]),
            container=ContainerProvenance(**raw["container"]),
            execution=ExecutionProvenance(**raw["execution"]),
            corpus=[ProjectProvenance(**p) for p in raw.get("corpus", [])],
        )
