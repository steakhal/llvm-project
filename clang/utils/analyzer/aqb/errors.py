from __future__ import annotations


class AqbError(Exception):
    """Base class for all AQB domain errors."""


class RunNotFoundError(AqbError):
    """A run id (or unique prefix) could not be resolved to a stored run."""


class RuntimeCommandError(AqbError):
    """A container-runtime command exited non-zero."""


class ClangBuildError(AqbError):
    """Building a Clang Volume failed; the partial volume has been removed."""


class CommitTitleError(AqbError):
    """The analyzer commit's title could not be resolved (local repo or GitHub).

    A commit always has a title, so AQB treats a resolution failure as fatal
    rather than recording empty provenance.
    """
