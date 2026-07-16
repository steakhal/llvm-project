from __future__ import annotations

import unittest

from aqb.errors import AqbError, ClangBuildError, RunNotFoundError, RuntimeCommandError
from aqb.store import RunNotFoundError as StoreRunNotFoundError


class ErrorsTest(unittest.TestCase):
    def test_all_domain_errors_derive_from_aqb_error(self):
        for exc in (RunNotFoundError, RuntimeCommandError, ClangBuildError):
            self.assertTrue(issubclass(exc, AqbError))

    def test_store_reexports_the_same_run_not_found_error(self):
        # store must re-export the shared class, not define its own.
        self.assertIs(StoreRunNotFoundError, RunNotFoundError)
