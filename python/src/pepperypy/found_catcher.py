"""
The MIT License (MIT)

Copyright (c) [2015-2025] [Andrew Annex]

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
"""

# Derived from SpiceyPy's found_catcher.py.
# Modified for PepperyPy (2026, Jacob Williams): found_check and
# no_found_check apply to the calling thread (and asyncio task) only, through
# a ContextVar, so that threads querying at once do not change each other's
# behavior; found_check_on and found_check_off still change the default for
# the whole process (config.catch_false_founds).

__all__ = [
    "found_check_off",
    "found_check_on",
    "found_check",
    "no_found_check",
    "get_found_catch_state",
    "spice_found_exception_thrower",
]
from contextlib import contextmanager
import functools
from typing import Callable, Iterator

from . import config
from ._core import _found_state, catch_false_founds
from .exceptions import NotFoundError


def spice_found_exception_thrower(f: Callable) -> Callable:
    """
    Decorator for wrapping functions that return a found flag last (as
    SpiceyPy's does): NotFoundError if it is false, otherwise the other
    results (when found checks are on).
    """

    @functools.wraps(f)
    def wrapper(*args, **kwargs):
        res = f(*args, **kwargs)
        if catch_false_founds():
            found = res[-1]
            if isinstance(found, bool) and not found:
                raise NotFoundError(
                    "Spice returns not found for function: {}".format(f.__name__),
                    found=found,
                )
            elif hasattr(found, "__iter__") and not all(found):
                raise NotFoundError(
                    "Spice returns not found in a series of calls for function: {}".format(
                        f.__name__
                    ),
                    found=found,
                )
            actualres = res[0:-1]
            return actualres[0] if len(actualres) == 1 else actualres
        return res

    return wrapper


@contextmanager
def found_check() -> Iterator[None]:
    """
    Within the block (in this thread or asyncio task), a false found flag
    raises NotFoundError and the flag is not returned::

        with spice.found_check():
            name = spice.bodc2n(-999991)  # raises NotFoundError
    """
    token = _found_state.set(True)
    try:
        yield
    finally:
        _found_state.reset(token)


@contextmanager
def no_found_check() -> Iterator[None]:
    """
    Within the block (in this thread or asyncio task), a false found flag
    does not raise; the flag is returned last::

        with spice.no_found_check():
            name, found = spice.bodc2n(-999991)  # found is False
    """
    token = _found_state.set(False)
    try:
        yield
    finally:
        _found_state.reset(token)


def found_check_off() -> None:
    """
    Turn off found catching (the default for all threads)
    """
    config.catch_false_founds = False


def found_check_on() -> None:
    """
    Turn on found catching (the default for all threads)
    """
    config.catch_false_founds = True


def get_found_catch_state() -> bool:
    """
    Returns the current found catch state (in this thread)
    """
    return catch_false_founds()
