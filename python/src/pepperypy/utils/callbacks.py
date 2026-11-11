"""SpiceyPy's callback decorators (``spiceypy.utils.callbacks``).

PepperyPy calls Python functions directly (no ctypes): the decorators return
the function unchanged, so that code written for SpiceyPy runs as is.

- ``SpiceUDFUNS``: ``f(et) -> float``, a scalar function of time.
- ``SpiceUDFUNB``: ``f(udfuns, et) -> bool``: a condition, or whether
  ``udfuns`` is decreasing at et (``uddc``).
"""


def _identity(f):
    return f


SpiceUDFUNS = SpiceUDFUNB = SpiceUDQDEC = SpiceUDFUNC = SpiceUDSTEP = SpiceUDREFN = _identity
SpiceUDREPI = SpiceUDREPU = SpiceUDREPF = SpiceUDBAIL = _identity


def CallUDFUNS(f, x):
    """Call a scalar function of time."""
    return f(x)
