"""SpiceyPy's cell types (``spiceypy.utils.support_types``), without ctypes."""
from ..cells import (SpiceCell, SPICEDOUBLE_CELL, SPICEINT_CELL, SPICECHAR_CELL, SPICEBOOL_CELL,  # noqa: F401
                     SPICETIME_CELL, Cell_Double, Cell_Int, Cell_Char, Cell_Bool, Cell_Time, CTRLBLOCK)


def is_iterable(i):
    """Whether i is iterable (and not a string)."""
    return hasattr(i, '__iter__') and not isinstance(i, (str, bytes))

from ..exceptions import SpiceyError, NotFoundError  # noqa: F401,E402  (SpiceyPy has them here too)


class SpiceSPK18Subtype:
    """SPK type 18 subtypes."""
    S18TP0 = 0
    S18TP1 = 1


class SpiceEKDataType:
    """EK column data types."""
    SPICE_CHR = 0
    SPICE_DP = 1
    SPICE_INT = 2
    SPICE_TIME = 3
    SPICE_BOOL = 4


class SpiceEKExprClass:
    """EK query expression classes."""
    SPICE_EK_EXP_COL = 0
    SPICE_EK_EXP_FUNC = 1
    SPICE_EK_EXP_EXPR = 2
