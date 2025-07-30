"""
This module initializes the environment for koatl.
It sets up the meta-finder hook to enable importing .tl files,
and also declares functions that are required for certain tl features to work,
such as coalescing and module exports.

koatl.runtime should be written in Python only since otherwise
it would create a circular dependency.
"""

import functools
from types import SimpleNamespace

from . import meta_finder

meta_finder.install_hook()


from .virtual import *
from .record import *
from .helpers import *


__tl__ = SimpleNamespace(
    Exception=Exception,
    slice=slice,
    vget=virtual.vget,
    vhas=virtual.vhas,
    unpack_record=helpers.unpack_record,
    set_exports=helpers.set_exports,
    do=helpers.do,
    ok=helpers.ok,
    partial=functools.partial,
    **{name: helpers.__dict__[name] for name in helpers.__all__},
    **{name: record.__dict__[name] for name in record.__all__},
    **{name: virtual.__dict__[name] for name in virtual.__all__}
)


__all__ = [
    "__tl__",
    *helpers.__all__,
    *record.__all__,
    *virtual.__all__,
]
