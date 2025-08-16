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


def dummy(name):
    def wrapper(*args, **kwargs):
        raise RuntimeError(
            f"{name} is not available without the prelude. Please import koatl.prelude."
        )

    return wrapper


__tl__ = SimpleNamespace(
    Exception=Exception,
    slice=slice,
    type=type,
    vget=virtual.vget,
    vhas=virtual.vhas,
    set_exports=helpers.set_exports,
    do=helpers.do,
    partial=functools.partial,
    # These require more complex logic and require the prelude.
    # The runtime provides dummy implementations that raise if used without the prelude.
    memo_value=dummy("memo"),
    async_memo_value=dummy("memo"),
    op_map=dummy("?"),
    op_coal=("??"),
    Ok=dummy("try-expr"),
    Err=dummy("try-expr"),
    Result=dummy("Result"),
    **{name: helpers.__dict__[name] for name in helpers.__all__},
    **{name: record.__dict__[name] for name in record.__all__},
    **{name: virtual.__dict__[name] for name in virtual.__all__},
)


__all__ = [
    "__tl__",
    *helpers.__all__,
    *record.__all__,
    *virtual.__all__,
]
