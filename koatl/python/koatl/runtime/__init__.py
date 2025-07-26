"""
This module initializes the environment for koatl.
It sets up the meta-finder hook to enable importing .tl files,
and also declares functions that are required for certain tl features to work,
such as coalescing and module exports.

koatl.runtime should be written in Python only since otherwise
it would create a circular dependency.
"""

from functools import wraps
from types import SimpleNamespace

from . import meta_finder
from .._rs import Record, vget, ok

meta_finder.install_hook()


class MatchError(Exception):
    def __init__(self, message):
        super().__init__(message)


def set_exports(package_name, globals_dict, exports, module_star_exports):
    import importlib

    exports = set(exports)

    for module in module_star_exports:
        mod = importlib.import_module(module, package_name)

        if hasattr(mod, "__all__"):
            for name in mod.__all__:
                exports.add(name)
        else:
            for name in dir(mod):
                if name.startswith("_"):
                    continue

                exports.add(name)

    if "__all__" not in globals_dict:
        globals_dict["__all__"] = ()

    globals_dict["__all__"] = tuple(set(globals_dict["__all__"]) | exports)


def unpack_record(obj):
    """
    used in record unpacking
    """
    if hasattr(obj, "items"):
        return Record(obj.items())
    else:
        return Record(obj.__dict__)


def do(f):
    @wraps(f)
    def impl(*args, **kwargs):
        coro = f(*args, **kwargs)

        try:
            m = coro.send(None)
        except StopIteration as e:
            return e.value

        def recurse(v):
            nonlocal m
            try:
                m = coro.send(v)
                return vget(m, "bind_once")(recurse)
            except StopIteration as e:
                try:
                    return vget(m, "pure")(e.value)
                except AttributeError:
                    return e.value

        return vget(m, "bind_once")(recurse)

    return impl


from .traits import *


__tl__ = SimpleNamespace(
    Record=Record,
    MatchError=MatchError,
    unpack_record=unpack_record,
    set_exports=set_exports,
    vget=vget,
    ok=ok,
    do=do,
    **{name: traits.__dict__[name] for name in traits.__all__}
)


__all__ = [
    "__tl__",
    "Record",
    "MatchError",
    *traits.__all__,
]
