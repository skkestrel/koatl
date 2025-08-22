import functools
import importlib
from types import SimpleNamespace

from koatl.runtime.record import Record
from . import meta_finder
from .vattr import vhas, vget

meta_finder.install_hook()

__all__ = ["__tl__"]


def set_exports(package_name, globals_dict, exports, module_star_exports):
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


__tl__ = SimpleNamespace(
    Exception=Exception,
    slice=slice,
    type=type,
    partial=functools.partial,
    #
    record_literal=Record.from_dict_ref,
    #
    set_exports=set_exports,
    #
    vget=vget,
    vhas=vhas,
)
