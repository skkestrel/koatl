"""
This module initializes the environment for koatl.
It sets up the meta-finder hook to enable importing .tl files,
and also declares functions that are required for certain tl features to work,
such as coalescing and module exports.
"""

from . import meta_finder

meta_finder.install_hook()
del meta_finder


class MatchError(Exception):
    def __init__(self, message):
        super().__init__(message)


class OkMeta(type):
    def __instancecheck__(cls, instance):
        return Ok(instance)


class Ok(metaclass=OkMeta):
    def __new__(cls, value):
        match value:
            case None:
                return False
            case BaseException():
                return False
        return True


def _set_exports(package_name, globals_dict, exports, module_star_exports):
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

    globals_dict["__all__"] = tuple(exports)


from .._rs import Record, tl_vget

from .traits import *


__all__ = [
    "Record",
    "Ok",
    "MatchError",
    "ExtensionProperty",
    "tl_vget",
    "_set_exports",
    # traits.tl
    "Trait",
    "register_global_attr",
    "register_global_trait",
]
