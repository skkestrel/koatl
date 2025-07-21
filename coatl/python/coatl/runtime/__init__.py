"""
This module initializes the environment for coatl.
It sets up the meta-finder hook to enable importing .tl files,
and also declares functions that are required for certain tl features to work,
such as coalescing and module exports.
"""

from . import meta_finder
meta_finder.install_hook()
del meta_finder

def __set_exports(package_name, globals_dict, exports, module_star_exports):
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

def __coalesces(x):
    return x is None or isinstance(x, BaseException)

def __match_proxy(v):
    from types import SimpleNamespace
    return SimpleNamespace(value=v)

__all__ = ["__coalesces", "__set_exports", "__match_proxy"]