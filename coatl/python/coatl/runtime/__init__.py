"""
This module initializes the environment for coatl.
It sets up the meta-finder hook to enable importing .tl files,
and also declares functions that are required for certain tl features to work,
such as coalescing and module exports.
"""

from collections import defaultdict
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


class MatchError(Exception):
    def __init__(self, message):
        super().__init__(message)


def __slice_iter(sl):
    i = 0 if sl.start is None else sl.start
    step = 1 if sl.step is None else sl.step

    if sl.stop is None:
        while True:
            yield i
            i += step
    else:
        yield from range(i, sl.stop, step)


__vtable = defaultdict(
    dict,
    {
        dict: {"iter": dict.items},
        slice: {"iter": __slice_iter},
    },
)


class Trait:
    def __init__(self, name, requires_methods=[]):
        self.name = name
        self.requires_methods = requires_methods
        self.methods = {}

    def add_curried(self, methods):
        import functools

        def closure(name, method):
            def bind_self(obj):
                # TODO remove first parameter from signature

                @functools.wraps(method)
                def call(*args, **kwargs):
                    return method(obj, *args, **kwargs)

                call.__name__ = name
                call.__qualname__ = self.name + "." + name

                return call

            bind_self.__name__ = name
            bind_self.__qualname__ = self.name + "." + name

            self.methods[name] = bind_self

        for name, method in methods.items():
            closure(name, method)


__traits = defaultdict(dict, {})


def __vtable_lookup(obj, name, no_traits=False):
    for cls in type(obj).__mro__:
        if cls in __vtable and name in __vtable[cls]:
            return __vtable[cls][name](obj)

    # TODO generalize this?
    if name == "iter":
        if hasattr(obj, "__iter__"):
            return __builtins__["iter"](obj)
        raise TypeError(f"'{type(obj).__name__}' object is not iterable")

    if not no_traits:
        for trait_name, trait in __traits.items():
            found = True
            for requirement in trait.requires_methods:
                try:
                    __vtable_lookup(obj, requirement, no_traits=True)
                except (AttributeError, TypeError):
                    found = False
                    break

            if found:
                if name in trait.methods:
                    return trait.methods[name](obj)

    raise AttributeError(f"'{type(obj).__name__}' object has no attribute '{name}'")


__all__ = [
    "__coalesces",
    "__set_exports",
    "__match_proxy",
    "MatchError",
    "__vtable",
    "__traits",
    "__vtable_lookup",
    "Trait",
]
