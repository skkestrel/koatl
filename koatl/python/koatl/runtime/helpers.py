from functools import wraps
from .virtual import vget


__all__ = ["MatchError"]


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


def do(f):
    @wraps(f)
    def impl(*args, **kwargs):
        gen = f(*args, **kwargs)

        try:
            m = gen.send(None)
        except StopIteration:
            raise ValueError(
                "Returning before `@` is not allowed. "
                "Use `return @MonadType.pure(value)` instead."
            ) from None

        try:
            # TODO: this is a workaround to avoid recursion.
            # is it possible to derive bind_gen directly from bind_once?

            return vget(m, "bind_gen")(gen)
        except AttributeError:
            pass

        def recurse(v):
            nonlocal m
            try:
                m = gen.send(v)
                return vget(m, "bind_once")(recurse)
            except StopIteration as e:
                return vget(m, "pure")(e.value)

        try:
            return vget(m, "bind_once")(recurse)
        except AttributeError:
            raise ValueError("@ can only be used with an object that has `bind_once`.")

    return impl
