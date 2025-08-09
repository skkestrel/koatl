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


def ok(obj):
    try:
        return obj.ok
    except AttributeError:
        pass

    if obj is None:
        return False
    if isinstance(obj, BaseException):
        return False
    return True


def do(f):
    @wraps(f)
    def impl(*args, **kwargs):
        gen = f(*args, **kwargs)

        try:
            m = gen.send(None)
        except StopIteration as e:
            raise ValueError(
                "Returning before `@` is not allowed. "
                "Use `return @MonadType.pure(value)` instead."
            ) from None

        def recurse(v):
            nonlocal m
            try:
                m = gen.send(v)
                return vget(m, "bind_once")(recurse)
            except StopIteration as e:
                return vget(m, "pure")(e.value)

        try:
            # TODO: this is a workaround to avoid recursion.
            # is it possible to derive bind_gen directly from bind_once?

            bind_gen = vget(m, "bind_gen")
        except AttributeError:
            try:
                return vget(m, "bind_once")(recurse)
            except AttributeError:
                # Fallback to Result if m is not a monadic type
                from . import __tl__

                bind_gen = __tl__.Result(m).bind_gen

        return bind_gen(gen)

    return impl
