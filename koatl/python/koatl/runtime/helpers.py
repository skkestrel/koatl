from functools import wraps
from .traits import vget
from .record import Record


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


def unpack_record(obj):
    """
    used in record unpacking
    """
    if hasattr(obj, "items"):
        return Record(obj.items())
    else:
        return Record(obj.__dict__)


def ok(obj):
    if obj is None:
        return False
    if isinstance(obj, BaseException):
        return False
    return True


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
