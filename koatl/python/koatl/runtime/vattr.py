from functools import partial
from itertools import count
from koatl._rs import fast_vget


def vget(obj, name, ignore_traits=False):
    try:
        return getattr(obj, name)
    except AttributeError as e:
        orig_err = e

    # special case for iter - this could be implemented using types and trait vtbls
    # but this is simpler and probably faster
    if name == "iter":
        if isinstance(obj, slice):
            start = obj.start if obj.start is not None else 0
            step = obj.step if obj.step is not None else 1
            if obj.stop is None:
                return iter(count(start, step))
            else:
                return iter(range(start, obj.stop, step))

        try:
            return iter(obj.items())
        except AttributeError:
            pass

        try:
            return iter(obj)
        except TypeError:
            pass

    v = fast_vget(obj, name, ignore_traits)
    if v is not None:
        if hasattr(v, "_property"):
            return v(obj)

        return partial(v, obj)

    raise orig_err


def vhas(obj, name, ignore_traits=False):
    if hasattr(obj, name):
        return True

    if name == "iter":
        if isinstance(obj, slice):
            return True

        try:
            obj.items()
            return True
        except AttributeError:
            pass

        try:
            iter(obj)
            return True
        except TypeError:
            pass

    v = fast_vget(obj, name, ignore_traits)
    if v is not None:
        return True

    return False
