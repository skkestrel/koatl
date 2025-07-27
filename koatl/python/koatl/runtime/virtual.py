from functools import partial
from itertools import count
from types import new_class
from .._rs import fast_vget, fast_vset, fast_vset_trait


def ExtensionProperty(f):
    f.ext_prop = True
    return f


def vget(obj, name):
    try:
        return getattr(obj, name)
    except:
        # special case for iter - this could be implemented using types and trait vtbls
        # but this is simpler and probably faster
        if name == "iter":
            if isinstance(obj, slice):
                start = obj.start if obj.start is not None else 0
                step = obj.step if obj.step is not None else 1
                if obj.stop is None:
                    return count(start, step)
                else:
                    return range(start, obj.stop, step)

            try:
                return obj.items()
            except AttributeError:
                pass

            try:
                return iter(obj)
            except TypeError:
                pass

        v = fast_vget(obj, name)
        if v is not None:
            if hasattr(v, "ext_prop"):
                return v(obj)

            return partial(v, obj)

        raise AttributeError(
            f"'{type(obj).__name__}' object has no v-attribute '{name}'"
        ) from None


def vhas(obj, name):
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

    v = fast_vget(obj, name)
    if v is not None:
        return True

    return False


def Trait(module, name, methods, *, requires=[]):
    def fix_methods(type_name, methods):
        import inspect

        for method_name, method in methods.items():
            method.__name__ = method_name
            method.__qualname__ = f"{type_name}.{method_name}"

            if not isinstance(method, (staticmethod, classmethod)):
                sig = inspect.signature(method)
                params = list(sig.parameters.values())
                params[0].replace(name="self")
                sig = sig.replace(parameters=params)
                method.__signature__ = sig

    fix_methods(name, methods)

    def instancecheck(cls, instance):
        if cls != typ:
            # if not checking for the trait itself, use the default behavior
            return type.__instancecheck__(cls, instance)

        for req in requires:
            try:
                vget(instance, req)
            except AttributeError:
                return False

        return True

    meta = type(
        f"{name}Meta",
        (type,),
        {"__instancecheck__": instancecheck, "__module__": "types"},
    )

    def populate(ns):
        for method_name, method in methods.items():
            ns[method_name] = method
        ns["__module__"] = module
        ns["_methods"] = methods
        ns["_requires"] = requires

    typ = new_class(name, (), {"metaclass": meta}, populate)

    return typ


def register_global_attr(type, name, value):
    fast_vset(type, name, value)


def register_global_trait(type):
    for name, method in type._methods.items():
        fast_vset_trait(type.__name__, type._requires, name, method)


__all__ = [
    "Trait",
    "ExtensionProperty",
    "register_global_attr",
    "register_global_trait",
]
