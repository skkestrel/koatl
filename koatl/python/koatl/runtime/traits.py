from collections import defaultdict
from functools import partial
from itertools import count
from types import new_class


types_vtbl = defaultdict(dict)
traits_vtbl = defaultdict(dict)


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

            return iter(obj)

        if name in types_vtbl:
            for o in type(obj).mro():
                if o in types_vtbl[name]:
                    return partial(types_vtbl[name][o], obj)

        if name in traits_vtbl:
            for key, value in traits_vtbl[name].items():
                if isinstance(obj, key):
                    return partial(value, obj)

        raise AttributeError(
            f"'{type(obj).__name__}' object has no v-attribute '{name}'"
        ) from None


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
        for req in requires:
            try:
                vget(instance, req)
            except AttributeError:
                return False

        return True

    meta = type(name, (type,), {"__instancecheck__": instancecheck})

    def populate(ns):
        for method_name, method in methods.items():
            ns[method_name] = method
        ns["__module__"] = module

    typ = new_class(name, (), {"metaclass": meta}, populate)
    typ._methods = methods

    return typ


def register_global_attr(type, name, value):
    if name not in types_vtbl:
        types_vtbl[name] = {}
    types_vtbl[name][type] = value


def register_global_trait(type):
    for name, method in type._methods.items():
        if name not in traits_vtbl:
            traits_vtbl[name] = {}
        traits_vtbl[name][type] = method


__all__ = [
    "Trait",
    "ExtensionProperty",
    "register_global_attr",
    "register_global_trait",
]
