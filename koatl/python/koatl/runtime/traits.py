import types
from .._rs import types_vtbl, traits_vtbl, tl_vget, tl_vcheck


def Trait(name, methods, *, requires=[]):
    import inspect

    for method_name, method in methods.items():
        method.__name__ = method_name
        method.__qualname__ = f"{name}.{method_name}"

        sig = inspect.signature(method)
        params = list(sig.parameters.values())
        params[0].replace(name="self")
        sig = sig.replace(parameters=params)
        method.__signature__ = sig

    def instancecheck(cls, instance):
        for req in requires:
            if not tl_vcheck(instance, req):
                return False

        return True

    meta = type(name, (type,), {"__instancecheck__": instancecheck})

    def populate(ns):
        for method_name, method in methods.items():
            ns[method_name] = method

    typ = types.new_class(name, (), {"metaclass": meta}, populate)
    typ.__instancecheck__ = instancecheck
    typ._methods = methods

    return typ


def ExtensionProperty(f):
    f.ext_prop = True
    return f


def register_global_attr(type, name, value):
    if name not in types_vtbl:
        types_vtbl[name] = {}
    types_vtbl[name][type] = value


def register_global_trait(type):
    for name, method in type._methods.items():
        if name not in traits_vtbl:
            traits_vtbl[name] = {}
        traits_vtbl[name][type] = method


@ExtensionProperty
def _slice_iter(sl):
    i = 0 if sl.start is None else sl.start
    step = 1 if sl.step is None else sl.step

    if sl.stop is None:
        while True:
            yield i
            i += step
    else:
        yield from range(i, sl.stop, step)


register_global_attr(slice, "iter", _slice_iter)

__all__ = [
    "Trait",
    "ExtensionProperty",
    "register_global_attr",
    "register_global_trait",
]
