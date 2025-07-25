import types
from .._rs import types_vtbl, traits_vtbl, tl_vcheck


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


def Trait(module, name, methods, *, requires=[]):
    fix_methods(name, methods)

    def instancecheck(cls, instance):
        for req in requires:
            if not tl_vcheck(instance, req):
                return False

        return True

    meta = type(name, (type,), {"__instancecheck__": instancecheck})

    def populate(ns):
        for method_name, method in methods.items():
            ns[method_name] = method
        ns["__module__"] = module

    typ = types.new_class(name, (), {"metaclass": meta}, populate)
    typ._methods = methods

    return typ


def PseudoType(module, name, methods, instancecheck):
    fix_methods(name, methods)

    meta = type(
        name,
        (type,),
        {"__instancecheck__": lambda cls, instance: cls(instance)},
    )

    def populate(ns):
        for method_name, method in methods.items():
            ns[method_name] = method
        ns["__module__"] = module
        ns["__new__"] = lambda cls, *args, **kwargs: instancecheck(*args, **kwargs)

    typ = types.new_class(name, (), {"metaclass": meta}, populate)
    typ._methods = methods

    return typ


def check_ok(value):
    if value is None:
        return False
    elif isinstance(value, BaseException):
        return False
    return True


def assert_ok(value):
    if isinstance(value, BaseException):
        raise value
    elif value is None:
        raise ValueError("Expected a value, got None")
    return value


def check_not_ok(value):
    return not check_ok(value)


def check_some(value):
    return value is not None


def check_err(value):
    return isinstance(value, BaseException)


Ok = PseudoType(
    "koatl.runtime.traits", "Ok", {"assert": staticmethod(assert_ok)}, check_ok
)
NotOk = PseudoType("koatl.runtime.traits", "NotOk", {}, check_not_ok)
Some = PseudoType("koatl.runtime.traits", "Some", {}, check_some)
Err = PseudoType("koatl.runtime.traits", "Err", {}, check_err)


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


__all__ = [
    "Trait",
    "PseudoType",
    "Ok",
    "NotOk",
    "Some",
    "Err",
    "ExtensionProperty",
    "register_global_attr",
    "register_global_trait",
]
