from types import SimpleNamespace
from koatl._rs import fast_vset, fast_vset_trait


def ext_prop(type, name):
    def impl(value):
        value._property = True
        fast_vset(type, name, value)
        return value

    return impl


def ext_method(type, name):
    def impl(value):
        fast_vset(type, name, value)
        return value

    return impl


def ext_trait(type):
    for name, value in type._own_methods.items():
        fast_vset_trait(type.__name__, type._trait_reqs, name, value)
    return type


Extension = SimpleNamespace(
    property=ext_prop,
    method=ext_method,
    trait=ext_trait,
)


__all__ = [
    "Extension",
]
