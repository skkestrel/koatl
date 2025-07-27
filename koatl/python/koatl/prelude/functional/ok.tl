import functools.wraps
import koatl.runtime.virtual.register_global_attr

OkMeta = class(type):
    __instancecheck__ = (cls, instance) => __tl__.ok(instance)

export Ok = class(metaclass=OkMeta):
    __init__ = (self, value) =>
        self.value = value

    assert = &[staticmethod] value =>
        value match:
            BaseException() as e => raise e
            None => raise ValueError("Expected a value, got None")
            default value

export Err = BaseException

register_global_attr(
    type(None),
    "bind_once",
    (self, f) => self
)

register_global_attr(
    BaseException,
    "bind_once",
    (self, f) => self
)

register_global_attr(
    Ok,
    "bind_once",
    (self, f) => f(self.value)
)

register_global_attr(
    object,
    "bind_once",
    (self, f) => f(self)
)

register_global_attr(
    object,
    "map_err",
    (self, f) =>
        if self matches Err():
            return f(self)
        else:
            return self
)