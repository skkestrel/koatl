import functools.wraps
import .monad.Monad
import koatl.runtime.virtual.register_global_attr

export Result = class(Monad):
    bind_once = (self, f) => self match:
        Ok(value) => f(value)
        Ok() => f(self)
        default => self

    bind_gen = (self, gen) =>
        try:
            while True:
                if not __tl__.ok(self):
                    return self

                if self matches Ok(value):
                    self = value

                self = gen.send(self)
        except StopIteration(value=value):
            if value === None:
                return Ok(None)
            return value

    map_err = (self, f) =>
        if self matches Err():
            return f(self)
        else:
            return self

    map_none = (self, f) =>
        if self matches None:
            return f()
        else:
            return self

OkMeta = class(type):
    __instancecheck__ = (cls, instance) => __tl__.ok(instance)

export Ok = class(metaclass=OkMeta):
    __match_args__ = ("value",)

    __init__ = (self, value) =>
        self.value = value

    # Since we are using a custom metaclass, we can't derive from Result
    # so need to copy the methods over manually from Result

    # We could just use the object fallback methods, but this avoids
    # the manual __tl__.vget so is slightly faster
    bind_once = Result.bind_once
    bind_gen = Result.bind_gen
    map_err = Result.map_err
    map_none = Result.map_none

    assert = staticmethod& value =>
        value match:
            BaseException() as e => raise e
            None => raise ValueError("Expected a value, got None")
            default value

export Err = BaseException

register_global_attr(
    object,
    "map_err",
    Result.map_err
)

register_global_attr(
    object,
    "map_none",
    Result.map_none
)

register_global_attr(
    object,
    "bind",
    Result.bind_once
)

register_global_attr(
    object,
    "bind_once",
    Result.bind_once
)

register_global_attr(
    object,
    "bind_gen",
    Result.bind_gen
)