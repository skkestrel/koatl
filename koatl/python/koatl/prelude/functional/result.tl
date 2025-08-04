import functools.wraps
import .monad.Monad
import koatl.runtime.virtual.register_global_attr

export Result = class(Monad):
    bind_once = (self, f) => self match:
        Ok(value) => f(value)
        Ok() => f(self)
        default => self

    bind = bind_once

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

    pure = staticmethod& x => Ok(x)

    apply = (self, f) =>
        if not __tl__.ok(self):
            return self
        if not __tl__.ok(f):
            return f

        # unwrap the Oks - this is a bit weird
        # but it allows bare types to be Results
        if self matches Ok(value):
            self = self.value
        if f matches Ok(value):
            f = f.value

        f(self)

    # Map must not be defined since it will override any special map
    # implementation for traits, specifically for Iterable -
    # is there a better way around this (in the vtable implementation)?
    # map = ...

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

    assert = staticmethod& value =>
        value match:
            BaseException() as e => raise e
            None => raise ValueError("Expected a value, got None")
            default value


# Since we are using a custom metaclass, we can't derive from Result
# so need to copy the methods over manually from Result

# We could just use the object fallback methods, but this avoids
# the vget so is slightly faster
for name, method in Result.__dict__:
    if name.startswith("_"):
        continue

    setattr(Ok, name, method)
    register_global_attr(object, name, method)

export Err = BaseException