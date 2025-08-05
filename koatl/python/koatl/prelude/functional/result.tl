import functools.wraps
import .monad.Monad
import koatl.runtime.virtual.register_global_attr

export Result = class:
    __slots__ = ()
    __match_args__ = ("value",)

    __new__ = (cls, value) =>
        if isinstance(value, Result):
            return value
        value.ok then Ok(value) else Err(value)

    __init__ = (self, *args, **kwargs) =>
        raise ValueError("Result should not be instantiated directly, use Ok or Err")

    checked = staticmethod& f =>
        """
        Runs the function in a try block and returns a Result.
        If the function raises an exception, it will be wrapped in an Err.
        """
        try:
            return Ok(f())
        except BaseException() as e:
            return Err(e)

    bind_once = (self, f) =>
        self = Result(self)
        if not self.ok:
            return self

        let v = Result(f(self.value))

    bind = bind_once

    bind_gen = (self, gen) =>
        self = Result(self)
        try:
            while True:
                if not self.ok:
                    return self

                self = Result(gen.send(self.value))
        except StopIteration(value=value):
            return Result.pure(value)

    pure = staticmethod& x => Ok(x)

    apply = (self, f) =>
        self = Result(self)
        f = Result(f)

        if not self.ok:
            return self
        if not f.ok:
            return f

        Ok(f(self))

    map = (self, f) =>
        self = Result(self)
        if self.ok:
            return Ok(f(self.value))
        return self

    map_err = (self, f) =>
        self = Result(self)
        if not self.ok:
            return Err(f(self.value))
        return self

    map_none = (self, f) =>
        self = Result(self)
        if not self.ok and self.value === None:
            return Err(f())
        return self

export Ok = class(Result):
    __slots__ = ("value",)
    ok = True
    __new__ = (cls, value) => object.__new__(cls)
    __init__ = (self, value) =>
        if hasattr(self, "value"):
            # This is necessary to prevent overwriting the value
            # since Python's Result.__new__ will call Ok.__init__ again
            # if the object is already created.
            return None
        self.value = value

    __repr__ = self => f"Ok({repr(self.value)})"
    unwrap = self => self.value
    coalesce = (self, f) => self.value

export Err = class(Result):
    __slots__ = ("value",)
    ok = False
    __new__ = (cls, value) => object.__new__(cls)
    __init__ = (self, value) =>
        if hasattr(self, "value"):
            return None
        self.value = value

    __repr__ = self => f"Err({repr(self.value)})"
    unwrap = self =>
        if self.value matches BaseException():
            raise self.value
        raise ValueError(f"Expected Ok, got {repr(self.value)}")
    coalesce = (self, f) => f()


register_global_attr(type(None), "ok", ExtensionProperty(_ => False))
register_global_attr(BaseException, "ok", ExtensionProperty(_ => False))
register_global_attr(object, "ok", ExtensionProperty(_ => True))

register_global_attr(object, "result", ExtensionProperty(Result))

# Enables @ operator for bare objects.
register_global_attr(object, "bind_gen", Result.bind_gen)

__tl__.op_coal = (x, f) =>
    if x matches Result():
        return x.coalesce(f)

    if not x.ok:
        return f()
    return x

__tl__.op_map = (x, f) =>
    if x matches Result():
        return x.map(f)

    if x.ok:
        return f(x)
    return x