import functools.wraps
import .monad.Monad
import koatl.runtime.virtual.register_global_attr

ok = x =>
    if x === None:
        return False
    if isinstance(x, BaseException):
        return False
    return True

export Result = class:
    __slots__ = ()
    __match_args__ = ("value",)

    __new__ = (cls, value) =>
        if isinstance(value, Result):
            return value
        value.ok then Ok(value) else Err(value)

    __init__ = (self, *args, **kwargs) =>
        raise ValueError("Result should not be instantiated directly, use Ok or Err")

    raw = self =>
        """
        Unwraps the value from a Result, making it look like
        a non-monadic Python value.
        """
        if self matches not Result():
            return self

        if self.ok:
            if self.value.ok:
                return self.value
            else:
                return self
        else:
            if self.value.ok:
                return self
            else:
                return self.value

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


# Provide a default implementation for *most* Result methods
# ...we shouldn't provide a default for `map` since that
# would prevent virtual trait lookup. TODO: fix this?
for name, method in Result.__dict__:
    if name.startswith("_"):
        continue
    if name == "map":
        continue

    let make_closure = method => (self, *args, **kwargs) => Result.raw(method(self, *args, **kwargs))

    register_global_attr(object, name, make_closure(method))

register_global_attr(object, "ok", ExtensionProperty(ok))

__tl__.ok = ok

__tl__.op_coal = (x, f) =>
    if not x.ok:
        return f()
    return x

__tl__.op_map = (x, f) =>
    if x.ok:
        return f()
    return x