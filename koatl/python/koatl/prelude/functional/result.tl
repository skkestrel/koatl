import functools.wraps
import .algebra.(Identity, MonadOnce)

export Result = class(MonadOnce):
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

        f(self.value)

    bind_gen = (self, gen) =>
        self = Result(self)
        try:
            while True:
                if not self.ok:
                    return self

                self = Result(gen.send(self.value))
        except StopIteration(value=value):
            return Result(value)

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

    T = class(MonadOnce):
        __slots__ = ("value",)
        __init__ = (self, value) =>
            if not hasattr(value, "bind_once"):
                raise TypeError("Expected a MonadOnce value")

            self.value = value

        __repr__ = self => f"Result.T({repr(self.value)})"

        pure = (self, x) => self.value.pure(Result.pure(x))

        bind_once = (self, f) =>
            self.value.bind_once(x =>
                x = Result(x)

                if not x.ok:
                    return self.value.pure(x)

                f(x.value)
            )


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

    __eq__ = (self, other) =>
        if not isinstance(other, Ok):
            return False
        return self.value == other.value

    __repr__ = self => f"Ok({repr(self.value)})"
    unwrap = self => self.value
    coalesce = (self, f) => self.value


export Err = class(Result):
    __slots__ = ("value",)
    ok = False
    __new__ = (cls, value=None) => object.__new__(cls)
    __init__ = (self, value=None) =>
        if hasattr(self, "value"):
            return None
        self.value = value

    __eq__ = (self, other) =>
        if not isinstance(other, Err):
            return False
        return self.value == other.value

    __repr__ = self => f"Err({self.value === None then "" else repr(self.value)})"
    unwrap = self =>
        if self.value matches BaseException():
            raise self.value
        raise ValueError(f"Expected Ok, got {repr(self.value)}")
    coalesce = (self, f) => f()


ExtensionProperty(type(None), "ok")& _ => False
ExtensionProperty(BaseException, "ok")& _ => False
ExtensionProperty(object, "ok")& _ => True

ExtensionProperty(object, "result")& Result

__tl__.Ok = Ok
__tl__.Err = Err
__tl__.Result = Result

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