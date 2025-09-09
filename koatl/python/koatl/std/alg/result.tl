import functools.wraps
import .base.(Identity, MonadOnce)

infer_ok = x =>
    x match:
        None => False
        BaseException() => False
        _ => True

op_coal = (x, f) =>
    if x matches Result():
        return x.coalesce(f)

    if not infer_ok(x):
        return f()
    return x

op_map = (x, f) =>
    if x matches Result():
        return x.map(f)

    if infer_ok(x):
        return f(x)
    return x

export Result = class(MonadOnce):
    __slots__ = ()
    __match_args__ = ("value",)

    __new__ = (cls, value) =>
        if isinstance(value, Result):
            return value
        infer_ok(value) then Ok(value) else Err(value)

    __init__ = (self, *args, **kwargs) =>
        raise ValueError("Result should not be instantiated directly, use Ok or Err")

    checked = staticmethod! f =>
        """
        Runs the function in a try block and returns a Result.
        If the function raises an exception, it will be wrapped in an Err.
        """
        try:
            return Ok(f())
        except BaseException() as e =>
            return Err(e)

    bind_once = (self, f) =>
        if not self.ok:
            return self

        let value = f(self.value)
        if value not matches Result():
            raise ValueError(f"Expected the binder to return a Result, but got {type(value)}")
        value

    bind_gen = (self, gen) =>
        try:
            while True:
                if not self.ok:
                    return self

                self = gen.send(self.value)
                if self not matches Result():
                    raise ValueError(f"Expected the binder to return a Result, but got {type(self)}")
        except StopIteration(value=value) =>
            return Result(value)

    pure = staticmethod! x => Ok(x)

    apply = (self, f) =>
        if not self.ok:
            return self
        if not f.ok:
            return f

        Ok(f(self))

    map = (self, f) =>
        if self.ok:
            return Ok(f(self.value))
        return self

    map_err = (self, f) =>
        if not self.ok:
            return Err(f(self.value))
        return self

    map_none = (self, f) =>
        if not self.ok and self.value === None:
            return Err(f())
        return self

    T = (typ) =>
        """
        TODO: cache the Result.T class to avoid creating it every time?
        """
        if not isinstance(typ, type):
            raise ValueError(f"Expected a type, but got {type(typ).__name__}")

        let cls = class(MonadOnce):
            __slots__ = ("value",)
            __init__ = (self, value) =>
                if not isinstance(value, typ):
                    raise ValueError(f"Expected a {typ}, but got {type(value).__name__}")

                self.value = value

            __repr__ = self => f"Result.T({typ.__name__})({repr(self.value)})"

            pure = (self, x) => cls(typ.pure(Result.pure(x)))

            bind_once = (self, f) =>
                cls(self.value.bind_once(x =>
                    if not isinstance(x, Result):
                        raise ValueError(f"Expected the inner type to be a Result, but got {type(x).__name__}")

                    if not x.ok:
                        return typ.pure(x)

                    let value = f(x.value)
                    if not hasattr(value, "value"):
                        raise ValueError(
                            f"Expected the binder to return a {cls.__name__}, " +
                                f"but got a {type(value).__name__}"
                        )

                    value.value
                ))

        cls.__name__ = f"Result.T({typ.__name__})"
        cls.__qualname__ = f"Result.T({typ.__name__})"
        cls


export Ok = class(Result):
    __slots__ = ("value",)
    ok = True

    __new__ = (cls, value) => object.__new__(cls)
    __init__ = (self, value) =>
        if hasattr(self, "value"):
            # This is necessary to prevent overwriting the value
            # since Python's Result.__new__ will call Ok.__init__ again,
            # in the case we call Result(Ok(value))
            return None
        self.value = value

    __eq__ = (self, other) =>
        if not isinstance(other, Ok):
            return False
        return self.value == other.value

    __repr__ = self => f"Ok({repr(self.value)})"

    __reduce__ = self =>
        Ok, (self.value,)

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

    __reduce__ = self =>
        Err, (self.value,)

    unwrap = self =>
        if self.value matches BaseException():
            raise self.value
        raise ValueError(f"Expected Ok, got {repr(self)}")
    coalesce = (self, f) => f()
