import functools.wraps

export Fn = class:
    compose = staticmethod(
        (*args) =>
            args match:
                [] => raise ValueError("At least one function is required for composition")
                [f] => f
                [*fs] =>
                    composed = (*args, **kwargs) =>
                        value = fs[-1](*args, **kwargs)
                        for f in fs[..-1..-1]:
                            value = f(value)
                        value

                    composed.__name__ = "<Fn.compose()>"
                    composed.__qualname__ = composed.__name__
                    composed.signature = fs[-1].signature

                    composed
                default raise ValueError("Invalid arguments for Fn.compose()")
    )

export untry = x => x matches BaseException() then raise x else x

export map_err = f => value => not Ok(value) then f(value) else value

OkMeta = class(type):
    __instancecheck__ = (cls, instance) => Ok(instance)

export Ok = class(metaclass=OkMeta):
    __new__ = (cls, value) =>
        value match:
            None => False
            BaseException() => False
            _ => True

    do = staticmethod(
        f =>
            wraps(f)(
                (*args, **kwargs) =>
                    gen = f(*args, **kwargs)

                    try:
                        x = gen.send(None)
                        while True:
                            if not Ok(x):
                                return x

                            x = gen.send(x)
                    except StopIteration(value=value):
                        return value
            )
    )

NotOkMeta = class(type):
    __instancecheck__ = (cls, instance) => not Ok(instance)

export NotOk = class(metaclass=NotOkMeta):
    __new__ = (cls, value) => not Ok(value)