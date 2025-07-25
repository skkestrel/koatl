import asyncio
import functools.wraps
import koatl.runtime.traits.register_global_attr

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

export Async = class:
    __init__ = (self, generator) =>
        self.generator = generator

    __await__ = self =>
        for future in self.generator:
            @@(try future.__await__() except AttributeError())
                !map_err(e => TypeError(f"Object {future} is not awaitable: {e}"))
                .(Ok.assert)

    __repr__ = self =>
        return "Async(...)"

    do = staticmethod(
        f => wraps(f)((*args, **kwargs) => Async(f(*args, **kwargs)))
    )

    sleep = staticmethod(asyncio.sleep)
    create_task = staticmethod(asyncio.create_task)
    gather = staticmethod(asyncio.gather)
    wait = staticmethod(asyncio.wait)
    run = asyncio.run

Ok.do = staticmethod(
    f => (*args, **kwargs) =>
            gen = f(*args, **kwargs)

            try:
                x = gen.send(None)
                while True:
                    if not Ok(x):
                        return x

                    x = gen.send(x)
            except StopIteration(value=value) as e:
                return value
        | wraps(f)
)

Ok.assert = staticmethod(
    value =>
        value match:
            BaseException() as e => raise e
            None => raise ValueError("Expected a value, got None")
            default value
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