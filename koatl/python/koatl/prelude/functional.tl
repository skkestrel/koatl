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

Ok.do = staticmethod(
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
                except StopIteration(value=value) as e:
                    return value
        )
)

import koatl.runtime.traits.register_global_attr
register_global_attr(
    object,
    "map_err",
    (self, f) =>
        if self matches Err():
            return f(self)
        else:
            return self
)