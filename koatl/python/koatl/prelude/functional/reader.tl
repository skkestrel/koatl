import functools.wraps

export Reader = class:
    NoKey = object()

    __init__ = (self, fn) => self.fn = fn

    __repr__ = self => "Reader(...)"

    run = (self, ctx) => self.fn(ctx)

    # TODO - can can this be optimized?
    bind_once = (self, f) => &[Reader] ctx =>
            v = f(self.fn(ctx))
            if v matches Reader():
                v.run(ctx)
            else:
                v

    ask = &[staticmethod] (key=NoKey) => Reader(
        key === Reader.NoKey then ctx => ctx else ctx => ctx[key]
    )

    pure = &[staticmethod] value => Reader(ctx => value)