import functools.wraps
import .monad.Monad

export Reader = class(Monad):
    __init__ = (self, fn) => self.fn = fn

    __repr__ = self => "Reader(...)"

    run = (self, ctx) => self.fn(ctx)

    bind_once = (self, f) => Reader& ctx =>
        let v = f(self.fn(ctx))
        if v matches Reader():
            v.fn(ctx)
        else:
            v

    bind_gen = (self, gen) => Reader& ctx =>
        try:
            while True:
                self = gen.send(self.fn(ctx))
        except StopIteration(value=value):
            return value

    NoKey = object()

    ask = staticmethod& (key=NoKey) => Reader(
        key === Reader.NoKey then ctx => ctx else ctx => ctx[key]
    )

    pure = staticmethod& value => Reader(ctx => value)