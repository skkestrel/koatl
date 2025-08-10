import functools.wraps
import .MonadOnce

export Env = class(MonadOnce):
    """
    A monad that provides a *mutable* reference to the environment.
    This means that it can behave like a State, without needing to
    explicitly write updates to the context.

    Structurally, it is identical to Reader.
    """

    __init__ = (self, fn) => self.fn = fn

    __repr__ = self => "Env(...)"

    run = (self, ctx) => self.fn(ctx)

    pure = staticmethod& value => Env(ctx => value)

    get = None # to be populated later

    item = staticmethod& name => Env(ctx => ctx[name])

    bind_once = (self, f) => Env& ctx =>
        let v = f(self.fn(ctx))
        if v matches Env():
            return v.fn(ctx)
        v

    bind_gen = (self, gen) => Env& ctx =>
        try:
            while True:
                self = gen.send(self.fn(ctx))
        except StopIteration(value=value):
            return value

    NoKey = object()

Env.get = Env(ctx => ctx)