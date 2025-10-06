import functools.wraps
import .base.MonadOnce

export Env = class(MonadOnce):
    __slots__ = ("f",)

    """
    A monad that provides a *mutable* reference to the environment.
    This means that it can behave like a State, without needing to
    explicitly write updates to the context.

    Structurally, it is identical to Reader.
    """

    __init__ = (self, f) => self.f = f

    __repr__ = self => "Env(...)"

    run = (self, ctx) => self.f(ctx)

    pure = staticmethod! value => Env(ctx => value)

    get = None # to be populated later

    item = staticmethod! name => Env(ctx => ctx[name])

    bind_once = (self, f) => Env! ctx =>
        let v = f(self.f(ctx))
        if v not matches Env():
            raise ValueError(f"Expected the binder to return an Env, but got {type(v)}")

        v.f(ctx)

    bind_gen = (self, gen) => Env! ctx =>
        try:
            while True:
                self = gen.send(self.f(ctx))
                if self not matches Env():
                    raise ValueError(f"Expected the binder to return an Env, but got {type(self)}")
        except StopIteration(value=value) =>
            return value

    NoKey = object()

Env.get = Env(ctx => ctx)