import collections.defaultdict
import functools.wraps
import .result.Ok
import .Monad

export Memo = class(Monad):
    Cache = class:
        __init__ = self =>
            self.cache = defaultdict(dict)

        get_or_compute = (self, name, deps, f) =>
            if try self.cache[name][deps] except KeyError() matches (Ok() as v):
                return v

            let value = f()
            self.cache[name][deps] = value
            value

        __repr__ = self => f"Memo.Cache({self.cache})"

    __init__ = (self, f) => self.f = f

    __repr__ = self => f"Memo(...)"

    value = staticmethod& (id, deps, f) =>
        Memo(ctx => ctx.get_or_compute(id, tuple(deps), f))

    fn = staticmethod& f => wraps(f)& (*args, **kwargs) =>
        let id = f"{f.__module__}.{f.__qualname__}"
        let deps = (tuple(args), tuple(kwargs.items()))

        Memo(ctx =>
            ctx.get_or_compute(id, deps, () =>
                let v = f(*args, **kwargs)
                if v matches Memo():
                    v = v.run(ctx)
                v
            )
        )

    run = (self, ctx=Cache()) => self.f(ctx)

    pure = staticmethod& value => Memo(ctx => value)

    bind_once = (self, f) => Memo(ctx =>
        let value = f(self.run(ctx))
        if value matches Memo():
            value = value.run(ctx)
        value
    )

    bind_gen = (self, gen) => Memo(ctx =>
        self = self.run(ctx)
        try:
            while True:
                self = gen.send(self)
                if self matches Memo():
                    self = self.run(ctx)
        except StopIteration(value=value):
            return value
    )

__tl__.memo = Memo.value