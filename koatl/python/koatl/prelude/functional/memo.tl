import collections.defaultdict
import functools.wraps
import .result.(Result, Ok)
import .Monad

export Memo = class(Monad):
    Cache = class:
        __init__ = self =>
            self.cache = defaultdict(dict)

        __repr__ = self => f"Memo.Cache({self.cache})"

        try_get = (self, name, deps) =>
            # TODO should try operator return a Result directly?
            Result(try self.cache[name][deps] except KeyError())

        update = (self, name, deps, value) =>
            self.cache[name][deps] = value
            value

    __init__ = (self, f) => self.f = f

    __repr__ = self => f"Memo(...)"

    value = staticmethod& (id, deps, f) =>
        Memo(ctx =>
            if ctx.try_get(id, tuple(deps)) matches Ok(value):
                return value

            ctx.update(id, tuple(deps), f())
        )

    fn = staticmethod& f => wraps(f)& (*args, **kwargs) =>
        let id = f"{f.__module__}.{f.__qualname__}"
        let deps = (tuple(args), tuple(kwargs.items()))

        Memo(ctx =>
            if ctx.try_get(id, deps) matches Ok(value):
                return value

            let v = f(*args, **kwargs)
            if v matches Memo():
                v = v.run(ctx)

            ctx.update(id, deps, v)
        )

    run = (self, ctx=None) =>
        if ctx === None:
            ctx = Memo.Cache()
        self.f(ctx)

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