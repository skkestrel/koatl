import collections.defaultdict
import functools.wraps
import .result.(Result, Ok)
import .MonadOnce

export Memo = class(MonadOnce):
    Cache = class:
        __init__ = self =>
            self.cache = defaultdict(dict)

        __repr__ = self => f"Memo.Cache({self.cache})"

        try_get = (self, name, deps) => try self.cache[name][deps] except KeyError()

        update = (self, name, deps, value) =>
            self.cache[name][deps] = value
            value

    __init__ = (self, f) => self.f = f

    __repr__ = self => f"Memo(...)"

    # This needs to be special cased because if we try to use Memo.value
    # directly, the *generator* itself gets memoized. We want to memoize
    # the thing that that comes out of the generator.
    bind_value = staticmethod& (id, deps, f) =>
        Memo(ctx =>
            if ctx.try_get(id, tuple(deps)) matches Ok(value):
                return value

            ctx.update(id, tuple(deps), f().f(ctx))
        )

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
                v = v.f(ctx)

            ctx.update(id, deps, v)
        )

    run = (self, ctx=None) =>
        if ctx === None:
            ctx = Memo.Cache()
        self.f(ctx)

    pure = staticmethod& value => Memo(ctx => value)

    bind_once = (self, f) => Memo(ctx =>
        let value = f(self.f(ctx))
        if value matches Memo():
            return value.f(ctx)
        value
    )

    bind_gen = (self, gen) => Memo(ctx =>
        self = self.f(ctx)
        try:
            while True:
                self = gen.send(self)
                if self matches Memo():
                    self = self.f(ctx)
        except StopIteration(value=value):
            return value
    )

__tl__.memo_value = Memo.value
__tl__.bind_memo_value = Memo.bind_value