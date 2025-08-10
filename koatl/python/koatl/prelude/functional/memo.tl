import collections.defaultdict
import functools.wraps

import .algebra.(MonadOnce, Identity)
import .result.(Result, Ok, Err)
import .env.Env

export Memo = class(Env):
    __slots__ = ("f",)

    """
    The Memo monad is a specialization of Env, where
    the environment is a cache that stores previously computed values.
    It is used to memoize computations, allowing for efficient reuse of results
    without recomputing them.
    """

    Cache = class:
        """
        Basic implementation of a cache without any eviction policy.
        """
        __init__ = self =>
            self.cache = defaultdict(dict)

        __repr__ = self => f"Memo.Cache({self.cache})"

        try_get = (self, name, deps) => try self.cache[name][deps] except KeyError()

        update = (self, name, deps, value) =>
            self.cache[name][deps] = value
            value

    NullCache = class:
        """
        A cache that does not store any values.
        Useful for testing or when memoization is not needed.
        """
        __init__ = self => None

        __repr__ = self => "Memo.NullCache()"

        try_get = (self, name, deps) => Err()

        update = (self, name, deps, value) => value

    __init__ = (self, f) => self.f = f

    __repr__ = self => f"Memo(...)"

    # We need to handle
    # memo @value
    # separately, because we *don't* want to memoize the monad itself
    # but rather the value it produces.
    # Another way to think about it is that
    # fused_value produces a Memo.T<MonadOnce<T>> value.
    fused_value = staticmethod& (id, deps, f) =>
        Memo(ctx =>
            if ctx.try_get(id, tuple(deps)) matches Ok((inner_type, value)):
                if inner_type === Memo:
                    # This enables nesting Memo inside a memo block.
                    # The issue is that this do-block expects a value to be bound,
                    # but we don't want to use @Memo.pure(value) because that would
                    # produce a Memo<Memo<T>> value.
                    # It's very hacky, but it works.
                    return @Identity(value)
                else:
                    # This gives us a Memo<MonadOnce<T>> value as normal.
                    return @inner_type.pure(value)

            let inner = f()
            let inner_type = type(inner)

            if isinstance(inner, Memo):
                inner = @Identity(inner.f(ctx))
            else:
                inner = @inner

            ctx.update(id, tuple(deps), (inner_type, inner))
            inner
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

        # Same semantics as fused_value.
        Memo.fused_value(id, deps, () => f(*args, **kwargs))


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
__tl__.bind_memo_value = Memo.fused_value