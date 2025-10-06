import collections.defaultdict
import functools.wraps

import .base.(MonadOnce, Identity)
import .result.(Result, Ok, Err)
import .async.(Async)

export Memo = class(MonadOnce):
    __slots__ = ("f",)

    """
    The Memo monad is like a a specialization of Env, where
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

        try_get = (self, name, deps) => check self.cache[name][deps] except KeyError()

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

    value = staticmethod! (id, deps, f) =>
        """
        Create a memoized value with the given id and dependencies.
        f should be a function taking no arguments that returns the value to be memoized.
        The id is a unique identifier for the memoized value, and deps are the dependencies
        that determine when the value should be recomputed.
        If the value is already cached, it will return the cached value.
        If the value is not cached, it will compute the value using f and cache it.
        """
        Memo(ctx =>
            if ctx.try_get(id, tuple(deps)) matches Ok(value):
                return value

            let inner = f()
            let inner_type

            if isinstance(inner, Memo):
                # Collapse Memo<Memo<T>> into Memo<T>.
                # Usually, this should only happen inside .flat(),
                # but since we don't have type inference,
                # we need to do it here to nesting memo inside memo.
                inner = inner.f(ctx)

            ctx.update(id, tuple(deps), inner)
        )

    fn = staticmethod! f => wraps(f)! (*args, **kwargs) =>
        let id = f"{f.__module__}.{f.__qualname__}"
        let deps = (tuple(args), tuple(kwargs.items()))
        Memo.value(id, deps, () => f(*args, **kwargs))

    run = (self, ctx=None) =>
        if ctx === None:
            ctx = Memo.Cache()
        self.f(ctx)

    pure = staticmethod! value => Memo(ctx => value)

    bind_once = (self, f) => Memo(ctx =>
        let value = f(self.f(ctx))

        if value not matches Memo():
            raise ValueError(f"Expected the binder to return a Memo, but got {type(value)}")

        return value.f(ctx)
    )

    bind_gen = (self, gen) => Memo(ctx =>
        try:
            while True:
                let inner = self.f(ctx)
                self = gen.send(inner)

                if self not matches Memo():
                    raise ValueError(f"Expected the binder to return a Memo, but got {type(value)}")
        except StopIteration(value=value) =>
            return value
    )



export AsyncMemo = class(MonadOnce):
    """
    Like Memo, but fuses Async computations with the Memo itself.
    """

    __slots__ = ("f",)

    __init__ = (self, f) => self.f = f

    __repr__ = self => f"AsyncMemo(...)"

    value = staticmethod! (id, deps, f) =>
        """
        Like Memo.value, but we need to cache the result of the Async.
        """
        AsyncMemo(ctx =>
            # This pauses the generator to wait for an Async context.
            # This allows with_ctx to work a little bit better with memoization,
            # deferring the cache lookup until we are in an async context.
            @Async.pure(None)

            if ctx.try_get(id, tuple(deps)) matches Ok(value):
                return value

            let inner = f()
            let inner_type

            if isinstance(inner, Memo):
                inner = inner.f(ctx)

            if not isinstance(inner, Async):
                raise ValueError(f"Expected the inner computation to be Async, but got {type(inner)}")

            inner = @inner

            ctx.update(id, tuple(deps), inner)
        )

    fn = staticmethod! f => wraps(f)! (*args, **kwargs) =>
        let id = f"{f.__module__}.{f.__qualname__}"
        let deps = (tuple(args), tuple(kwargs.items()))
        AsyncMemo.value(id, deps, () => f(*args, **kwargs))

    run = (self, ctx=None) =>
        if ctx === None:
            ctx = Memo.Cache()
        self.f(ctx).run()

    with_ctx = (self, ctx) => self.f(ctx)

    pure = staticmethod! value => AsyncMemo(ctx => Async.pure(value))

    bind_once = (self, f) => AsyncMemo(ctx =>
        let value = f(@self.f(ctx))

        if value not matches AsyncMemo():
            raise ValueError(f"Expected the binder to return a Memo, but got {type(value)}")

        return value.f(ctx)
    )

    bind_gen = (self, gen) => AsyncMemo(ctx =>
        try:
            while True:
                let inner = @self.f(ctx)
                self = gen.send(inner)

                if self not matches AsyncMemo():
                    raise ValueError(f"Expected the binder to return a Memo, but got {type(value)}")

        except StopIteration(value=value) =>
            return value
    )