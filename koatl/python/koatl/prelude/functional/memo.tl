import collections.defaultdict
import functools.wraps

import .algebra.(MonadOnce, Identity)
import .result.(Result, Ok, Err)
import .async.(Async)

export Memo = class(MonadOnce):
    __slots__ = ("f",)

    """
    The Memo monad is like a a specialization of Env, where
    the environment is a cache that stores previously computed values.
    It is used to memoize computations, allowing for efficient reuse of results
    without recomputing them.

    As a special case, memoized Async computations are fused together
    with the Memo, behaving like a Memo.T<Async<T>>.
    (However, the type is still Memo<Async<T>>.)
    Unfortunately, this is a source of much confusion...
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

    value = staticmethod& (id, deps, f) =>
        """
        Create a memoized value with the given id and dependencies.
        f should be a function taking no arguments that returns the value to be memoized.
        The id is a unique identifier for the memoized value, and deps are the dependencies
        that determine when the value should be recomputed.
        If the value is already cached, it will return the cached value.
        If the value is not cached, it will compute the value using f and cache it.
        """
        Memo(ctx =>
            if ctx.try_get(id, tuple(deps)) matches Ok((inner_type, value)):
                if inner_type === None or inner_type === Memo:
                    # Non-monadic cached values, or Memo cached values, shouldn't
                    # wrap the output in a monadic type.
                    return @Identity(value)
                else:
                    return @inner_type.pure(value)

            let inner = f()
            let inner_type

            if isinstance(inner, Memo):
                # Fuse Memo<Memo<T>> into Memo<T>.
                inner_type = None
                inner = @Identity(inner.f(ctx))
            else if isinstance(inner, Async):
                # Fuse Memo<Async<T>>.
                inner_type = Async
                inner = @inner
            else:
                inner_type = None
                inner = @Identity(inner)

            ctx.update(id, tuple(deps), (inner_type, inner))
            inner
        )

    fn = staticmethod& f => wraps(f)& (*args, **kwargs) =>
        let id = f"{f.__module__}.{f.__qualname__}"
        let deps = (tuple(args), tuple(kwargs.items()))
        Memo.value(id, deps, () => f(*args, **kwargs))


    run = (self, ctx=None) =>
        if ctx === None:
            ctx = Memo.Cache()
        self.f(ctx)

    pure = staticmethod& value => Memo(ctx => value)

    bind_once = (self, f) => Memo(ctx =>
        let inner = self.f(ctx)

        if isinstance(inner, Async):
            inner = @inner
        else:
            inner = @Identity(inner)

        let value = f(inner)

        if value matches not Memo():
            raise ValueError(f"Expected the binder to return a Memo, but got {type(value)}")

        return value.f(ctx)
    )

    bind_gen = (self, gen) => Memo(ctx =>
        try:
            while True:
                let inner = self.f(ctx)
                if isinstance(inner, Async):
                    inner = @inner
                else:
                    inner = @Identity(inner)

                self = gen.send(inner)

                if self matches not Memo():
                    raise ValueError(f"Expected the binder to return a Memo, but got {type(value)}")

        except StopIteration(value=value):
            return value
    )


__tl__.memo_value = Memo.value