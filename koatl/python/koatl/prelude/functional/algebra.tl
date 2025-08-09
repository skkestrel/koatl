export Monad = class(Trait):
    """
    Abstract base class for monads.

    Subclasses must implement `bind` and `pure`.
    Automatically provides (likely suboptimal) standard
    functor operations: map, apply, lift.
    """
    __slots__ = ()

    bind = Abstract& (self, f) => None

    pure = staticmethod& Abstract& value => None

    # Automatically generated implementations.
    map = (self, f) => self.bind(x => self.pure(f(x)))
    apply = (self, mf) => self.bind(x => mf.map(f => f(x)))


export MonadOnce = class(Monad, Trait):
    """
    Abstract base class for monads that don't require branching.

    A MonadOnce only calls `f` in `bind_once` once, which is suitable for
    use with Python generators that cannot be cloned or reused.

    Subclasses must implement `bind_once` and `pure`.

    Subclasses may also implement `bind_gen` for more efficient
    handling of do-blocks.
    """
    __slots__ = ()

    # The default implementation required for `@` syntax that should be overridden by subclasses.
    bind_once = Abstract& (self, f) => None

    # An optional, optimized implementation of `bind` that skips deep recursion.
    # TODO: can this be automatically generated from bind_once?
    # bind_gen = (self, gen) => ...

    # Automatically generated implementations.
    bind = (self, f) => self.bind_once(f)


export Identity = class(MonadOnce):
    __init__ = (self, *args, **kwargs) => raise ValueError(
        "Identity should not be instantiated directly. "
    )

    bind_once = (self, f) => f(self)

    pure = staticmethod& x => x


export Traversable = class(Trait):
    """
    Abstract base class for traversable data structures.
    Subclasses must implement `traverse`.
    """
    __slots__ = ()

    traverse = Abstract& (self, f) => None