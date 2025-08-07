export Monad = class(Trait):
    """
    Abstract base class for monads.

    Subclasses must implement `bind` and `pure`.
    Automatically provides standard monadic operations: bind_once, map, and apply.
    """
    __slots__ = ()

    bind = Abstract& (self, f) => None

    pure = staticmethod& Abstract& value => None

    # Automatically generated implementations.
    map = (self, f) => self.bind(x => self.pure(f(x)))
    apply = (self, f) => self.bind(x => f.map(fn => fn(x)))


# TODO: enforce binding only once?
export MonadOnce = class(Monad, Trait):
    """
    Abstract base class for single-use monads.

    A MonadOnce can only be bound once, which is suitable for
    use with Python generators that cannot be cloned or reused.

    Subclasses must implement `bind_once` and `pure`.
    Automatically provides (likely suboptimal) standard monadic operations: bind, map, and apply.
    """
    __slots__ = ()

    # The default implementation required for `@` syntax that should be overridden by subclasses.
    bind_once = Abstract& (self, f) => None

    # An optional, optimized implementation of `bind` that skips deep recursion.
    # TODO: can this be automatically generated from bind_once?
    # bind_gen = (self, gen) => ...

    # Automatically generated implementations.
    bind = (self, f) => self.bind_once(f)
    map = (self, f) => self.bind(x => self.pure(f(x)))
    apply = (self, f) => self.bind(x => f.map(fn => fn(x)))