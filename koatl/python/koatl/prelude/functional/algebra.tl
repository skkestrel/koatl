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

    # Fuses together this monad with an inner monad, so bind acts on both at once.
    # Another way to think about it is as a Monad transformer.
    fuse = (self, inner_type=None) => FusedMonadOnce(self, inner_type)

    # An optional, optimized implementation of `bind` that skips deep recursion.
    # TODO: can this be automatically generated from bind_once?
    # bind_gen = (self, gen) => ...

    # Automatically generated implementations.
    bind = (self, f) => self.bind_once(f)


export FusableMonadOnce = class(MonadOnce, Trait):
    """
    Abstract base class for monads that can be transformed, such as Result or Reader.

    Subclasses must implement `fused_bind_once`.
    """
    __slots__ = ()

    fused_bind_once = Abstract& (self, f, outer_type) => None
    bind_once = (self, f) => self.fused_bind_once(f, Identity)


export FusedMonadOnce = class(MonadOnce):
    """
    A monad that represents the fusion of an outer monad and an inner monad.
    The inner monad is specified by `inner_type`, which may be None to
    allow for type inference on the first bind.

    The inner type must implement FusableMonadOnce.
    """

    __slots__ = ("outer", "inner")

    __init__ = (self, outer, inner_type=None) =>
        self.outer = outer
        self.inner_type = inner_type

    __repr__ = self => f"FusedMonadOnce({self.outer})"

    pure = (self, x) =>
        if self === FusedMonadOnce:
            raise ValueError(
                "Cannot call pure on FusedMonadOnce directly. "
                    "Use an instance instead, or Outer.pure(Inner.pure(x))"
            )

        if self.inner_type === None:
            raise ValueError(
                "Could not infer the inner monad type for FusedMonadOnce. "
                    "Use .fuse(TInner) to explicitly specify the inner monad type."
            )

        self.outer.pure(self.inner_type.pure(x))

    bind_once = (self, f) => self.outer.bind_once(inner =>
        if self.inner_type === None:
            self.inner_type = type(inner)

        if not isinstance(inner, self.inner_type):
            raise TypeError(
                f"Expected {self.inner_type}, got {type(inner)}"
            )

        inner.fused_bind_once(f, type(self.outer))
    )

    # TODO: how to implement bind_gen?

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