import abc

# TODO: this should be really called MonadOnce.
export Monad = class(abc.ABC):
    __slots__ = ()

    # The default implementation required for `@` syntax that should be overridden by subclasses.
    bind_once = abc.abstractmethod& (self, f) => None

    pure = staticmethod& abc.abstractmethod& value => None

    # Automatically given by bind_once.
    bind = (self, f) => self.bind_once(f)

    # An optional, optimized implementation of `bind` that skips deep recursion.
    # TODO: can this be automatically generated?
    # bind_gen = (self, gen) => ...

    # Automatically generated implementations.
    map = (self, f) => self.bind(x => self.pure(f(x)))
    apply = (self, f) => self.bind(x => f.map(fn => fn(x)))