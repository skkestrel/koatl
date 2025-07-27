import abc

export Monad = class(abc.ABC):
    bind = (self, f) => self.bind_once(f)

    # An optional, optimized implementation of `bind` that skips deep recursion.
    bind_gen = (self, gen) => raise NotImplementedError()

    # The default implementation required for `@` syntax that should be overridden by subclasses.
    bind_once = abc.abstractmethod& (self, f) => None

    pure = staticmethod& abc.abstractmethod& value => None