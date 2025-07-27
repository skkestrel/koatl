import abc

export import .ok.*
export import .async.*
export import .reader.*

export Fn = class:
    compose = staticmethod& (*args) =>
        args match:
            [] => raise ValueError("At least one function is required for composition")
            [f] => f
            [*fs] =>
                composed = (*args, **kwargs) =>
                    value = fs[-1](*args, **kwargs)
                    for f in fs[..-1..-1]:
                        value = f(value)
                    value

                composed.__name__ = "<Fn.compose()>"
                composed.__qualname__ = composed.__name__
                composed.signature = fs[-1].signature

                composed
            default raise ValueError("Invalid arguments for Fn.compose()")

export Monad = class(abc.ABC):
    bind = (self, f) => self.bind_once(f)

    # An optional, optimized implementation of `bind` that skips deep recursion.
    bind_gen = (self, gen) => raise NotImplementedError()

    # The default implementation required for `@` syntax that should be overridden by subclasses.
    bind_once = abc.abstractmethod& (self, f) => None

    pure = staticmethod& abc.abstractmethod& value => None

register_global_attr(
    object,
    "bind",
    (self, f) => self.bind_once(f)
)