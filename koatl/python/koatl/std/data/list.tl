import itertools

import koatl.std.control.Monad

export List = class(Monad):
    __new__ = (cls, *args, **kwargs) => raise ValueError(
        "List cannot be instantiated directly. "
            + "Use [] or list()."
    )

    bind_once = (self, f) =>
        raise NotImplementedError(
            "List may not be bound using @ "
                + "due to generator limitations. "
                + "Wrap in Ok to use the Result monad."
        )

    bind = (self, f) => List(self.flat_map(f))

    pure = staticmethod! x => [x]

    len = property! self => len(self)