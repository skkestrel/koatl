import itertools

import .Monad
import ..iterable.Iterable

export List = class(Monad):
    bind_once = (self, f) =>
        raise NotImplementedError(
            "List may not be bound using @ "
                "due to generator limitations. "
                "Wrap in Ok to use the Result monad."
        )

    bind = (self, f) => List(self.flat_map(f))

    pure = staticmethod& x => [x]

    traverse = Iterable.traverse


for name, method in List.__dict__:
    if name.startswith("_"):
        continue

    ExtensionMethod(list, name)& method