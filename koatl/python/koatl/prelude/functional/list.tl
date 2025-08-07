import itertools

import .Monad
import ..iterable.Iterable

export List = class(Monad):
    bind_once = (self, f) =>
        raise NotImplementedError(
            "Binding List in a do-block doesn't work in Koatl " +
                "due to generator limitations. " +
                "Wrap in Ok to use the regular Result monad."
        )

    bind = (self, f) => List(self.flat_map(f))

    pure = staticmethod& x => [x]

    traverse = Iterable.traverse


for name, method in List.__dict__:
    if name.startswith("_"):
        continue

    ExtensionMethod(list, name)& method