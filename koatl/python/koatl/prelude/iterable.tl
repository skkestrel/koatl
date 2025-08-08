import itertools
import builtins

export Iterable = ExtensionTrait& class(Trait):
    iter = Abstract

    map = (self, f) => builtins.map(f, self.iter)

    filter = (self, f) => builtins.filter(f, self.iter)

    flat_map = (self, f) => itertools.chain.from_iterable(self.map(f))

    traverse = (self, f) =>
        import .functional.(Async, Result)

        let it = self.iter
        let v
        try:
            v = next(it)
        except StopIteration():
            return []

        let m = f(v)

        # TODO: can special case traversals be generalized?

        # The Result traversal of a list is short-circuiting.
        # Fall back to Result semantics for bare types.
        if m matches Result() or not hasattr(m, "apply"):
            if not m.ok:
                return m

            let acc = [m]
            for v in it:
                let fv = f(v)
                if not fv.ok:
                    return fv
                acc.append(fv)
            return acc

        # The Async traversal of a list is gather.
        if m matches Async():
            let acc = [m]
            for v in it:
                acc.append(f(v))
            return Async.gather(*acc)

        # Fall back to default applicative semantics.
        let acc = m.map([$])
        for v in it:
            m = f(v)
            # acc = liftA2((list, value) => [*list, value])(acc, f(v))
            acc = acc.apply(m.map(x => acc => [*acc, x]))
        acc

    fold = (self, init, f) =>
        let acc = init
        for i in self:
            acc = f(acc, i)
        acc

    first = (self, f) =>
        for i in self:
            if f(i):
                return i
        return None

    last = (self, f) =>
        let result = None
        for i in self:
            if f(i):
                result = i
        return result

    at = (self, index) =>
        for i in self:
            if i == index:
                return i
        raise IndexError("Index out of range")

    sum = self =>
        let acc = 0
        for i in self:
            acc = acc + i
        acc

    list = self =>
        list(self.iter)

    record = self =>
        Record(self.iter)