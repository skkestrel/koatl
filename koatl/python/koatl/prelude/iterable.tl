import itertools
import builtins

export Iterable = ExtensionTrait& class(Trait):
    iter = Abstract

    map = (self, f) => builtins.map(f, self.iter)

    filter = (self, f) => builtins.filter(f, self.iter)

    flat_map = (self, f) => itertools.chain.from_iterable(self.map(f))

    traverse = (self, f) =>
        let it = self.iter
        let v
        try:
            v = next(it)
        except StopIteration():
            return []

        let m = f(v)

        if not hasattr(m, "apply"):
            # special case for bare types - slightly more efficient
            # ...also required since bare types don't have .map
            if not m.ok:
                return m

            let acc = [m]
            for v in it:
                let fv = f(v)
                if not fv.ok:
                    return fv
                acc.append(fv)
            return acc

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