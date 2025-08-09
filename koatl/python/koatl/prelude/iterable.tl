import itertools
import builtins
import .functional.Traversable
import .functional.(Ok, Err)

export Iterable = ExtensionTrait& class(Traversable, Trait):
    iter = Abstract

    skip = (self, n) =>
        let it = self.iter
        for _ in ..n:
            next(it, None)
        it

    take = (self, n) =>
        let impl = () =>
            let it = self.iter
            for _ in ..n:
                try:
                    yield next(it)
                except StopIteration(value=value):
                    return value

        impl.__name__ = f"take"
        impl.__qualname__ = f"Iterable.take"

        impl()

    take_while = (self, f) =>
        let impl = () =>
            for i in self.iter:
                if not f(i):
                    return None
                yield i

        impl.__name__ = f"take_while"
        impl.__qualname__ = f"Iterable.take_while"

        impl()

    chain = (self, *others) =>
        itertools.chain(self.iter, *others.map($.iter))

    zip = (self, *others) =>
        zip(self.iter, *others.map($.iter))

    enumerate = (self, start=0) =>
        enumerate(self.iter, start)

    map = (self, f) => builtins.map(f, self.iter)

    filter = (self, f) => builtins.filter(f, self.iter)

    flat_map = (self, f) => itertools.chain.from_iterable(self.map(f))

    reverse = self => reversed(list(self.iter))

    sort = (self, key=None, reverse=False) =>
        sorted(self.iter, key=key, reverse=reverse)

    copy = self => itertools.tee(self.iter)


    count = self => len(self.iter)

    fold = (self, init, f) =>
        let acc = init
        for i in self:
            acc = f(acc, i)
        acc

    associate = (self, f) =>
        let acc = dict() # Don't use Record here.
        for i in self:
            let value = f(i)
            acc[i] = value
        acc

    group_by = (self, f) =>
        let acc = dict()
        for i in self:
            let key = f(i)
            if not acc.__contains__(key):
                acc[key] = []
            acc[key].append(i)
        acc

    count_by = (self, f) =>
        let acc = dict()
        for i in self:
            let key = f(i)
            if not acc.__contains__(key):
                acc[key] = 0
            acc[key] += 1
        acc

    all = (self, f) =>
        for i in self:
            if not f(i):
                return False
        return True

    any = (self, f) =>
        for i in self:
            if f(i):
                return True
        return False

    find = (self, f) =>
        for i in self:
            if f(i):
                return Ok(i)
        return Err()

    first = self =>
        (try next(self.iter)).map_err(_ => None)

    last = (self, f) =>
        let result = Err()
        for i in self:
            if f(i):
                result = Ok(i)
        return result

    at = (self, index) =>
        for i in self:
            if i == index:
                return Ok(i)
        return Err()

    sum = self =>
        let acc = 0
        for i in self:
            acc = acc + i
        acc

    for_each = (self, f) =>
        for i in self:
            f(i)

    list = self =>
        list(self.iter)

    dict = self =>
        dict(self.iter)

    record = self =>
        Record(self.iter)

    traverse = (self, f=None) =>
        import .functional.(Async, Result)

        if f === None:
            f = x => x

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
