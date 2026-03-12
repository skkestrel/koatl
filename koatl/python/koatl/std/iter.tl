import itertools
import builtins

import koatl.std.data.Record
import koatl.std.trait.Trait
import .control.(Traversable, Ok, Err, Memo, Async, AsyncMemo, Result)

export Iterable = class(Traversable, Trait):
    iter = Trait.abstract! self => None

    debug_iter = (self, **kwargs) =>
        import .debug_iter.(DebugIterator)
        DebugIterator(self.iter, **kwargs)

    traverse = (self, f=None) =>
        if f === None:
            f = x => x

        let it = self.iter
        let v
        try:
            v = next(it)
        except StopIteration() =>
            return []

        let m = f(v)

        # TODO: can special case traversals be generalized?

        if not hasattr(m, "apply"):
            raise ValueError(f"Can only traverse with an Applicative type.")

        # The Result traversal of a list is short-circuiting.
        if m matches Result():
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

        if m matches AsyncMemo():
            return AsyncMemo(ctx =>
                let acc = [m.f(ctx)]

                for v in it:
                    acc.append(f(v).f(ctx))

                return Async.gather(*acc)
            )

        # Fall back to default applicative semantics.
        let acc = m.map([$])
        for v in it:
            m = f(v)
            # acc = liftA2((list, value) => [*list, value])(acc, f(v))
            acc = acc.apply(m.map(x => acc => [*acc, x]))
        acc

export Iterator = class(Trait):
    __next__ = Trait.abstract! self => None

    skip = (self, n) =>
        for _ in ..n:
            next(self, None)
        self

    skip_while = (self, f) =>
        while True:
            try:
                let value = next(self)
                if not f(value):
                    return itertools.chain([value], self)
            except StopIteration() =>
                return self

    take = (self, n) =>
        let it = self
        let impl = () =>
            for _ in ..n:
                try:
                    yield next(it)
                except StopIteration(value=value) =>
                    return value

        impl.__name__ = f"take"
        impl.__qualname__ = f"Iterator.take"

        impl()

    take_while = (self, f) =>
        let it = self
        let impl = () =>
            for i in it:
                if not f(i):
                    return None
                yield i

        impl.__name__ = f"take_while"
        impl.__qualname__ = f"Iterator.take_while"

        impl()

    chain = (self, *others) =>
        itertools.chain(self, *others.map($.iter))

    product = (self, *others) =>
        itertools.product(self, *others.map($.iter))

    cycle = self => itertools.cycle(self)

    zip = (self, *others) =>
        zip(self, *others.map($.iter))

    enumerate = (self, start=0) =>
        enumerate(self, start)

    map = (self, f) => builtins.map(f, self)

    filter = (self, f) => builtins.filter(f, self)

    filter_map = (self, f=None) =>
        let it = self
        let impl = () =>
            if f === None:
                for i in it:
                    if let Ok(value) = Result(i):
                        yield value
            else:
                for i in it:
                    if let Ok(value) = Result(f(i)):
                        yield value

        impl.__name__ = f"filter_map"
        impl.__qualname__ = f"Iterator.filter_map"

        impl()

    flat_map = (self, f=None) =>
        if f === None:
            itertools.chain.from_iterable(self)
        else:
            itertools.chain.from_iterable(builtins.map(f, self))

    reversed = self => reversed(list(self))

    sorted = (self, key=None, reverse=False) =>
        sorted(self, key=key, reverse=reverse)

    tally = (self, f=None) =>
        let acc = 0
        if f === None:
            for _ in self:
                acc += 1
        else:
            for i in self:
                if f(i):
                    acc += 1
        acc

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

    group_by = (self, f=None) =>
        let acc = dict()
        for i in self:
            let key = if f === None then i else f(i)
            if not acc.__contains__(key):
                acc[key] = []
            acc[key].append(i)
        acc

    count_by = (self, f=None) =>
        let acc = dict()
        for i in self:
            let key = if f === None then i else f(i)
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
        (check next(self)).map_err(_ => None)

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

    for_each = (self, f) =>
        for i in self:
            f(i)

    unique = self =>
        let seen = set()
        let it = self
        let impl = () =>
            for i in it:
                if not seen.__contains__(i):
                    seen.add(i)
                    yield i

        impl.__name__ = f"unique"
        impl.__qualname__ = f"Iterator.unique"

        impl()

    join = (self, sep="") =>
        sep.join(builtins.map(str, self))

    sum = self =>
        let acc = 0
        for i in self:
            acc = acc + i
        acc

    mean = self =>
        let acc = 0
        let count = 0
        for i in self:
            acc = acc + i
            count = count + 1
        if count == 0:
            raise ValueError("mean of empty iterable")
        acc / count

    max = (self, key=None) =>
        let v, m
        try:
            v = next(self)
            if key === None:
                m = v
            else:
                m = key(v)
        except StopIteration(value=value) =>
            raise ValueError("max of empty iterable")

        if key === None:
            for i in self:
                if i > m:
                    v = i
                    m = i
        else:
            for i in self:
                let ki = key(i)
                if ki > m:
                    v = i
                    m = ki

        v

    min = (self, key=None) =>
        let v, m
        try:
            v = next(self)
            if key === None:
                m = v
            else:
                m = key(v)
        except StopIteration(value=value) =>
            raise ValueError("min of empty iterable")

        if key === None:
            for i in self:
                if i < m:
                    v = i
                    m = i
        else:
            for i in self:
                let ki = key(i)
                if ki < m:
                    v = i
                    m = ki
        v

    list = self =>
        list(self)

    set = self =>
        set(self)

    tuple = self =>
        tuple(self)

    dict = self =>
        dict(self)

    record = self =>
        Record(self)
