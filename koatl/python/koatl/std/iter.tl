import itertools
import builtins

import koatl.std.data.Record
import koatl.std.trait.Trait
import .control.(Traversable, Ok, Err, Memo, Async, AsyncMemo, Result)

export Iterable = class(Traversable, Trait):
    iter = Trait.abstract! self => None

    skip = (self, n) =>
        let it = self.iter
        for _ in..n:
            next(it, None)
        it

    skip_while = (self, f) =>
        let it = self.iter
        while True:
            try:
                let value = next(it)
                if not f(value):
                    return Iterable.chain([value], it)
            except StopIteration() =>
                return it

    take = (self, n) =>
        let impl = () =>
            let it = self.iter
            for _ in..n:
                try:
                    yield next(it)
                except StopIteration(value=value) =>
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

    product = (self, *others) =>
        itertools.product(self.iter, *others.map($.iter))

    zip = (self, *others) =>
        zip(self.iter, *others.map($.iter))

    enumerate = (self, start=0) =>
        enumerate(self.iter, start)

    map = (self, f) => builtins.map(f, self.iter)

    filter = (self, f) => builtins.filter(f, self.iter)

    filter_map = (self, f=None) =>
        let impl = () =>
            if f === None:
                for i in self.iter:
                    if Result(i) matches Ok(value):
                        yield value
            else:
                for i in self.iter:
                    if Result(f(i)) matches Ok(value):
                        yield value

        impl.__name__ = f"filter_map"
        impl.__qualname__ = f"Iterable.filter_map"

        impl()

    flat_map = (self, f) => itertools.chain.from_iterable(self.map(f))

    reverse = self => reversed(list(self.iter))

    sorted = (self, key=None, reverse=False) =>
        sorted(self.iter, key=key, reverse=reverse)

    copy = self => itertools.tee(self.iter)

    count = (self, f=None) =>
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
            let key = f === None then i else f(i)
            if not acc.__contains__(key):
                acc[key] = []
            acc[key].append(i)
        acc

    count_by = (self, f=None) =>
        let acc = dict()
        for i in self:
            let key = f === None then i else f(i)
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
        (check next(self.iter)).map_err(_ => None)

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
        let impl = () =>
            for i in self:
                if not seen.__contains__(i):
                    seen.add(i)
                    yield i

        impl.__name__ = f"unique"
        impl.__qualname__ = f"Iterable.unique"

        impl()

    join_str = (self, sep="") =>
        sep.join(self.map(str))

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
        let it = self.iter
        let v, m
        try:
            v = next(it)
            if key === None:
                m = v
            else:
                m = key(v)
        except StopIteration(value=value) =>
            raise ValueError("max of empty iterable")


        if key === None:
            for i in it:
                if i > m:
                    v = i
                    m = i
        else:
            for i in it:
                let ki = key(i)
                if ki > m:
                    v = i
                    m = ki

        v

    min = (self, key=None) =>
        let it = self.iter
        let v, m
        try:
            v = next(it)
            if key === None:
                m = v
            else:
                m = key(v)
        except StopIteration(value=value) =>
            raise ValueError("min of empty iterable")

        if key === None:
            for i in it:
                if i < m:
                    v = i
                    m = i
        else:
            for i in it:
                let ki = key(i)
                if ki < m:
                    v = i
                    m = ki
        v

    list = self =>
        list(self.iter)

    dict = self =>
        dict(self.iter)

    record = self =>
        Record(self.iter)

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
