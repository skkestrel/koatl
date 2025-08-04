import itertools
import builtins

methods = {
    map: (x, f) => builtins.map(f, x.iter)

    filter: (x, f) => builtins.filter(f, x.iter)

    flat_map: (x, f) => itertools.chain.from_iterable(x.map(f))

    traverse: (x, f) =>
        let it = x.iter
        let v
        try:
            v = next(it)
        except StopIteration():
            return []

        let m = f(v)

        if not hasattr(m, "apply"):
            # special case for bare types - slightly more efficient
            # ...also required since bare types don't have .map
            if not __tl__.ok(m):
                return m

            let acc = [m]
            for v in it:
                let fv = f(v)
                if not __tl__.ok(fv):
                    return fv
                acc.append(fv)
            return acc

        let acc = m.map([$])
        for v in it:
            m = f(v)
            # acc = liftA2((list, value) => [*list, value])(acc, f(v))
            acc = acc.apply(m.map(x => acc => [*acc, x]))
        acc

    fold: (x, init, f) =>
        let acc = init
        for i in x:
            acc = f(acc, i)
        acc

    first: (x, f) =>
        for i in x:
            if f(i):
                return i
        return None

    last: (x, f) =>
        let result = None
        for i in x:
            if f(i):
                result = i
        return result

    at: (x, index) =>
        for i in x:
            if i == index:
                return i
        raise IndexError("Index out of range")

    sum: x =>
        let acc = 0
        for i in x:
            acc = acc + i
        acc

    list: x =>
        list(x.iter)

    record: x =>
        Record(x.iter)
}

# TODO merge this with typing.Iterable?

export Iterable = Trait(__name__, "Iterable", methods, requires=["iter"])
register_global_trait(Iterable)