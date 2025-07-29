methods = {
    map: (x, f) =>
        for i in x:
            yield f(i)

    filter: (x, f) =>
        for i in x:
            if f(i):
                yield i

    flat_map: (x, f) =>
        for i in x:
            yield from f(i)

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