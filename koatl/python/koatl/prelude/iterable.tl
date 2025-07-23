export iter = __vtable_lookup($, "iter")

traits = [
    map: (x, f) =>
        for i in x:
            @f(i)

    filter: (x, f) =>
        for i in x:
            if f(i):
                @i

    flat_map: (x, f) =>
        for i in x:
            @@f(i)

    fold: (x, init, f) =>
        acc = init
        for i in x:
            acc = f(acc, i)
        acc

    first: (x, f) =>
        for i in x:
            if f(i):
                return i
        return None

    last: (x, f) =>
        result = None
        for i in x:
            if f(i):
                result = i
        return result

    at: (x, index) =>
        if index < 0:
            index = index + len(x)
        if (index < 0) + (index >= len(x)):
            raise IndexError("Index out of range")
        return x[index]

    sum: x =>
        x!fold(0, (acc, i) => acc + i)

    list: x =>
        list(x)
]


__traits["iter"] = Trait("iter", requires_methods=["iter"])
__traits["iter"].add_curried(traits)