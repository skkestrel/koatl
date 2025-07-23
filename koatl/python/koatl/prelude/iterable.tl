methods = [
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
        l = len(x)
        if index < 0:
            index = index + l
        if (index < 0) + (index >= l):
            raise IndexError("Index out of range")
        return x[index]

    sum: x =>
        x.fold(0, (acc, i) => acc + i)

    list: x =>
        list(x!iter)

    record: x =>
        Record(x!iter)
]

import .mixins.(create_mixin, curse_mixin)

export Iterable = create_mixin("Iterable", methods)
curse_mixin(Iterable, [list, tuple, dict, set, slice, type((() => @None)())])