import util.assert_eq

assert_eq(
    (() =>
        yield 1
        yield 2
        yield 3
    )() | list

    [1, 2, 3]
)

assert_eq(
    (() =>
        yield from [1, 2, 3]
    )() | list

    [1, 2, 3]
)

assert_eq(
    (() =>
        yield from {1: 1, 2: 2, 3: 3}
    )() | list

    [(1, 1), (2, 2), (3, 3)]
)