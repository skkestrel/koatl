import util.assert_eq

assert_eq(
    (() =>
        @1
        @2
        @3
    )() | list

    [1, 2, 3]
)

assert_eq(
    (() =>
        @@[1, 2, 3]
    )() | list

    [1, 2, 3]
)

assert_eq(
    (() =>
        @@{1: 1, 2: 2, 3: 3}
    )() | list

    [(1, 1), (2, 2), (3, 3)]
)