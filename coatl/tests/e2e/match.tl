# TODO: match is broken

import util.assert_eq

assert_eq(
    [1, 2, 3] match [3, *x]: x else 123,
    123,
)

assert_eq(
    [1, 2, 3] match [1, *x]: x else 123,
    [2, 3].(list),
)

assert_eq(
    [1, 2, 3] match:
        [1, *x]: x
        123
    [2, 3].(list),
)