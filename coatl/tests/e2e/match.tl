# TODO: match is broken

import util.assert_eq

assert_eq(
    1 match 1 if False: 2 else: 3,
    3
)

x = 1
assert_eq(
    1 match .x: 2 else: 3,
    2
)

x = 1
assert_eq(
    2 match .x: 2 else: 3,
    3
)

assert_eq(
    2 match x: 2,
    2
)

assert_eq(
    [1, 2, 3] match [3, *x]: x else: 123,
    123,
)

assert_eq(
    [1, 2, 3] match [1, *x]: x else: 123,
    [2, 3].(list),
)

assert_eq(
    [1, 2, 3] match:
        [1, *x]: x
        : 123
    [2, 3].(list),
)