import util.assert_eq

# parsing test
1 match:
    1 if False => 2
    _ => 3

assert_eq(
    1 match:
        1 if False => 2
        _ => 3
    3
)

assert_eq(
    1 match:
        1 if False => 2
        _ => 3
    3
)

x = 1
assert_eq(
    1 match:
        .x => 2
        _ => 3
    2
)

assert_eq(
    1 match:
        .x => 2
        _ => 3
    2
)

assert_eq(
    1 match:
        .x => 2
        _ => 3
    2
)

assert_eq(
    1 match:
        .x => 2
        _ => 3
    2
)

x = 1
assert_eq(
    2 match:
        .x => 2
        _ => 3
    3
)

assert_eq(
    2 match:
        x => 2
    2
)

assert_eq(
    [1, 2, 3] match:
        [3, *x] => x
        _ => 123
    123
)

assert_eq(
    [1, 2, 3] match:
        [1, *x] => x
        _ => 123
    [2, 3] | list
)

assert_eq(
    [1, 2, 3] match:
        [1, *x] => x
        _ =>
            123
    [2, 3] | list
)

x = 1
assert_eq([1, 2, 3] matches [1, 2, 3], True)
assert_eq([1, 2, 3] matches [.x, _, 3], True)

f = () =>
    if [1, 2, 3] matches [1, 2, x]:
        assert_eq(x, 3)
    else:
        raise

    if [1, 2, 3] not matches [1, 2, x]:
        raise
    else:
        assert_eq(x, 3)

f()