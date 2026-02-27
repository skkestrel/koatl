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
    if let [1, 2, x] = [1, 2, 3]:
        assert_eq(x, 3)
    else:
        raise

    if not let [1, 2, x] = [1, 2, 3]:
        raise
    else:
        assert_eq(x, 3)

f()

# while let — unwrap Some-like values until None
data = [("Some", 1), ("Some", 2), ("Some", 3), ("None", 0)]
results = []
idx = 0
while let ("Some", val) = data[idx]:
    results.append(val)
    idx = idx + 1

assert_eq(results, [1, 2, 3])
assert_eq(idx, 3)

# while let with list pattern
items = [[1, 2], [3, 4], [5], [6, 7]]
sums = []
i = 0
while let [a, b] = items[i]:
    sums.append(a + b)
    i = i + 1

assert_eq(sums, [3, 7])
assert_eq(i, 2)