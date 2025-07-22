import util.assert_eq

assert_eq([
    1
    2
    *[3]
    4
], [1, 2, 3, 4])

assert_eq([1, 2,
    3, 4], [1, 2, 3, 4])

assert_eq([
    1, 2,
    3, 4,
], [1, 2, 3, 4])

assert_eq([
    1, 2
    3, 4,
], [1, 2, 3, 4])

apply = (x, y) => x(y)
assert_eq(apply(
    x =>
        y = x * 2
        y * 2
    8
), 8 * 2 * 2)

assert_eq(
    apply(
        x =>
            y = x * 2
            y * 2
        8
    )
    8 * 2 * 2
)

assert_eq([
    1: 2
    2: 3
], [1: 2, 2: 3])

assert_eq([
    1 + 2 +
        3
    2 + 3 +
        4
    5    
], [6, 9, 5])