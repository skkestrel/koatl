import util.assert_eq

a = x => x
assert_eq(a(1), 1)

a = (x, y) => [x, y]
assert_eq(a(1, 2), [1, 2])

a = (x, y) =>
    x = x + y
    [x, y]
assert_eq(a(1, 2), [3, 2])

a = x =>
        y =>
            z => z
assert_eq(a(1)(2)(3), 3)

a = x => x
assert_eq(a.__name__, "a")