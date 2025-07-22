import util.assert_eq

assert_eq((x => x)($)(1), 1)

assert_eq(1
    | $ + 1
    | $ * 2
    | $ * 5, 20)

f = $ + 1

assert_eq(f(2), 3)

f = x => x * 2
g = f($)

assert_eq(g(3), 6)

assert_eq(3 | f($), 6)
assert_eq(f($)(3), 6)

assert_eq(3 | [$, 2, 3], [3, 2, 3])
assert_eq([$, 2, 3](3), [3, 2, 3])