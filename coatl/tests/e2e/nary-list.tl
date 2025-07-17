import util.assert_eq

x, y = 1, 2
assert_eq([x, y], [1, 2])

x, = 1,
assert_eq(x, 1)

x, = [1]
assert_eq(x, 1)