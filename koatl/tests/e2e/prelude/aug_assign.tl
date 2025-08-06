import util.assert_eq

x = 1
x += 2
assert_eq(x, 3)

x = None
x ??= 2
assert_eq(x, 2)

x = 4
x |= $ + 2
assert_eq(x, 6)