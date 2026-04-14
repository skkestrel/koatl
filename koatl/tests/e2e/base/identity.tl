import util.assert_eq

x = None

assert_eq(x is None, True)
assert_eq(x is not None, False)
assert_eq(x is None, True)
assert_eq(x is not None, False)

y = 1
assert_eq(y is None, False)
assert_eq(y is not None, True)
assert_eq(y is None, False)
assert_eq(y is not None, True)