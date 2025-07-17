import util.assert_eq

assert_eq(.., slice(None, None, None))
assert_eq(0.., slice(0, None, None))
assert_eq(..0, slice(None, 0, None))
assert_eq(...., slice(None, None, None))
assert_eq(..1.., slice(None, 1, None))
assert_eq(..1*5.., slice(None, 5, None))
assert_eq(10+2....10-2, slice(12, None, 8))