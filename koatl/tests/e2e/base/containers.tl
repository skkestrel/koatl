import util.assert_eq

assert_eq(type([1, 2, 3]), list)
assert_eq(type((1, 2, 3)), tuple)

# Record type is not directly made available by the runtime.
assert_eq(type({1: 2}).__name__, "Record")

assert_eq(type(()), tuple)
assert_eq(type((1,)), tuple)
assert_eq(type(1), int)

assert_eq(repr([1, 2, 3]), "[1, 2, 3]")
assert_eq(repr((1, 2, 3)), "(1, 2, 3)")
assert_eq(repr({1: 2}), "{1: 2}")

# records should be accessible by attribute
assert_eq({asdf: 1}.asdf, 1)