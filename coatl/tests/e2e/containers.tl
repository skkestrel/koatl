import util.assert_eq

assert_eq(type([1, 2, 3]), list)
assert_eq(type((1, 2, 3)), tuple)
assert_eq(type([1: 2]), dict)
assert_eq(type(()), tuple)
assert_eq(type((1,)), tuple)
assert_eq(type(1), int)

assert_eq(repr([1, 2, 3]), "[1, 2, 3]")
assert_eq(repr((1, 2, 3)), "(1, 2, 3)")