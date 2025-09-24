import util.assert_eq

none = None
assert_eq(none?(1)?(2), None)
assert_eq(none?[1]?.a, None)
assert_eq(none?.a, None)
assert_eq(none?. (a), None)

obj = (class:
    a = 1
)()

assert_eq((x => x)?(1), 1)
assert_eq([1]?[0], 1)
assert_eq(obj?.a, 1)
assert_eq(1?. ($ + 1), 2)

assert_eq(None ?? 1, 1)
assert_eq(int(5) ?? 1, 5)

counts = 0
f = () =>
    counts = counts + 1

f() ?? 1

# should only call once
assert_eq(counts, 1)

assert_eq((1).?f, None)
assert_eq(None.?f, None)