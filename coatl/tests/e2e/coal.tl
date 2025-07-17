import util.assert_eq

assert_eq(None?(1)?(2), None)
assert_eq(None?[1]?.a, None)
assert_eq(None?.a, None)
assert_eq(None?.(a), None)

obj = (class:
    a = 1
)()

assert_eq((x => x)?(1), 1)
assert_eq([1]?[0], 1)
assert_eq(obj?.a, 1)
assert_eq(1?.($ + 1), 2)

assert_eq(None ?? 1, 1)
assert_eq(int(5) ?? 1, 5)