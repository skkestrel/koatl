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


# try exprs

assert_eq(type(try x), NameError)
assert_eq(type(try int(1)[1]), TypeError)
assert_eq(type(try [0][5]), IndexError)
assert_eq(try 5, 5)


assert_eq((try 5)?.($+1), 6)
assert_eq(type((try x)?.($+1)), NameError)