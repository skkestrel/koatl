import util.assert_eq

# precedence
assert_eq(try 1 ?? 2, 1)
assert_eq(try z ?? 2, 2)
assert_eq(type(try z), NameError)
assert_eq(try z ?? try 1 ?? 2, 1)
assert_eq(try z ?? try None ?? 2, 2)

# handlers
assert_eq(try 1 except ValueError() ?? 2, 1)
assert_eq(try a except NameError() ?? 2, 2)
assert_eq(try a except (ValueError() | NameError()) ?? 2, 2)

try:
    try a except (ValueError() | StopIteration())
    assert False
except:
    None