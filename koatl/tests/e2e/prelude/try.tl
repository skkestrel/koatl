import util.assert_eq

# precedence
assert_eq(try 1 ?? 2, 1)
assert_eq(try z ?? 2, 2)
assert_eq(try z matches Err(), True)
assert_eq((try z).value matches NameError(), True)
assert_eq(try z ?? try 1 ?? 2, 1)
assert_eq(try z ?? try None ?? 2, None)
assert_eq(try z ?? Err(None) ?? 2, 2)

err_type = NameError
assert_eq(try a except err_type() matches Err(), True)

# handlers
assert_eq(try 1 except ValueError() ?? 2, 1)
assert_eq(try a except NameError() ?? 2, 2)
assert_eq(try a except (ValueError() | NameError()) ?? 2, 2)

try:
    try a except (ValueError() | StopIteration())
    raise
except:
    None

assert_eq(try 1 matches Ok(1), True)

assert_eq((try 5)?.($+1), Ok(6))
assert_eq((try x)?.($+1) matches Err(), True)