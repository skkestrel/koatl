import util.assert_eq

# precedence
assert_eq(check 1 ?? 2, 1)
assert_eq(check z ?? 2, 2)
assert_eq(check z matches Err(NameError()), True)
assert_eq(check z ?? check 1 ?? 2, 1)
assert_eq(check z ?? check None ?? 2, None)
assert_eq(check z ?? Err(None) ?? 2, 2)

err_type = NameError
assert_eq(check a except err_type() matches Err(), True)

# handlers
assert_eq(check 1 except ValueError() ?? 2, 1)
assert_eq(check a except NameError() ?? 2, 2)
assert_eq(check a except (ValueError() | NameError()) ?? 2, 2)

try:
    check a except (ValueError() | StopIteration())
    raise
except _ =>
    None

assert_eq(check 1 matches Ok(1), True)

assert_eq((check 5)?.($+1), Ok(6))
assert_eq((check x)?.($+1) matches Err(), True)