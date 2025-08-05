import util.assert_eq

assert_eq(
    True and False
    False
)

# No short-circuiting here.

flag = 0
assert_eq(
    True and (
        flag = 1
        True
    )
    True
)
assert_eq(flag, 1)

# This needs to short-circuit.

flag = 0
assert_eq(
    False and (
        flag = 1
        True
    )
    False
)
assert_eq(flag, 0)

# Same with or

flag = 0
assert_eq(
    True or (
        flag = 1
        True
    )
    True
)
assert_eq(flag, 0)