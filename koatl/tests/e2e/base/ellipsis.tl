import util.assert_eq

# Ellipsis literal
assert_eq(..., Ellipsis)
assert_eq(type(...), type(Ellipsis))

x = ...
assert_eq(x, ...)

# Ellipsis in slice (step-only)
xs = [0, 1, 2, 3, 4, 5]
assert_eq(xs[....2], [0, 2, 4])
assert_eq(xs[1....2], [1, 3, 5])

# Full slice with step
assert_eq(xs[1..5..2], [1, 3])
