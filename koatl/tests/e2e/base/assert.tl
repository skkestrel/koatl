import util.assert_eq

assert True
assert 1 == 1
assert 2 + 2 == 4, "math is broken"

x = 42
assert x == 42, f"expected 42, got {x}"

assert_eq(1, 1)
