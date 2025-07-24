import util.assert_eq

for a, b in [[1, 2]]:
    assert_eq([a, b], [1, 2])

for a, b, {3: c, **d} in [[1, 2, {3: 3, 4: 4, 5: 5}]]:
    assert_eq([a, b, c, d], [1, 2, 3, {4: 4, 5: 5}])

for [
    a
    b
    {
        3: c
        **d
    }
  ] in [[1, 2, {3: 3, 4: 4, 5: 5}]]:
    assert_eq([a, b, c, d], [1, 2, 3, {4: 4, 5: 5}])

assert_eq(([x, y] => x + y)([1, 2]), 3)
assert_eq(([x, {2: y}] => x + y)([1, {2: 2}]), 3)