import util.assert_eq

# this should get the special-cased iter
assert_eq([1, 2, 3]!iter | list, [1, 2, 3])

# !iter should grab items whenever possible
assert_eq([1: 2, 3: 4]!iter | list, [(1, 2), (3, 4)])

# !iter should work on slices
assert_eq((..5)!iter | list, [0, 1, 2, 3, 4])

# ! should grab attrs if they exist
import types.SimpleNamespace as sn
x = sn(a=1)
assert_eq(x!a, 1)