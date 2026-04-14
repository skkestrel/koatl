import util.assert_eq

# basic usage

call_with_five = f => f(5)

assert_eq(call_with_five($ + 1), 6)

# $ as bare call arg passes the identity function
assert_eq(call_with_five($), 5)

assert_eq(callable([1, 2, $]), True)


# weird cases

assert_eq((x => x)($ + 1)(1), 2)

assert_eq(1
    |> $ + 1
    |> $ * 2
    |> $ * 5, 20)

f = $ + 1

assert_eq(f(2), 3)

assert_eq(3 |> [$, 2, 3], [3, 2, 3])
f = [$, 2, 3]
assert_eq(f(3), [3, 2, 3])

# placeholder guard regression

assert_eq({}.($.__or__({1: 2}).__or__({3: 4})), {1: 2, 3: 4})

# placeholder inside ternary

assert_eq(list(map(if $ > 1 then $ * 10 else 0, [1, 2, 3])), [0, 20, 30])

# ? hole: partial application sugar

insert = (a, b, c) => [a, b, c]

# positional hole
assert_eq(insert(1, ?, 3)(2), [1, 2, 3])

# multiple holes
assert_eq(insert(?, 2, ?)(1, 3), [1, 2, 3])

# hole as sole arg
f3 = insert(10, ?, 30)
assert_eq(f3(20), [10, 20, 30])

# in a pipe
result = 5 |> insert(1, ?, 3)
assert_eq(result, [1, 5, 3])

# kwarg hole
kw_fn = (a, b=0, c=0) => [a, b, c]
assert_eq(kw_fn(1, b=?, c=5)(99), [1, 99, 5])

# spread hole: *? takes an iterable and spreads it
concat = (*args) => list(args)
assert_eq(concat(*?)([1, 2, 3]), [1, 2, 3])

# kwarg spread hole: **?
kw_merge = (**kw) => kw
d = {"a": 1, "b": 2}
assert_eq(kw_merge(**?)(d), {"a": 1, "b": 2})