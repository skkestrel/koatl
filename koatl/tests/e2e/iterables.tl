import util.assert_eq

assert_eq(
    (..10).sum(),
    45
)

assert_eq(
    [1, 2, 3].map($ + 4).list(),
    [5, 6, 7]
)

assert_eq(
    [1, 2, 3].filter($ > 1).list(),
    [2, 3]
)

assert_eq(
    [1: 2].map([k, v] => [v, k]).record(),
    [2: 1]
)