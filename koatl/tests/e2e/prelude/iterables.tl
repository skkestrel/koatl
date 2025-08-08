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
    {1: 2}.map([k, v] => [v, k]).record(),
    {2: 1}
)

assert_eq(
    (..3).associate($ + 2)
    {0: 2, 1: 3, 2: 4}
)

assert_eq(
    (..6).group_by($ < 2)
    {False: [2, 3, 4, 5], True: [0, 1]}
)