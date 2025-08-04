import util.assert_eq

# Traverse makes sure that apply and map implementations are correct.

assert_eq(
    [1, 2, 3].traverse(x => @Async.pure(x)).run() # run in a new event loop
    [1, 2, 3]
)

assert_eq(
    [1, 2, 3].traverse(x => x + 1)
    [2, 3, 4]
)

assert_eq(
    [1, 2, 3].traverse(x => None)
    None
)