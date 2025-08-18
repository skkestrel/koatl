import util.assert_eq

assert_eq(
    {a: 0}.keys() | list
    ["a"]
)

assert_eq(
    {(): 0}.keys() | list
    [()]
)

assert_eq(
    {(1): 0}.keys() | list
    [1]
)

assert_eq(
    {(
        let x = 1
        x
    ): 0}.keys() | list
    [1]
)