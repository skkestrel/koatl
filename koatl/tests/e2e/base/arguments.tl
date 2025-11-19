import util.assert_eq

let fn = (*args, default=2) => args + (default,)

assert_eq(fn(1, 3), (1, 3, 2))