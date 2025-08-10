import util.assert_eq

g = () =>
    @Env.item("c")

f = () =>
    @Env.item("a"), @Env.item("b"), @g()

assert_eq(f().run({ a: 1, b: 2, c: 3 }), (1, 2, 3))