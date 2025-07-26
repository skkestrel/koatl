import util.assert_eq

{ask} = Reader

g = () =>
    @ask("c")

f = () =>
    @ask("a"), @ask("b"), @g()

assert_eq(f().run({ a: 1, b: 2, c: 3 }), (1, 2, 3))