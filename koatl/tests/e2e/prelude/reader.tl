import util.assert_eq

g = Reader.do(() =>
    @"c"
)

f = Reader.do(() =>
    @"a", @"b", @g()
)

assert_eq(f().run({ a: 1, b: 2, c: 3 }), (1, 2, 3))