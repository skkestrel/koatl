import util.assert_eq

f = Reader.do(() =>
    @"a", @"b"
)

assert_eq(f().run({ a: 1, b: 2 }), (1, 2))