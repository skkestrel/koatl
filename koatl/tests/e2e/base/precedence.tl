
import util.assert_eq

x = x => x + 1
assert_eq(x(1), 2)

x = x => x + 1 * 2
assert_eq(x(1), 3)

x = x =>
    x
        | x => x * 2
        | x => x - 3
        | x => x + 1 * 2
assert_eq(x(1), 1)


a = 1
m = 2
e = 3
x = a + m * e**(a + a)**e * m + a
assert_eq(x, ((a+((m*(e**((a+a)**e)))*m))+a))