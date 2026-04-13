import util.assert_eq

x = 0
for i in [1, 2, 3]:
    if i == 2:
        pass
    else:
        x = x + i

assert_eq(x, 4)

Noop = class:
    pass

assert_eq(type(Noop()).__name__, "Noop")
