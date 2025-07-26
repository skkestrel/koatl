import util.assert_eq

v = []
fn = Ok.do(() =>
    v.append(@123)
    v.append(@None)
    v.append(@321)
)
fn()

assert_eq(v, [123])

v = []
fn = Ok.do(() =>
    v.append(@123)
    v.append(@222)
    v.append(@321)
)
fn()

assert_eq(v, [123, 222, 321])