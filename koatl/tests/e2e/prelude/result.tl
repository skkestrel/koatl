import util.assert_eq

v = []
fn = () =>
    v.append(@123)
    v.append(@None)
    v.append(@321)
fn()

assert_eq(v, [123])

v = []
fn = () =>
    v.append(@123)
    v.append(@222)
    v.append(@321)
fn()

assert_eq(v, [123, 222, 321])

v = []
fn = () =>
    v.append(@123)
    v.append(@Ok(None))
    v.append(@ValueError())
    v.append(@321)
fn()

assert_eq(v, [123, None])