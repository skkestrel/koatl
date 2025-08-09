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


# Result transformer with Result

v = []
fn = () =>
    v.append(@Result.T(Ok(Ok(123))))
    v.append(@Result.T(Ok(Err())))
    v.append(@Result.T(Ok(Ok(321))))

assert_eq(
    fn()
    Ok(Err())
)

assert_eq(
    v
    [123]
)

v = []
fn = () =>
    v.append(@Result.T(Ok(Ok(123))))
    v.append(@Result.T(Err()))
    v.append(@Result.T(Ok(Ok(123))))

assert_eq(
    fn()
    Err()
)

assert_eq(
    v
    [123]
)

# Result transformer with Async

v = []
fn = () =>
    v.append(@Result.T(Async(Async.from_sync(() => Ok(123)))))
    v.append(@Result.T(Async(Async.from_sync(() => Err()))))
    v.append(@Result.T(Async(Async.from_sync(() => Ok(321)))))

assert_eq(
    fn().run()
    Err()
)

assert_eq(
    v
    [123]
)


v = []
fn = () =>
    v.append(@Result.T(Async(Async.from_sync(() => Ok(123)))))
    v.append(@Result.T(Async(Async.from_sync(() => Ok(321)))))
    v.append(@Result.T(Async(Async.from_sync(() => Err()))))

assert_eq(
    fn().run()
    Err()
)

assert_eq(
    v
    [123, 321]
)