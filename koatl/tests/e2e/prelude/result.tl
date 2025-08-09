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


# Result fusion

v = []
fn = () =>
    v.append(@Ok(Ok(123)).fuse())
    v.append(@Ok(Err()).fuse())
    v.append(@Ok(Ok(321)).fuse())

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
    v.append(@Ok(Ok(123)).fuse())
    v.append(@Err().fuse())
    v.append(@Ok(Ok(123)).fuse())

assert_eq(
    fn()
    Err()
)

assert_eq(
    v
    [123]
)

# Await fusion

v = []
fn = () =>
    v.append(@Async(Async.from_sync(() => Ok(123))).fuse())
    v.append(@Async(Async.from_sync(() => Err())).fuse())
    v.append(@Async(Async.from_sync(() => Ok(321))).fuse())

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
    v.append(@Async(Async.from_sync(() => Ok(123))).fuse())
    v.append(@Async(Async.from_sync(() => Ok(321))).fuse())
    v.append(@Async(Async.from_sync(() => Err())).fuse())

assert_eq(
    fn().run()
    Err()
)

assert_eq(
    v
    [123, 321]
)