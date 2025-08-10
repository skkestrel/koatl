import util.assert_eq

v = []
fn = () =>
    v.append(@Result(123))
    v.append(@Err())
    v.append(@Result(321))
fn()

assert_eq(v, [123])

v = []
fn = () =>
    v.append(@Result(123))
    v.append(@Result(222))
    v.append(@Result(321))
fn()

assert_eq(v, [123, 222, 321])

# Result transformer with Result

ResultTResult = Result.T(Result)

v = []
fn = () =>
    v.append(@ResultTResult(Ok(Ok(123))))
    v.append(@ResultTResult(Ok(Err())))
    v.append(@ResultTResult(Ok(Ok(321))))

assert_eq(
    fn().value
    Ok(Err())
)

assert_eq(
    v
    [123]
)

v = []
fn = () =>
    v.append(@ResultTResult(Ok(Ok(123))))
    v.append(@ResultTResult(Err()))
    v.append(@ResultTResult(Ok(Ok(123))))

assert_eq(
    fn().value
    Err()
)

assert_eq(
    v
    [123]
)