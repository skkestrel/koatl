import util.assert_eq

# Test *args with keyword-only default argument
let fn = (*args, default=2) => args + (default,)

assert_eq(fn(1, 3), (1, 3, 2))

# Test regular arg with default and *args
fn = (a, b=2, *args) => (a, b) + args

assert_eq(fn(1, 3, 4, 5), (1, 3, 4, 5))
assert_eq(fn(1), (1, 2))
fn = (a, /, b, *, c=5) => (a, b, c)

assert_eq(fn(1, 2), (1, 2, 5))
assert_eq(fn(1, 2, c=10), (1, 2, 10))
fn = (a, b=3, /, c=4) => (a, b, c)

assert_eq(fn(1), (1, 3, 4))
assert_eq(fn(1, 2), (1, 2, 4))
assert_eq(fn(1, 2, 5), (1, 2, 5))
fn = (a, b, *, c, d=10) => (a, b, c, d)

assert_eq(fn(1, 2, c=3), (1, 2, 3, 10))
assert_eq(fn(1, 2, c=3, d=4), (1, 2, 3, 4))

# Test **kwargs
fn = (a, **kwargs) => (a, kwargs)

assert_eq(fn(1), (1, {}))
assert_eq(fn(1, b=2, c=3), (1, {"b": 2, "c": 3}))
fn = (a, b, /, c, *args, d, e=5, **kwargs) => (a, b, c, args, d, e, kwargs)

assert_eq(fn(1, 2, 3, d=4), (1, 2, 3, (), 4, 5, {}))
assert_eq(fn(1, 2, 3, 4, 5, d=6, e=7, f=8), (1, 2, 3, (4, 5), 6, 7, {"f": 8}))
fn = (a, b, c, /) => (a, b, c)

assert_eq(fn(1, 2, 3), (1, 2, 3))
fn = (*, a, b) => (a, b)

assert_eq(fn(a=1, b=2), (1, 2))