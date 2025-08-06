import util.assert_eq

#-
Note: many of the tests in this file are inside temporary functions;
this is because the global scope is special in Python.
-#



# Scope variables shouldn't escape

test = 1
if True:
    test = 2
    assert_eq(test, 2)
    let test = 3
    assert_eq(test, 3)
    test = 4
    assert_eq(test, 4)
assert_eq(test, 2)

# Nested scopes should identify captures properly

test = 1
f = () =>
    test = 2
    let g = () =>
        test = 3
    g()
    assert_eq(test, 3)
f()
assert_eq(test, 3)

# ...but should also not leak variables

test = 1
f = () =>
    test = 2
    let g = () =>
        let test = 3
    g()
    assert_eq(test, 2)
f()
assert_eq(test, 2)

# Arguments should shadow all other variables

test = 0
f = test =>
    test = 1
    assert_eq(test, 1)
f(1)
assert_eq(test, 0)

# Assignment lhs should be lifted into function body scope

f = () =>
    let g = 1
    let g = () => g = 2
    g()
    assert_eq(g, 2)
f()

# Recursion should work

a = () =>
    let fib = n => n < 2 then 1 else fib(n - 1) + fib(n - 2)
    assert_eq(fib(5), 8)
a()

# Classes should work

f = () =>
    let A = class:
        B = 2

        f = (self, x=B) =>
            x

    assert_eq(A.B, 2)
    assert_eq(A().f(), 2)
f()

f = () =>
    let A = class:
        f = self =>
            # this should now resolve to the class itself
            A = 2

    A().f()
    assert_eq(A, 2)
f()

# Shadowing should work
f = () =>
    let x = 1
    let x = x + 1
    assert_eq(x, 2)

    # but recursion should also work
    let x = 3
    let x = () => x.__name__
    assert_eq(type(x()), str)
f()