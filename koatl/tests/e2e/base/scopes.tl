import util.assert_eq

test = 1
if True:
    test = 2
    assert_eq(test, 2)

    let test = 3
    assert_eq(test, 3)

    test = 4
    assert_eq(test, 4)

assert_eq(test, 2)

f = () =>
    let test = 5
    assert_eq(test, 5)

f()
assert_eq(test, 2)

f = () =>
    test = 5
    let g = () =>
        test = 6
    g()
    assert_eq(test, 6)

f()


f = () =>
    test = 5
    let g = () =>
        let test = 6
    g()
    assert_eq(test, 5)

f()

test = 0

f = test =>
    test = 1
    assert_eq(test, 1)

f(1)
assert_eq(test, 0)