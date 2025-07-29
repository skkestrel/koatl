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