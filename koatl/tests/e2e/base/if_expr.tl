import util.assert_eq

a = if True:
    1
else:
    2

assert_eq(a, 1)

a = if False:
    2 * 4
else:
    let x = 4 * 8
    x * 2

assert_eq(a, 4 * 8 * 2)

a = if False:
    2 * 4
else:
    if True:
        1
    else:
        0

assert_eq(a, 1)

a = if False:
    2 * 4
else: if True:
        1
else:
    0

assert_eq(a, 1)

a = if True:
    1
  else:
    2

assert_eq(a, 1)