import util.assert_eq

assert_eq(type(try x), NameError)
assert_eq(type(try int(1)[1]), TypeError)
assert_eq(type(try [0][5]), IndexError)
assert_eq(try 5, 5)