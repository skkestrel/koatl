import util.assert_eq

import os
import os.(error, environ, ctermid as test)

assert_eq(error, os.error)
assert_eq(environ, os.environ)
assert_eq(test, os.ctermid)

import os.*

assert_eq(getcwd, os.getcwd)

import util.module0

assert_eq(set(module0.__all__), set(["z"]))
assert_eq(module0.z, 3)

import util.module1

assert_eq(set(module1.__all__), set(["a", "z"]))
assert_eq(module1.a, 1)
assert_eq(module1.z, 3)

import util.module2

assert_eq(set(module2.__all__), set(["b"]))
assert_eq(module2.b, 3)