import util.assert_eq

import os
import os.(error, environ, ctermid as test)

assert_eq(error, os.error)
assert_eq(environ, os.environ)
assert_eq(test, os.ctermid)

import os.*

assert_eq(getcwd, os.getcwd)