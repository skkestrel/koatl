import ast
import koatl
import util.assert_contains

# Test: import a.b should emit "from a import b"
parsed = koatl.transpile("import a.b", mode="no_prelude")
assert_contains(ast.unparse(parsed), "from a import b")

# Test: import a.b.(.) should emit "import a.b as b"
parsed = koatl.transpile("import a.b.(.)", mode="no_prelude")
assert_contains(ast.unparse(parsed), "import a.b as b")

# Test: import a.b.c should emit "from a.b import c"
parsed = koatl.transpile("import a.b.c", mode="no_prelude")
assert_contains(ast.unparse(parsed), "from a.b import c")