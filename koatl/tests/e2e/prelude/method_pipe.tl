import util.assert_eq

# Basic method pipe: a->f() is equivalent to f(a)
assert_eq("hello"->str.upper(), "HELLO")
assert_eq([3, 1, 2]->sorted(), [1, 2, 3])
assert_eq(5->pow(2), 25)

# Chained method pipes
assert_eq("  hello  "->str.strip()->str.upper(), "HELLO")

# Method pipe partial: a->f is functools.partial(f, a)
add = (x, y) => x + y
add5 = 5->add
assert_eq(add5(3), 8)

# Partial with a qualified name
assert_eq(type(10->pow), type((x => x)(10->pow)))

# Optional method pipe: a?->f() applies f only when a is not None
assert_eq(None?->str(), None)
assert_eq("hello"?->str.upper(), "HELLO")

# Optional method pipe with multiple args
assert_eq(None?->pow(2), None)
assert_eq(3?->pow(2), 9)

# Chained optional method pipes
assert_eq(None?->str()?->str.upper(), None)
assert_eq("hello"?->str.strip()?->str.upper(), "HELLO")
