# Koatl Quick Reference

Koatl is a functional-first language that transpiles to Python. For full details, see [CONTEXT_LANGUAGE.md](CONTEXT_LANGUAGE.md).

---

## Key Differences from Python

| Feature               | Python                     | Koatl                                                                  |
| --------------------- | -------------------------- | ---------------------------------------------------------------------- |
| Function definition   | `def f(x): return x+1`     | `f = x => x + 1`                                                       |
| Slice syntax          | `[1:5]`, `[::2]`           | `[1..5]`, `[....2]` (use `..`). Slices are also now iterable.          |
| Block-scoped variable | (no equivalent)            | `let x = 1`                                                            |
| Import from           | `from a.b import c, d`     | `import a.b.(c, d)`                                                    |
| Decorator             | `@decorator; def ...`      | `decorator! () => ...`                                                 |
| Pipe                  | (no equivalent)            | `x \|> f` — passes x into f (last arg)                                 |
| Method pipe           | (no equivalent)            | `x->f(args)` = `f(x, args)`; `x->f` = `partial(f, x)`                  |
| Optional method pipe  | (no equivalent)            | `x?->f(args)` — maps over `Ok`/non-`None`, passes through `Err`/`None` |
| Coalesce              | (no equivalent)            | `x ?? default`                                                         |
| Ternary               | `Y if X else Z`            | `if X then Y else Z`                                                   |
| List comprehension    | `[f(x) for x in xs]`       | `xs.iter.map(f).list()`                                                |
| `except` clauses      | `except TypeError as e:`   | `except TypeError() =>` (match arm)                                    |
| `with` binding        | `with f = open(...) as f:` | `with f = open(...):`                                                  |

---

## Functions

```koatl
let add = (a, b) => a + b
let greet = (name, greeting="Hello") => f"{greeting}, {name}!"

# Multi-line body — last expression is the return value
let fib = n =>
    if n < 2 then 1
    else fib(n-1) + fib(n-2)

# Pattern-matched arguments
let head = [first, *_] => first
let sum_pair = [a, b] => a + b
```

---

## Variable Declarations

```koatl
let x = 1       # Block-scoped — does NOT leak out of if/while/for
x = 2           # Unscoped assignment (Python-style)
const y = 3     # Block-scoped constant (convention only)
export z = 4    # Module export (added to __all__)
global g = 5    # Global scope
```

---

## Pipes & Placeholder

```koatl
# |> passes left side as last (or sole) argument
data |> transform |> save_to(db, $, format="json")

# -> passes left side as first argument
x->f(a, b)          # f(x, a, b)
x->f.g(a, b)        # f.g(x, a, b)
x->(expr)(a, b)     # expr(x, a, b)
x->f                # partial(f, x) — no parens = partial application

# ?-> optional: maps over Ok/non-None, passes through Err/None
check open(path)?->parse()

# .() for inline lambdas where -> would be awkward
results.(x => x.value * scale)

# $ — anonymous function from surrounding expression
list.map($ * 2)     # list.map(x => x * 2)
f(a, $, c)          # x => f(a, x, c)
```

---

## Ranges & Slices

```koatl
..10            # range(0, 10)
1..10           # range(1, 10)
1..10..2        # range(1, 10, 2)
arr[..3]        # arr[:3]
arr[1..4]       # arr[1:4]
arr[..5..2]     # arr[:5:2]

for i in ..10: print(i)
(..100).iter.filter($ % 2 == 0).list()
```

---

## F-Strings

`:` is the format delimiter (same as Python), only at bracket depth 0:

```koatl
f"{pi:.2f}"         # "3.14"
f"{num:05d}"        # "00042"
f"{(if ok: x):.2f}" # : inside () is NOT a format delimiter
```

---

## Pattern Matching

```koatl
result = match x:
    0 => "zero"
    1 | 2 => "one or two"
    [a, b] => f"pair: {a}, {b}"
    [a, *rest] => f"head {a}, tail {rest}"
    {name, age} => f"{name} is {age}"   # shorthand for {name: name, age: age}
    n if n > 0 => "positive"
    _ => "default"

# . prefix matches the value of a variable (not capture)
y = 42
match x:
    .y => "matched y"
    y => "captured as y"

# matches — capture-free boolean test
x matches [_, _]
x not matches None
```

---

## if let / while let

```koatl
if let [a, b] = some_list:
    print(a, b)     # a and b scoped to block

# if not let — body must diverge; captures leak out (like Swift guard let)
if not let Ok(value) = result:
    return
use(value)

while let ("Some", val) = data[idx]:
    process(val)
    idx += 1
```

---

## Records

```koatl
person = {name: "Alice", age: 30}
person.name         # "Alice"
person["age"]       # 30
{**person, age: 31} # spread / update

# Computed key
key = "id"
{(key): 123}        # {"id": 123}

# Methods and properties
counter = {
    value: 0
    inc: Record.method! self => {**self, value: self.value + 1}
    val: Record.property! self => self.value
}
```

---

## Classes

```koatl
Animal = class:
    __init__ = (self, name) => self.name = name
    speak = self => f"{self.name}!"
    label = property! self => f"Animal({self.name})"
    create = staticmethod! name => Animal(name)

Dog = class(Animal):
    __init__ = (self, name) => super().__init__(name)
    fetch = self => f"{self.name} fetches!"
```

---

## Safe Navigation

```koatl
obj?.prop       # None if obj is None/Err, else obj.prop
obj?[0]         # None if obj is None/Err
obj?(arg)       # None if obj is None/Err
obj.?attr       # None if the attribute doesn't exist (catches AttributeError)
x ?? default    # default if x is None or Err
```

---

## Error Handling

```koatl
# check wraps any expression in Ok/Err
result = check risky()                  # Ok(value) or Err(exception)
result = check expr except ValueError() # only catch ValueError
safe   = check dict[key] ?? fallback    # KeyError → Err → fallback

# try/except uses match-arm syntax
result = try:
    parse(raw)
except ValueError(args=[m]) => f"bad: {m}"
except KeyError() | TypeError() => "key/type error"
finally: cleanup()

# @ — monadic bind (not a decorator — use decorator! for that)
process = () =>
    let x = @get_value()    # Err short-circuits immediately
    let y = @transform(x)
    x + y                   # implicitly Ok(x + y)
```

---

## Iterators

Call `.iter` on any list/dict/range/string to enter the lazy iterator pipeline:

```koatl
[1,2,3].iter.map($ * 2).list()          # [2, 4, 6]
(..10).iter.filter($ % 2 == 0).sum()    # 20
[3,1,4].iter.sorted().list()            # [1, 3, 4]

# Eager overrides on concrete containers (no .iter needed)
[1,2,3].map($ * 2)                      # [2, 4, 6]
{a:1, b:2}.map_values($ * 10)           # {a:10, b:20}
```

Key iterator methods: `.map(f)`, `.filter(f)`, `.flat_map(f)`, `.filter_map()`, `.fold(init, f)`, `.take(n)`, `.skip(n)`, `.zip(other)`, `.enumerate()`, `.sorted(key?)`, `.group_by(f)`, `.unique()`, `.sum()`, `.first()`, `.find(f)`, `.for_each(f)`, `.list()`, `.dict()`, `.record()`

---

## Imports

```koatl
import a.b.c            # from a.b import c
import a.b.(c, d)       # from a.b import c, d
import a.b.*            # from a.b import *
import a.b.c as alias
```

Inside `(...)`, entries are resolved relative to the preceding path. Dotted paths extend the prefix; a `.` prefix pops one level:

```koatl
import a.b.(
    c           # from a.b import c
    c.d         # from a.b.c import d
    c.(d, e)    # from a.b.c import d, e
    .x          # from a import x  (one level up)
    .           # from a import b  (import the module itself)
)

export my_value = 42
export import other.(x, y)
```

---

## Useful Patterns

```koatl
# Pipeline
(..100).iter.filter($ % 2 == 0).map($ * $).take(5).list()

# Safe nested access
get_name = (data, id) =>
    if not let Ok(user) = check data[id]: return "unknown"
    check user.profile?.name ?? "unnamed"

# Memoized recursion
let fib = n => if n < 2 then @Memo.pure(n) else memo @fib(n-1) + @fib(n-2)
fib(35).run()
```

---

## Misc

- **Commas optional** in multiline lists, records, and calls
- **Semicolons** separate statements on one line: `let a = 1; a + 2`
- **Block expressions**: `(` at end of line starts a block; final expression is the value
- **Block comments**: `#- ... -#` (nestable)
- **`with` as expression**: `content = with f = open("x"): f.read()`
- **Inline if**: `if cond then x else y` (no `:` after `then`)
- **`@` is monadic bind**, not decorator — use `decorator! value` for decoration

---
