# Koatl Language Reference

**Koatl** is a functional-first language that transpiles to Python. It brings expressive functional idioms to Python while maintaining full interoperability: same runtime, same `pip install`, same deployment. For a compact cheat sheet see [language-quickref.md](language-quickref.md).

---

## Functions

Koatl replaces `def` and `lambda` with `=>` for all function definitions. Assigning an arrow function to a name sets `__name__` automatically.

### Forms

```koatl
# Unary (single argument, no parens needed)
let double = x => x * 2
let head   = [first, *_] => first          # pattern-matched argument

# Multi-argument
let add    = (a, b) => a + b
let greet  = (name, greeting="Hello") => f"{greeting}, {name}!"

# Variable / keyword arguments
let f = (a, b, *args, **kwargs) => other(a + 2, *args, **kwargs)
let g = (a, *, kw_only) => a + kw_only    # keyword-only
let h = (a, /) => a                        # positional-only

# Multi-line body — last expression is the return value
let fib = n =>
    if n < 2 then 1
    else fib(n-1) + fib(n-2)

# Inline multi-statement
let f = x => let y = x * 2; y + 1
```

### Pattern-Matched Arguments

Parenthesized function arguments support destructuring (not guards):

```koatl
let sum_pair = ([a, b]) => a + b
let show_rec = ({name, age}) => f"{name} is {age}"
let mixed    = ([x, y], default=10) => x + y + default
```

### Argument Delegation

`delegate` copies argument names and defaults from another function's signature. It must appear after `*` or `*args`:

```koatl
let target = (*, x=10, y=20) => x + y

let f = (a, *, delegate target(x)) => (a, x)
f(1)         # (1, 10)
f(1, x=5)    # (1, 5)

# Alias: expose under a different name
let h = (*, delegate target(x as local_x)) => local_x

# Override default
let j = (*, delegate target(x=42)) => x

# **kwargs spread: remaining target args collected into dict
let k = (*, delegate target(x, **kw)) => (x, kw)
k()          # (1, {"y": 20})

# Multiple delegates from different targets
let n = (*, delegate ta(p, q), delegate tb(r, s)) => (p, q, r, s)
```

`delegate` is a contextual keyword — it can still be used as an identifier outside argument lists.

---

## Variables & Scope

```koatl
let x = 1       # Block-scoped mutable variable
const y = 2     # Block-scoped constant (convention only, not enforced)
global z = 3    # Global scope
export a = 4    # Module export (added to __all__)
x = 5           # Unscoped assignment (Python-style)
```

**`let` creates proper block scope.** A binding later in a scope cannot affect earlier statements — `nonlocal` is never needed:

```koatl
let a = 1
if True:
    let a = 2
    print(a)    # 2
print(a)        # 1  (not clobbered)

outer = () =>
    let count = 0
    () =>
        count += 1  # no nonlocal needed
        count
```

### Pattern-Based Assignment

```koatl
let [x, y]            = [1, 2]
let {name, age}       = user          # shorthand for {name: name, age: age}
let {name: n, age: a} = user          # bind to different names
const {x, y}          = point
```

### Special Identifiers

**`__locals__`** — dict of Koatl variable names → values in the current scope:

```koatl
(arg1, arg2) =>
    let local = 100
    __locals__   # {local: 100, arg1: ..., arg2: ...}
```

**`__captures__`** — dict of variables captured from outer scopes (excludes globals and current-scope variables):

```koatl
outer = () =>
    let x = 1
    inner = () =>
        __captures__  # {x: 1}
    inner()
```

---

## Pipes & Placeholders & Holes

### `|>` Pipe

`x |> f` means `f(x)`. Passes the left side as the last (or sole) argument:

```koatl
data
    |> do_something
    |> transform
    |> save_to(db, ?, format="json")
```

### `->` Method Pipe

`x->f(args)` means `f(x, args)` — inserts `x` as the **first** argument:

| Form                 | Meaning         |
| -------------------- | --------------- |
| `x->f(args)`         | `f(x, args)`    |
| `x->f.g(args)`       | `f.g(x, args)`  |
| `x->(expr)(args)`    | `expr(x, args)` |
| `x->f` _(no parens)_ | `partial(f, x)` |

```koatl
data->process(opts)          # process(data, opts)
data->clean()->transform()   # transform(clean(data))
x->f                         # partial(f, x)
```

### `?->` Optional Method Pipe

Maps over `Ok`/non-`None`, passes through `Err`/`None`:

```koatl
check open(path)?->parse()       # Ok(parse(file)) or Err
result?->transform(opts)         # skips if result is Err/None
```

### `.()` Scoped Call

Prefer `->` for named functions. `.()` is useful for inline lambdas:

```koatl
results.(x => x.value * scale)
```

### `$` Placeholder

`$` constructs a lambda from its surrounding expression, up to the nearest function call. A bare `$` in an argument position passes the identity function `x => x`.

```koatl
f(a, $ * 2, c)          # f(a, x => x * 2, c)
f(a, $, c)              # f(a, x => x, c)     — bare $ = identity
list.map($ * 2)         # list.map(x => x * 2)
list.filter($ > 5)      # list.filter(x => x > 5)
($.name.upper())        # x => x.name.upper()
```

`$` propagates out through list/tuple literals but not call arguments:

```koatl
[$, 2, 3]       # x => [x, 2, 3]
```

### `?` Hole (partial application)

A bare `?` as a call argument creates a lambda with one positional parameter per hole:

```koatl
save_to(db, ?, format="json")   # x => save_to(db, x, format="json")
insert(?, "val", ?)             # (a, b) => insert(a, "val", b)

data |> save_to(db, ?, format="json")   # pipes data into second arg
```

`?` is only allowed bare — `f(?*2)` is a syntax error. Use `$` for expressions.

When in doubt, use an explicit arrow function.

---

## Pattern Matching

Patterns are used in `match`, `if let`, `for`, function arguments, and `except`.

### Pattern Types

**Literal** — match specific values:

```koatl
match x:
    1 => "one"
    "hello" => "greeting"
    True => "truthy"
```

**Capture** — bind to a variable; `_` discards:

```koatl
match x:
    [head, *tail] => (head, tail)
    {name: n} => n
    _ => "ignore"
```

**Value** — match against an existing variable (`.` prefix):

```koatl
y = 42
match x:
    .y => "matched 42"              # tests the value of y
    .module.attr => "matched attr"
    y => "captures any value as y"  # no dot = capture
```

**Sequence** — lists and tuples:

```koatl
match x:
    [] => "empty"
    [a] => "one element"
    [a, b, c] => "three"
    [a, *rest] => "head and rest"
    [*rest, last] => "all but last"
```

**Mapping** — records and dicts:

```koatl
match x:
    {a: v} => v              # extract key a
    {a, b} => (a, b)         # shorthand for a: a, b: b
    {**rest} => rest         # capture remaining fields
```

**Class** — constructor patterns:

```koatl
match x:
    Point(x, y) => ...
    ValueError(args=[m]) => m    # destructure args tuple
```

**Or** — alternatives with `|`:

```koatl
match x:
    1 | 2 | 3 => "small"
    [a, b] | {x: a, y: b} => a + b
```

**As** — bind after matching:

```koatl
match x:
    [a, *rest] as whole => (a, whole)
```

**Guards**:

```koatl
match x:
    [a, b] if a > b => "a larger"
    [a, b] => "b larger or equal"
```

### Match Expressions

Both prefix and postfix syntax are valid. Prefer prefix `match x:`:

```koatl
result = match x:
    {type: "ok", data: d} => d
    {type: "err", msg: m} => raise RuntimeError(m)
    _ => default

# Postfix form (reads well in some expression contexts)
x match:
    0 => "zero"
    _ => "other"
```

### If Let / If Not Let

`if let` enters the block only when the pattern matches; captures are scoped to the block:

```koatl
if let [a, b] = some_list:
    print(a, b)
else:
    print("no match")
```

`if not let` is the inverse — the body **must** diverge (return/raise/break/continue); captures leak to the surrounding scope (like Swift's `guard let`):

```koatl
if not let Ok(value) = result:
    return default
use(value)   # safe — we returned otherwise
```

### While Let

Loops while the pattern continues to match:

```koatl
idx = 0
while let ("Some", val) = data[idx]:
    process(val)
    idx += 1
```

### `matches` Operator

Capture-free boolean test — use `if let` when you need captures:

```koatl
x matches [_, _]              # True if x is a 2-element list
x not matches None
response.status matches 200 | 201
```

---

## Control Flow

### If / Elif / Else

`if` is an expression — it returns the value of whichever branch runs:

```koatl
# Block form
x = if condition:
    value_a
elif other:
    value_b
else:
    value_c

# Inline: use `then` (single expression only, no `:` after `then`)
x = if condition then value_a else value_b
```

### For Loops

`for` supports full pattern matching in the loop variable:

```koatl
for x in ..10:
    print(x)

for [key, value] in my_dict.items():
    print(key, value)

for {name, age} in users:
    print(f"{name} is {age}")

for (i, x) in items.iter.enumerate():
    print(i, x)
```

### While Loops

```koatl
while x > 0:
    x -= 1
```

### Try / Except / Finally

`except` clauses use **match-arm syntax** (`Pattern =>`), tried in order. Any pattern is valid including OR-patterns, guards, and `as`:

```koatl
result = try:
    parse(raw)
except ValueError(args=[m]) if len(m) > 0 =>
    f"long: {m}"
except ValueError() =>
    "short"
except KeyError | TypeError as e =>
    f"other: {e}"
except TypeError(args=[m]) | RuntimeError(args=[m]) =>
    f"shared handler: {m}"
finally:
    cleanup()
```

- `ExcType(args=[m])` destructures the `args` tuple (standard on all exceptions)
- `as e` binds the raw exception object
- `ExcType(field=var)` destructures named attributes

Pattern matching in `except` also works inside `check`:

```koatl
x = check a except NameError()   # caught; other exceptions propagate
```

### With

`with` binds the `__enter__` return value using pattern syntax and returns the body value:

```koatl
content = with f = open("file.txt"):
    f.read()

# Pattern on the context manager's return value
with [file1, file2] = open_pair():
    file1.read() + file2.read()
```

### Check & Coalescing

`check` wraps the result in `Ok`/`Err` instead of raising:

```koatl
result = check risky()                    # Ok(value) or Err(exception)
result = check expr except ValueError()  # only catch ValueError; others propagate
```

`??` lazily evaluates the right-hand default when the left side is `None`, `Err`, or an uncaught exception:

```koatl
config = check load_config() ?? default_config
port   = check int(env["PORT"]) ?? 8080
name   = check user.profile?.name ?? "unnamed"
```

### Await / Yield

```koatl
result = await async_operation()

gen = x => (
    yield 1
    yield x + 2
)

combined = => yield from other_generator()
```

---

## Attribute Access

### Standard

```koatl
obj.attr          # attribute access
obj.method(arg)   # method call
obj[key]          # subscript
obj::__dict__     # raw attribute — bypasses vget/__getattr__
```

### Safe Navigation (`?.`)

Short-circuits on `None`/`Err` — the object itself is potentially None/Err:

```koatl
obj?.prop            # None if obj is None/Err, else obj.prop
obj?[0]              # None if obj is None/Err
obj?(arg)            # None if obj is None/Err
obj?.a?.b?.c         # chains freely
```

### Maybe Attribute (`.?`)

Tries the access, returns `None` on `AttributeError` — the object exists but the attribute may not:

```koatl
obj.?attr               # None if attr doesn't exist on obj
config.?debug ?? False  # use debug if present, else False
```

**Key distinction**: `obj?.attr` checks whether _obj_ is None/Err; `obj.?attr` checks whether _attr_ exists.

---

## Decorators

`!` applies a single-argument function — the decorator operator:

```koatl
Cls = class:
    method = staticmethod! () => ...
    prop   = property! self => self.value

# a! b  ≡  a(b)
Extension.method(int, "double")! self => self * 2
```

---

## Containers

### Records

Javascript-style dicts with unquoted keys, dot access, optional commas, and method/property support:

```koatl
x = {a: 1, b: "hello"}
x.a == 1
x["a"] == 1

# Computed key (wrap in parens)
key = "id"
{(key): 123}        # {"id": 123}

# Spread / update
updated = {**x, b: "world"}

# Multiline (commas optional)
config = {
    host: "localhost"
    port: 8080
}

# Methods and properties (self is explicit)
counter = {
    value: 0
    inc: Record.method! self => {**self, value: self.value + 1}
    val: Record.property! self => self.value
}
counter.inc().val  # 1
```

### Lists

Python lists; multiline without commas:

```koatl
items = [
    1
    2
    3
]
[1, 2, 3].map($ * 2)     # eager: [2, 4, 6]
[1, 2, 3].filter($ > 1)  # eager: [2, 3]
```

### Tuples

```koatl
x = (1, 2, 3)
a, b = 1, 2

# Multi-line parens are block expressions, not tuples:
x = (       # this is a block — x gets 3
    1
    2
    3
)
x = (1, 2,  # still a tuple when a comma is present on same line
     3, 4)
```

### Sets

```koatl
set([1, 2, 3])   # no literal syntax
```

### Ranges & Slices

`..` replaces `:` for all slice/range syntax:

```koatl
..10          # range(0, 10)
1..10         # range(1, 10)
1..10..2      # range(1, 10, 2)
5..           # open-ended (from 5, as a slice)

arr[..3]      # arr[:3]
arr[2..]      # arr[2:]
arr[1..4]     # arr[1:4]
arr[..5..2]   # arr[:5:2]

# Slices are first-class values
s = ..5
arr[s]

# Ranges are iterable
for i in ..10: print(i)
(..100).iter.filter($ % 2 == 0).list()
```

---

## Iterators & Extensions

### Iterable vs Iterator

- **`Iterable`** — anything with an `.iter` extension property (lists, dicts, ranges, strings, tuples). Provides `.traverse()` and `.debug_iter()`.
- **`Iterator`** — anything with `__next__` (result of `.iter`, or any Python iterator). Provides the full method suite.

`.iter` delegates to `.items()` for dicts, so `for [k, v] in d.iter:` works naturally.

### Eager vs Lazy

Concrete containers have **eager** `map`/`filter` overrides that return their own type:

```koatl
[1, 2, 3].map($ * 2)              # [2, 4, 6]  — list in, list out
set([1, 2, 3]).map($ * 2)         # {2, 4, 6}
{a: 1, b: 2}.map_values($ * 10)   # {a: 10, b: 20}
{a: 1, b: 2}.map_keys($.upper())  # {A: 1, B: 2}
{a: 1, b: 2}.filter_values($ > 1) # {b: 2}
```

For everything else — aggregations, slicing, chaining — call `.iter` first:

```koatl
[1, 2, 3].iter.sum()                              # 6
[3, 1, 4].iter.sorted().list()                    # [1, 3, 4]
(..100).iter.filter($ % 7 == 0).take(5).list()   # [0, 7, 14, 21, 28]
```

### Iterator Method Reference

**Transformations**: `.map(f)`, `.filter(f)`, `.flat_map(f?)`, `.filter_map(f?)`, `.enumerate(start=0)`, `.zip(*others)`, `.chain(*others)`, `.product(*others)`, `.cycle()`, `.unique()`, `.reversed()`, `.sorted(key?, reverse?)`

**Slicing**: `.take(n)`, `.skip(n)`, `.take_while(f)`, `.skip_while(f)`

**Aggregations** (consume iterator): `.fold(init, f)`, `.sum()`, `.mean()`, `.min(key?)`, `.max(key?)`, `.tally(f?)`, `.join(sep="")`, `.all(f)`, `.any(f)`, `.find(f)`, `.first()`, `.last(f)`, `.at(i)`, `.for_each(f)`

**Collectors**: `.list()`, `.set()`, `.tuple()`, `.dict()`, `.record()`, `.associate(f)`, `.group_by(f)`, `.count_by(f?)`

### Extension Attributes & Traits

Extension attributes add methods and properties to any type — including builtins — without monkey-patching, via a virtual dispatch table (`vget`):

```koatl
# Add a method to int
Extension.method(int, "double")! self => self * 2
(5).double()   # 10

# Add a property
Extension.property(list, "len")! self => len(self)
[1, 2, 3].len  # 3

# Trait: add methods to any type satisfying requirements
export Iterable = Extension.trait! class(Trait):
    iter = Trait.abstract! self => ()            # requirement
    traverse = (self, f) => self.iter.for_each(f)  # available on anything with .iter

export Iterator = Extension.trait! class(Trait):
    __next__ = Trait.abstract! self => ()
    map    = (self, f) => map(f, self)
    filter = (self, f) => filter(f, self)
    list   = self => list(self)
    # ... fold, sum, take, skip, sorted, group_by, etc.
```

**Virtual dispatch order**: `__getattribute__` → type vtable → trait vtable → `AttributeError`.

### Debugging Pipelines

`.debug_iter()` wraps any iterable and tracks values through the pipeline, rendering an ASCII grid with `.show()`:

```koatl
[1, 2, 3, 4, 5]
    .debug_iter()
    .map($ * 2)
    .filter($ > 4)
    .show()
# ┌───┬────────┬──────┬────────┐
# │   │ source │  map │ filter │
# ├───┼────────┼──────┼────────┤
# │ 0 │   1    │   2  │ [skip] │
# │ 2 │   3    │   6  │   6    │
```

---

## Modules

### Imports

Koatl unifies `import` and `from ... import` into dot-separated paths:

```koatl
import a.b.c            # from a.b import c
import a.b.(c, d)       # from a.b import c, d
import a.b.*            # from a.b import *
import a.b.c as alias
```

Inside a `(...)` group, entries are resolved relative to the preceding path:

- Plain name `d` → import from current prefix
- Dotted path `f.g.(i, j)` → extend prefix, then import
- `.x` → pop one level: `.x` inside `a.b.(...)` → `from a import x`
- Bare `.` → import the prefix module itself: `.` inside `a.b.(...)` → `from a import b`

```koatl
import a.b.(
    c           # from a.b import c
    f.g.(i, j)  # from a.b.f.g import i, j
    .x          # from a import x
    .           # from a import b
)
```

**Relative imports**:

```koatl
import .local           # from . import local
import ..parent         # from .. import parent
import ...grandparent   # from ... import grandparent
```

**With aliasing**:

```koatl
import module.(a as x, b as y)
```

### Exports

```koatl
export my_value = 42
export import other.(x, y)   # re-export
# anything without export is module-private
```

---

## Monads

Koatl uses `@` as a **monadic bind** operator (not a decorator — use `decorator! value` for decoration). Inside a function, `@expr` yields-and-binds the monadic value, producing flat sequential-looking code for nested operations.

> `@` requires `bind_once` (called at most once). This supports deterministic monads: Result, Memo, Async, Env.

### Result

`Ok` / `Err`, with automatic wrapping via `Result(value)`:

```koatl
Result(1)           # Ok(1)
Result(None)        # Err(None)
Result(ValueError()) # Err(ValueError())
```

`@` unwraps `Ok` and short-circuits on `Err` (like `?` in Rust):

```koatl
process = () =>
    let x = @get_value()       # returns Err immediately if Err
    let y = @transform(x)
    x + y                      # implicitly Ok(x + y)

process()   # Ok(...) or Err(...)
```

`Result` provides a default `bind_once` for **all** types, so `@` also works on bare non-Result values.

### Memo

Memoized computation with the `memo` keyword. Dependencies are automatically inferred from directly captured variables:

```koatl
let fib = n =>
    if n < 2 then @Memo.pure(1)
    else memo @fib(n-1) + @fib(n-2)

fib(200).run()   # or fib(200).run(Memo.Cache())
```

`async memo` for async memoization.

### Async

```koatl
f = () =>
    print("start")
    @Async.sleep(1)
    print("done")

f().run()   # creates event loop; or `await f()` in async context
```

### Env

Provides access to an external context object without threading it through every function:

```koatl
g = () =>
    @Env.item("key")

f = () =>
    let a = @Env.item("first")
    let b = @g()
    a + b

f().run(context_dict)
```

---

## Syntax Reference

### Block Comments

Nestable `#- ... -#` block comments:

```koatl
x = #- this is a #- nested -# comment -# 2
```

### F-Strings

`:` is the format delimiter, but only at bracket depth 0:

```koatl
f"{pi:.2f}"              # "3.14"
f"{num:05d}"             # "00042"
f"{(if ok: x):.2f}"     # : inside () is NOT a delimiter

# Multi-line block inside {}
f"Result: {
    let a = compute()
    a * 2
}"

rf"path: {value}\n"     # raw f-string — \n is literal
```

### Block Expressions

A `(` at the end of a line starts an expression block; the final expression is the value:

```koatl
x = (
    a = 2
    b = 3
    a + b    # x == 5
)

x = 2 + (
    if True then 2 else 3
)
```

### Semicolons

Separate statements on one line:

```koatl
let a = 1; let b = 2; a + b
x = (let x = 123; x)
```

### Indentation Rules

- Opening `(` at end of a line starts a new indented block
- Optional commas in multiline lists, records, and function calls
- `[` and `{` also open blocks when at end of line

```koatl
my_list = [
    1
    2 + 2
    3
]

function_call(
    arg1
    arg2
    kw=value
)
```

---

## Operators Reference

### Precedence (highest to lowest for binary operators)

| Level | Operators                                                        | Associativity |
| ----- | ---------------------------------------------------------------- | ------------- |
| 0     | `**`                                                             | Right         |
| 1     | `*`, `/`, `//`, `%`, `@`                                         | Left          |
| 2     | `+`, `-`                                                         | Left          |
| 3     | `<<`, `>>`                                                       | Left          |
| 4     | `&`                                                              | Left          |
| 5     | `^`                                                              | Left          |
| 6     | `\|`                                                             | Left          |
| 7     | `<`, `>`, `<=`, `>=`, `==`, `!=`, `is`, `is not`, `in`, `not in` | Left          |
| 8     | `and`                                                            | Left          |
| 9     | `or`                                                             | Left          |
| 10    | `??`                                                             | Left          |
| 11    | `\|>`                                                            | Left (lowest) |

**Above binary precedence** (processed in order): `matches` / `not matches`, `memo`, `with`, `match:`, `try:...except:`, `await` / `yield`, `check`

### Postfix Operators

| Operator     | Meaning                                            | Example                     |
| ------------ | -------------------------------------------------- | --------------------------- |
| `()`         | Function call                                      | `f(a, b)`                   |
| `?()`        | Safe call (short-circuits on None/Err)             | `x?(a, b)`                  |
| `[]`         | Subscript                                          | `x[0]`, `x[1..3]`           |
| `?[]`        | Safe subscript                                     | `x?[0]`                     |
| `.attr`      | Attribute access                                   | `x.prop`                    |
| `?.attr`     | Safe attribute (short-circuits on None/Err)        | `x?.prop`                   |
| `.?attr`     | Maybe attribute (returns `None` on AttributeError) | `x.?prop`                   |
| `::attr`     | Raw attribute (bypasses `vget`)                    | `x::__dict__`               |
| `?::attr`    | Safe raw attribute                                 | `x?::__dict__`              |
| `.()`        | Scoped call; use for inline lambdas                | `x.(v => v * 2)`            |
| `?.()`       | Safe scoped call                                   | `x?.(v => v * 2)`           |
| `->f(args)`  | Method pipe (inserts as first arg)                 | `x->f(a, b)` = `f(x, a, b)` |
| `?->f(args)` | Optional method pipe                               | `x?->f(a)`                  |
| `!`          | Decorator / one-argument call                      | `decorator! value`          |

### Unary Prefix Operators

| Operator | Meaning          |
| -------- | ---------------- |
| `+`      | Unary plus       |
| `-`      | Unary minus      |
| `~`      | Bitwise NOT      |
| `@`      | Monadic bind     |
| `not`    | Logical negation |

### Comparison Operators

| Koatl    | Python   | Meaning        |
| -------- | -------- | -------------- |
| `==`     | `==`     | Equality       |
| `!=`     | `!=`     | Inequality     |
| `is`     | `is`     | Identity       |
| `is not` | `is not` | Non-identity   |
| `in`     | `in`     | Membership     |
| `not in` | `not in` | Non-membership |

### Assignment Operators

`=`, `+=`, `-=`, `*=`, `/=`, `//=`, `%=`, `**=`, `|=`, `??=`, `@=`

---

## Execution Modes

### Script

```bash
koatl script.tl
```

### Module (import from Python)

```python
import koatl.runtime   # enables .tl imports
import my_script       # imports my_script.tl
```

### Jupyter / IPython

```python
%load_ext koatl.notebook
```

Or with `koatl-kernel`:

```bash
pip install koatl-kernel
jupyter notebook   # select Koatl kernel
```

---

## Common Patterns

### Data Pipeline

```koatl
orders
    .iter
    .filter($.status == "pending")
    .group_by($.customer_id)
    .iter
    .map([id, items] => {id, total: items.iter.map($.price).sum()})
    .filter($.total > 100)
    .sorted($.total, reverse=True)[..10]
```

### Safe Nested Access

```koatl
get_name = (data, id) =>
    if not let Ok(user) = check data[id]: return "unknown"
    if not let Ok(profile) = check user.profile: return "no profile"
    check profile.name ?? "unnamed"
```

### Memoized Recursion

```koatl
let fib = n =>
    if n < 2 then @Memo.pure(n)
    else memo @fib(n-1) + @fib(n-2)

fib(200).run()
```

### Error-Chaining with @

```koatl
fetch_and_process = url =>
    let raw  = @Async.from_sync(() => check requests.get(url))
    let data = @raw.json()->Result()
    transform(data)

match fetch_and_process("https://example.com/api")():
    Ok(result) => use(result)
    Err(e)     => log(e)
```

### Record with Methods

```koatl
create_counter = (initial=0) => {
    value: initial
    inc:   Record.method! self => {**self, value: self.value + 1}
    add:   Record.method! (self, n) => {**self, value: self.value + n}
    get:   Record.property! self => self.value
}

create_counter(10).inc().add(5).get   # 16
```
