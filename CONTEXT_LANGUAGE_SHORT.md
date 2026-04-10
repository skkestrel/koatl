# Koatl Quick Reference

Koatl is a functional-first language that transpiles to Python. This is a quick guide to the most useful patterns. For full details, see [CONTEXT_LANGUAGE.md](CONTEXT_LANGUAGE.md).

---

## Key Differences from Python

| Feature                 | Python                    | Koatl                          |
| ----------------------- | ------------------------- | ------------------------------ |
| Function definition     | `def f(x): return x+1`    | `f = x => x + 1`               |
| Slice syntax            | `[1:5]`, `[::2]`          | `[1..5]`, `[....2]` (use `..`) |
| f-string format spec    | `f"{x:.2f}"`              | `f"{x%.2f}"` (use `%`)         |
| Block-scoped variable   | (no equivalent)           | `let x = 1`                    |
| Import from             | `from a.b import c`       | `import a.b.c`                 |
| Identity / non-identity | `is` / `is not`           | `===` / `!==`                  |
| Modulo                  | `x % y`                   | `x %% y`                       |
| Ternary                 | `Y if X else Z`           | `if X then Y else Z`           |
| List comprehension      | `[f(x) for x in xs]`      | `xs.iter.map(f).list()`        |
| Filtered comprehension  | `[x for x in xs if p(x)]` | `xs.iter.filter(p).list()`     |

---

## Functions

```koatl
# Single-arg arrow function
let double = x => x * 2

# Multi-arg
let add = (a, b) => a + b

# With defaults (require parentheses)
let greet = (name, greeting="Hello") => f"{greeting}, {name}!"

# Multi-line body (indented block; last expression is return value)
let fib = n =>
    if n < 2:
        1
    else:
        fib(n-1) + fib(n-2)

# Pattern-matched argument
let head = [first, *_] => first
let pair_sum = [a, b] => a + b

# Recursive: just call by name (no special syntax needed unless in let binding)
let rec_fib = n => if n < 2 then 1 else rec_fib(n-1) + rec_fib(n-2)
```

**Named functions**: assigning an arrow function to a name with `=` or `let` sets `__name__` automatically.

---

## Variable Declarations

```koatl
let x = 1       # Block-scoped; does NOT leak out of if/while/for blocks
x = 2           # Assignment (unscoped — Python-style)
const y = 3     # Block-scoped constant (convention only, not enforced)
export z = 4    # Module export (added to __all__)
global g = 5    # Global scope
```

**`let` provides proper block scoping** — unlike Python, inner `let` bindings do not affect the outer scope:

```koatl
let a = 1
if True:
    let a = 2
    print(a)    # 2
print(a)        # 1  (not affected!)
```

---

## Pipe Operator & Placeholder

```koatl
# | pipes left side as last argument (or single argument)
"hello world" | print              # print("hello world")
data | do_something | do_other     # chaining

# $ creates an anonymous function from the surrounding expression
list.map($ * 2)          # list.map(x => x * 2)
list.filter($ > 5)       # list.filter(x => x > 5)
f(a, $, c)               # x => f(a, x, c)
$ + 1 | $ * 2            # pipeable lambdas

# .() — scoped call (higher precedence than .)
(..10).iter.map($ + 1).(filter $ > 5).(list)
```

---

## Ranges & Slices

Use `..` everywhere Python uses `:` for ranges/slices:

```koatl
..10        # range(0, 10)  — iterable
5..         # from 5 to end (as a slice)
1..10       # range from 1 to 10
1..10..2    # range 1 to 10, step 2
..          # unbounded range (e.g. for infinite iteration)

# Slicing
arr[..3]        # first 3 elements  (Python: arr[:3])
arr[2..]        # from index 2      (Python: arr[2:])
arr[1..4]       # index 1 to 3      (Python: arr[1:4])
arr[..5..2]     # every 2nd, first 5 (Python: arr[:5:2])

# Ranges are iterable
for i in ..10:
    print(i)

(..100).iter.filter($ % 2 == 0).list()
```

---

## F-Strings

Use `%` instead of `:` for format specs:

```koatl
f"Hello, {name}!"                  # basic interpolation
f"{pi%.2f}"                        # format spec (Python: f"{pi:.2f}")
f"{num%05d}"                       # zero-padded
f"{num%>5d}"                       # right-aligned, width 5
f"{hex_num%#x}"                    # hex with prefix (0xff)

# Multi-line / block expression inside {}
f"Result: {
    let a = compute()
    a * 2
}"

# Verbatim f-strings (raw, no escape processing)
rf"path: {value}\n"     # \n is literal
```

---

## Pattern Matching

```koatl
# match expression
result = match x:
    0 => "zero"
    1 | 2 => "one or two"
    [a, b] => f"pair: {a}, {b}"
    [a, *rest] => f"head {a}, tail {rest}"
    {name, age} => f"{name} is {age}"       # shorthand for {name: name, age: age}
    {type: "ok", data: d} => d
    _ => "default"

# Guards
match x:
    n if n > 0 => "positive"
    n if n < 0 => "negative"
    _ => "zero"

# Postfix form (also valid)
x match:
    0 => "zero"
    _ => "other"

# Value patterns: use . prefix to match against a variable (not capture)
y = 42
match x:
    .y => "matched 42"          # matches the value of y
    y => "captured as y"        # binds any value to y

# matches operator — boolean test, no capture
x matches [_, _]                # True if x is a 2-element list
x not matches None              # True if x is not None
```

---

## if let / while let

`if let` destructures a value; enters the block only if the pattern matches:

```koatl
if let [a, b] = some_list:
    print(a, b)         # a and b are bound in this scope
else:
    print("no match")

# if not let — block must be Never (return/raise/break); captures leak out
if not let Ok(value) = result:
    return              # or raise, break, continue
use(value)              # value is available here (safe — returned otherwise)

# while let — loop while pattern matches
idx = 0
while let ("Some", val) = data[idx]:
    process(val)
    idx += 1
```

---

## Records (JS-like Dicts)

```koatl
# Create records with {key: value} — keys don't need quotes
person = {name: "Alice", age: 30}

# Access with dot notation or subscript
person.name             # "Alice"
person["age"]           # 30

# Computed and expression keys
key = "id"
obj = {(key): 123}      # {"id": 123}

# Spread / merge
updated = {**person, age: 31}

# Methods on records
counter = {
    value: 0
    increment: Record.method! self => {**self, value: self.value + 1}
    get: Record.method! self => self.value
}

# Multiline (commas optional)
config = {
    host: "localhost"
    port: 8080
    debug: True
}
```

---

## Classes

```koatl
# Basic class
Animal = class:
    __init__ = (self, name, sound) =>
        self.name = name
        self.sound = sound

    speak = self => f"{self.name} says {self.sound}!"

    # staticmethod / property with decorator shorthand
    create = staticmethod! (name, sound) => Animal(name, sound)
    label = property! self => f"Animal({self.name})"

dog = Animal("Rex", "woof")
dog.speak()         # "Rex says woof!"
dog.label           # "Animal(Rex)"

# Inheritance: class(Base) or class(Base1, Base2)
Dog = class(Animal):
    __init__ = (self, name) =>
        super().__init__(name, "woof")

    fetch = self => f"{self.name} fetches the ball!"

# Single-expression class body (e.g. for exceptions)
class MyError(Exception): None

# Inline class (usable as expression)
obj = (class:
    x = 42
)()
obj.x   # 42
```

**`!` is the decorator operator** — `staticmethod! fn` is equivalent to `staticmethod(fn)`.

---

## Safe Navigation

```koatl
obj?.property           # None if obj is None; otherwise obj.property
obj?[0]                 # None if obj is None; otherwise obj[0]
obj?(arg)               # None if obj is None; otherwise obj(arg)
obj?.a?.b?.c            # chains safely

obj.?attr               # None if attr doesn't exist on obj (no AttributeError)
config.?debug ?? False  # get debug if it exists, else False

# Coalesce operator
value ?? default        # default if value is None or Err
```

---

## Error Handling

### check — wrap in Result

`check` catches **all exceptions** and wraps the value in `Ok`/`Err`. It is safe to use on any operation that might raise — including dict/list access, attribute access, etc.

```koatl
result = check risky_call()             # Ok(value) or Err(exception)
result = check expr except ValueError() # only catch ValueError; others propagate
safe = check dict[key] ?? fallback      # safe key lookup — KeyError becomes Err
val   = check obj.attr ?? default       # safe attr access — AttributeError becomes Err

# Pattern-match the result
match check int(user_input):
    Ok(n) => process(n)
    Err() => print("not a number")
```

### try/except

```koatl
result = try:
    risky_operation()
except ValueError(msg=m) =>
    f"value error: {m}"
except KeyError as e =>
    f"key error: {e}"
finally:
    cleanup()
```

### Result monad with @

> **Note**: In Koatl `@` is the **monadic bind** operator, not a decorator. Python's `@decorator` syntax does not exist — use `decorator! value` instead.

```koatl
# @ unwraps Ok values and short-circuits on Err (like ? in Rust / do-notation in Haskell)
process = () =>
    let x = @get_value()        # if Err, function returns that Err immediately
    let y = @transform(x)       # same
    x + y                       # implicitly wrapped in Ok

process()                       # returns Ok(...) or Err(...)

# Works with any monad: Result, Memo, Async, Env
fib = n => if n < 2 then @Memo.pure(1) else memo @fib(n-1) + @fib(n-2)

# Manually match
match some_result:
    Ok(v) => use(v)
    Err(e) => handle(e)
```

---

## Iterators

Most iterator methods require `.iter` first (to get a lazy iterator). Lists, dicts, and ranges are all iterable:

```koatl
[1, 2, 3].iter.map($ * 2).list()        # [2, 4, 6]
[1, 2, 3].iter.filter($ > 1).list()     # [2, 3]
(..10).iter.sum()                       # 45
(..10).iter.filter($ % 2 == 0).list()   # [0, 2, 4, 6, 8]

# List methods (eager — return lists directly, no .iter needed)
[1, 2, 3].map($ * 2)            # [2, 4, 6]  (eager)
[1, 2, 3].filter($ > 1)         # [2, 3]     (eager)

# For loops with pattern destructuring (replaces enumerate, items() etc.)
for [key, value] in my_dict.items():
    print(key, value)

for {name, age} in users:           # destructure record fields
    print(f"{name} is {age}")

for (i, x) in items.iter.enumerate():
    print(i, x)

# Dict iteration
{a: 1, b: 2}.iter.map([k, v] => (k, v * 10)).dict()

# Common iterator methods
.map(f)           # transform each element
.filter(f)        # keep elements where f is true
.filter_map()     # filter out Err/None and unwrap Ok/Some
.flat_map(f)      # map then flatten
.fold(init, f)    # reduce with accumulator
.sum()            # total
.mean()           # average
.len              # count (property, not method)
.take(n)          # first n elements
.take_while(f)    # take while predicate holds
.skip(n)          # skip first n
.zip(other)       # pair elements
.enumerate()      # (index, element) pairs
.for_each(f)      # execute side effect
.list()           # collect to list
.dict()           # collect to dict
.sorted(key, reverse=False)  # sort
.group_by(f)      # group into dict by key function
.unique()         # deduplicate
.count_by(f)      # count occurrences of each key
.join_str(sep)    # join strings
.associate(f)     # map i => (i, f(i)) into dict
```

---

## Imports

```koatl
import a.b.c            # from a.b import c
import a.b.(c, d, e)   # from a.b import c, d, e
import a.b.*            # from a.b import *
import a.b.c as alias   # aliasing

# Relative imports
import .local           # from . import local
import ..parent         # from .. import parent

# Export
export my_value = 42
export import other.(x, y)
```

---

## Useful Patterns

### Data processing pipeline

```koatl
(..100).iter
    .filter($ % 2 == 0)
    .map($ * $ )
    .take(5)
    .list()     # [0, 4, 16, 36, 64]
```

### Transforming records

```koatl
users = [{name: "Alice", age: 30}, {name: "Bob", age: 25}]

users
    .filter($.age > 26)
    .map({**$, adult: True})
    .iter.for_each(print)
```

### Error-safe API access

```koatl
get_name = (data, user_id) =>
    if not let Ok(user) = check data[user_id]:
        return "unknown"
    if not let Ok(profile) = check user.profile:
        return "no profile"
    check profile.name ?? "unnamed"
```

### Memoized recursive function

```koatl
let fib = n => if n < 2 then @Memo.pure(1) else memo @fib(n-1) + @fib(n-2)
fib(35).run()   # fast — uses Memo cache
```

### Async with error handling

```koatl
fetch_data = url =>
    async memo @Async.from_sync(() =>
        check requests.get(url).json()
    )

result = @fetch_data("https://example.com/api")
result.map($.items()).map_err(e => print(f"Error: {e}"))
```

### Class-like record with methods

```koatl
create_counter = (initial=0) => {
    value: initial
    increment: Record.method! self => {**self, value: self.value + 1}
    add: Record.method! (self, n) => {**self, value: self.value + n}
    get: Record.method! self => self.value
}

let c = create_counter(10)
c.increment().add(5).get()  # 16
```

### Pattern matching on API response

```koatl
match fetch_repos("python"):
    Ok(response) =>
        response["items"]
            .map(create_repo)
            .filter($.stars > 10)
            .iter.sorted($.activity, reverse=True)
            .for_each(print)
    Err(e) =>
        print(f"Error: {e}")
```

### if let for optional unpacking

```koatl
# Safe nested access
if let Ok({name, email}) = check user_data[id]:
    send_email(email, f"Hello, {name}")
```

---

## with as Expression

`with` returns the value of its body — useful for resource-scoped computations:

```koatl
content = with f = open("file.txt"):
    f.read()

# Pattern matching on the context manager result
with [a, b] = open_pair():
    a.read() + b.read()
```

---

## Python Interoperability

**From Python, import a `.tl` file:**

```python
import koatl.runtime  # enables .tl imports
import my_module       # imports my_module.tl
from koatl.runtime import *  # also works
```

**From Koatl, use any Python library directly** — just import and call it:

```koatl
import requests
import json
import datetime.(datetime, timedelta)

response = requests.get("https://example.com")
data = json.loads(response.text)
```

All Python types, functions, and objects work transparently. Koatl's prelude (records, iterators, `check`, etc.) is automatically available without any imports.

---

## Misc Syntax Notes

- **Commas optional** in multiline lists, records, and function calls
- **Semicolons** separate statements on the same line: `let a = 1; let b = 2; a + b`
- **Block expressions**: `(` at end of line starts a block; final expression is the value
- **Block comments**: `#- ... -#` (nestable)
- **Decorators**: `!` applies a function: `property! self => self.value`, `staticmethod! () => ...`
- **Raw strings**: `r"..."` or `"""..."""` — no escape processing
- **Inline if**: `if cond then x else y` (single expression, no `:` after `then`)
- **Infinite range**: `..` iterates from 0 upward

```koatl
# Inline if
x = if valid then process(data) else default

# Block if as expression
x = if valid:
    process(data)
else:
    default
```
