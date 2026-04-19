# Koatl Quick Reference

Koatl is a functional-first language that transpiles to Python. For full details, see [language-reference.md](language-reference.md).

---

## Key Differences from Python

| Feature               | Python                     | Koatl                                                                  |
| --------------------- | -------------------------- | ---------------------------------------------------------------------- |
| Function definition   | `def f(x): return x+1`     | `f = x => x + 1`                                                       |
| Block-scoped variable | (no equivalent)            | `let x = 1`                                                            |
| Pipe                  | (no equivalent)            | `x \|> f` — passes x into f (last arg)                                 |
| Method pipe           | (no equivalent)            | `x->f(args)` = `f(x, args)`; `x->f` = `partial(f, x)`                  |
| Ternary               | `Y if X else Z`            | `if X then Y else Z`                                                   |
| Coalesce              | (no equivalent)            | `x ?? default`, `x ??= val`                                            |
| Optional method pipe  | (no equivalent)            | `x?->f(args)` — maps over `Ok`/non-`None`, passes through `Err`/`None` |
| List comprehension    | `[f(x) for x in xs]`       | `xs.iter.map(f).list()`                                                |
| Import from           | `from a.b import c, d`     | `import a.b.(c, d)`                                                    |
| Class definition      | `class Foo:`               | `Foo = class:` or `Foo = class(Base):`                                 |
| Decorator             | `@decorator; def ...`      | `decorator! () => ...`                                                 |
| Delegate args         | (no equivalent)            | `(delegate target(x, y))` — copy kwonly args from target's signature   |
| Slice syntax          | `[1:5]`, `[::2]`           | `[1..5]`, `[....2]` (use `..`). Slices are also now iterable.          |
| `except` clauses      | `except TypeError as e:`   | `except TypeError() =>` (match arm)                                    |
| `with` binding        | `with f = open(...) as f:` | `with f = open(...):`                                                  |
| Line continuation     | `\` or open delimiters     | Indentation-based — indented next line auto-continues                  |
| Block expression      | (no equivalent)            | `(let x = 1; x + 2)` — paren block evaluates to last expression        |
| Set literals          | `{1, 2, 3}`                | `set([1, 2, 3])` — `{...}` is always a record                          |

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

## Delegate

`delegate` copies arg names/defaults from another function. Implicitly starts keyword-only section:

```koatl
let target = (*, x=10, y=20) => x + y

let f = (a, delegate target(x)) => (a, x)          # basic
let g = (delegate target(x as local_x)) => local_x # alias
let h = (delegate target(x=42)) => x               # override default
let w = (delegate target(x, **kw)) => target(x=x, **kw)  # **kwargs spread
```

---

## Variables & Scope

```koatl
let x = 1       # Block-scoped (doesn't leak out of if/while/for)
x = 2           # Unscoped (Python-style)
const y = 3     # Block-scoped constant
export z = 4    # Added to __all__
global g = 5    # Global scope
```

---

## Pipes & Placeholders

```koatl
data |> transform |> save_to(db, ?, format="json")  # |> passes as sole arg

x->f(a, b)         # f(x, a, b)     — -> passes as first arg
x->f               # partial(f, x)  — no parens = partial
x?->parse()        # None/Err passthrough, maps over Ok/Some

list.map($ * 2)    # $ = anonymous fn from expression
save_to(db, ?, f)  # ? = partial application hole
results.(x => x.value * scale)  # .() inline lambda
```

---

## Ranges & Slices

```koatl
..10       1..10       1..10..2       # range(0,10)  range(1,10)  range(1,10,2)
arr[..3]   arr[1..4]   arr[..5..2]    # arr[:3]      arr[1:4]     arr[:5:2]
```

---

## Pattern Matching

```koatl
match x:
    0 => "zero"
    1 | 2 => "one or two"
    [a, *rest] => f"head {a}"
    {name, age} => f"{name} is {age}"
    n if n > 0 => "positive"
    .y => "matches value of y"      # . prefix = value match (not capture)
    _ => "default"

x matches [_, _]        # capture-free boolean test
x not matches None
```

---

## if let / while let

```koatl
if let [a, b] = some_list:
    use(a, b)

if not let Ok(value) = result:  # guard-let: body diverges, captures leak out
    return
use(value)
```

---

## Records & Classes

```koatl
person = {name: "Alice", age: 30}
{**person, age: 31}                 # spread/update
{(key): 123}                        # computed key

Animal = class:
    __init__ = (self, name) => self.name = name
    speak = self => f"{self.name}!"
    label = property! self => f"Animal({self.name})"

Dog = class(Animal):
    fetch = self => f"{self.name} fetches!"
```

---

## Safe Navigation & Error Handling

```koatl
obj?.prop   obj?[0]   obj?(arg)     # None if obj is None/Err
obj.?attr                            # None if attr doesn't exist
x ?? default                         # default if None or Err

result = check risky()               # Ok(value) or Err(exception)
result = check expr except ValueError()  # filtered check
safe = check dict[key] ?? fallback

# try/except with match arms
try: parse(raw)
except ValueError(args=[m]) => f"bad: {m}"
except KeyError() | TypeError() => "key/type error"

# @ monadic bind — Err short-circuits
process = () =>
    let x = @get_value()
    let y = @transform(x)
    x + y
```

---

## Iterators

Call `.iter` on any list/dict/range/string to enter the lazy iterator pipeline:

```koatl
[1,2,3].iter.map($ * 2).list()          # [2, 4, 6]
(..10).iter.filter($ % 2 == 0).sum()    # 20
[1,2,3].map($ * 2)                      # eager on concrete containers
{a:1}.map_values($ * 10)                # {a:10}
```

| Category  | Methods                                                                                                                    |
| --------- | -------------------------------------------------------------------------------------------------------------------------- |
| Transform | `.map`, `.filter`, `.flat_map`, `.filter_map`, `.enumerate`, `.zip`, `.chain`, `.unique`, `.sorted`, `.reversed`, `.cycle` |
| Slice     | `.take(n)`, `.skip(n)`, `.take_while(f)`, `.skip_while(f)`                                                                 |
| Aggregate | `.fold`, `.sum`, `.mean`, `.min`, `.max`, `.tally`, `.join`, `.all`, `.any`                                                |
| Find      | `.first`, `.find(f)`, `.last`, `.at(i)`                                                                                    |
| Collect   | `.list`, `.set`, `.tuple`, `.dict`, `.record`, `.group_by`, `.count_by`, `.associate`                                      |

Dict/Record extras (eager): `.map_values`, `.map_keys`, `.filter_values`, `.filter_keys`

---

## Imports

```koatl
import a.b.c                # from a.b import c
import a.b.(c, d)           # from a.b import c, d
import a.b.*                # from a.b import *
import a.b.c as alias

import a.b.(
    c           # from a.b import c
    c.d         # from a.b.c import d
    .x          # from a import x  (. pops one level)
    .           # from a import b  (module itself)
)

export import other.(x, y)  # re-export
```

---

## Tips

- **Use `??` instead of `if x is not None then x else y`:**

    ```koatl
    # Bad
    if x is not None then x else default_value
    # Good
    x ?? default_value
    ```

- **Use `match` instead of long if/else chains:**

    ```koatl
    # Bad
    if status == "ok" then handle_ok()
    else if status == "error" then handle_error()
    else if status == "pending" then handle_pending()
    else handle_unknown()

    # Good
    match status:
        "ok" => handle_ok()
        "error" => handle_error()
        "pending" => handle_pending()
        _ => handle_unknown()
    ```

- **Use `check` + `??` instead of try/except for simple fallbacks:**

    ```koatl
    # Bad
    result = try:
        config[key]
    except KeyError() => default
    # Good
    result = check config[key] ?? default
    ```

- **Use `if not let` for early returns instead of nested conditionals:**

    ```koatl
    # Bad
    let result = check fetch(url)
    if result matches Ok(_):
        let data = result.unwrap()
        process(data)
    else:
        return "failed"

    # Good
    if not let Ok(data) = check fetch(url):
        return "failed"
    process(data)
    ```

- **Use `@` monadic bind to chain fallible operations instead of nested checks:**

    ```koatl
    # Bad
    let a = check step1()
    if a matches Err(_): return a
    let b = check step2(a.unwrap())
    if b matches Err(_): return b
    a.unwrap() + b.unwrap()

    # Good
    let a = @step1()
    let b = @step2(a)
    a + b
    ```

- **Use `->` method pipe to avoid inside-out nesting:**

    ```koatl
    # Bad
    sorted(filter(map(data, transform), predicate))
    # Good
    data->map(transform)->filter(predicate)->sorted()
    ```

---

## Misc

- **Commas optional** in multiline lists, records, calls
- **Semicolons** separate statements on one line
- **Block comments**: `#- ... -#` (nestable)
- **`with` as expression**: `content = with f = open("x"): f.read()`
- **`@` is monadic bind**, not decorator — use `decorator! value`
- **Block expressions**: `(` at end-of-line opens block; last expr is value. Inline: `(let a = 1; a + 2)`
- **Line continuations**: indented next line auto-continues (no `\` needed)
- **F-strings**: `:` format delimiter at bracket depth 0 — `f"{pi:.2f}"`

---
