# Koatl

Python's strength was never its syntax — it was the ecosystem, the libraries, the sheer momentum of a language everyone already knows. Koatl compiles to that same Python but lets you write what you mean.

Same runtime, same `pip install`, same deployment — just without the ceremony that creeps into daily work: nesting calls three deep where a pipe would do, quoting every dictionary key, writing four-line `try`/`except` blocks for simple fallbacks, and fighting scope rules that leak variables where they don't belong.

Filtering and sorting a collection in Python reads inside out:

```python
sorted(u.name.upper() for u in users if u.age > 18)
```

In Koatl, the same pipeline reads left to right:

```koatl
users.filter($.age > 18).map($.name.upper()).sorted()
```

`$` lets you easily express lambdas (`$.age > 18` means `x => x.age > 18`), and the chain flows in the order you think about it. The advantage compounds with more involved work — consider filtering, grouping, and ranking a list of orders in Python:

```python
pending = [o for o in orders if o["status"] == "pending"]
grouped = {}
for o in pending:
    grouped.setdefault(o["customer_id"], []).append(o)
totals = [
    {"id": cid, "total": sum(o["price"] for o in items)}
    for cid, items in grouped.items()
    if sum(o["price"] for o in items) > 100
]
totals.sort(key=lambda x: x["total"], reverse=True)
result = totals[:10]
```

In Koatl:

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

Each step occupies one line, with no temporary variables and no `setdefault` incantations. Records use unquoted keys with dot access, and iterators gain `.filter`, `.map`, `.group_by`, and the rest through Koatl's [extension system](extensions). That directness extends to error handling, scoping, and function definitions:

```koatl
# One-line fallback instead of a four-line try/except
let config = check load_config() ?? default

# Arrow functions that go beyond a single expression
let process = (data, opts) =>
    let cleaned = data.filter($.valid)
    cleaned.map($.value * opts.scale)

# Block-scoped variables — inner bindings stay inner
let x = 1
if True:
    let x = 2
print(x)  # 1

# Pipes replace nested calls
data | do_something | transform | save_to(db, $, format="json")
```

> Koatl is under active development. Features might change.

## Quick Start

```bash
pip install koatl koatl-kernel
```

Optionally install the `quetzal-koatl` extension on VSCode for syntax highlighting.

```koatl
# hello_world.tl
"hello world" | print
```

```bash
koatl hello_world.tl
```

### Jupyter

Select the Koatl kernel in Jupyter, or start an interactive session with `koatl` in the terminal.

From an existing IPython kernel:

```python
%load_ext koatl.notebook
```

### Using Koatl from Python

`.tl` files can be imported directly by importing the runtime first:

```python
import koatl.runtime
import hello_world
```

## Features

The sections below address Python's main paper cuts: limited lambdas, leaky scoping, ceremonial error handling, and noisy dict syntax. None are fatal on their own — which is precisely why Koatl transpiles _to_ Python rather than replacing it — but over the course of a large project, the friction accumulates.

### Piping & placeholders

The `|` operator pipes a value into the next function and `$` creates a lambda in place, replacing inside-out nesting with a linear chain:

```koatl
data
    | do_something
    | transform
    | save_to(db, $, format="json")
```

<details>
<summary>Python equivalent</summary>

```python
save_to(db, transform(do_something(data)), format="json")
```

</details>

`$` works anywhere you would otherwise reach for `lambda`:

```koatl
users.iter.filter($.age > 18).map($.name.upper()).sorted()
```

<details>
<summary>Python equivalent</summary>

```python
sorted(x.name.upper() for x in users if x.age > 18)
```

</details>

### Arrow functions

Koatl replaces both `def` and `lambda` with `=>`, which handles one-liners, multiline bodies, and pattern-matched arguments uniformly:

```koatl
# One-liner
let add = (a, b) => a + b

# Multi-line: just indent
let process = (data, threshold) =>
    let filtered = data.filter($ > threshold)
    filtered.map($ * 2)

# Pattern-matched argument
let head = [first, *rest] => first
```

<details>
<summary>Python equivalent</summary>

```python
add = lambda a, b: a + b

def process(data, threshold):
    filtered = [x for x in data if x > threshold]
    return [x * 2 for x in filtered]

def head(arg):
    first, *rest = arg
    return first
```

</details>

### Everything is an expression

`if`, `match`, `try`, and `with` all produce values, so you can bind the result directly rather than declaring a variable above the block and assigning inside each branch:

```koatl
let result = try:
    parse(raw_input)
except ValueError(msg=m) => f"bad input: {m}"
except _ => default_value

let label = status match:
    200 | 201 => "ok"
    404 => "not found"
    code if code >= 500 => f"server error: {code}"
    _ => "unknown"

let content = with f = open("data.txt"):
    f.read()
```

<details>
<summary>Python equivalent</summary>

```python
try:
    result = parse(raw_input)
except ValueError as e:
    result = f"bad input: {e}"
except Exception:
    result = default_value

match status:
    case 200 | 201: label = "ok"
    case 404: label = "not found"
    case code if code >= 500: label = f"server error: {code}"
    case _: label = "unknown"

with open("data.txt") as f:
    content = f.read()
```

</details>

The pattern of initializing a variable to `None` above a block and assigning inside each branch disappears entirely.

### Error handling

`check` wraps a call into `Ok(value)` or `Err(exception)` instead of raising, and `??` provides a fallback for `None` and `Err` values. The `@` operator ([monadic bind](monads)) unwraps `Ok` values and short-circuits on `Err`, so chaining fallible steps stays flat instead of producing nested `try`/`except` blocks:

```koatl
let config = check load_config() ?? default_config
let port = check int(env["PORT"]) ?? 8080

# Chain fallible operations — any failure short-circuits
let result = () =>
    let data = @fetch(url)
    let parsed = @parse(data)
    transform(parsed)

result() match:
    Ok(value) => use(value)
    Err(e) => log(e)
```

<details>
<summary>Python equivalent</summary>

```python
try:
    config = load_config()
except Exception:
    config = default_config

try:
    port = int(os.environ["PORT"])
except (KeyError, ValueError):
    port = 8080

def result():
    try:
        data = fetch(url)
    except Exception as e:
        return ("err", e)
    try:
        parsed = parse(data)
    except Exception as e:
        return ("err", e)
    return ("ok", transform(parsed))

status, value = result()
if status == "ok":
    use(value)
else:
    log(value)
```

</details>

### Lexical scoping

Python variables assigned inside `if`, `for`, and `with` blocks leak into the enclosing scope; closures require `nonlocal` to mutate outer bindings. Koatl's `let` introduces proper lexical scoping:

```koatl
let x = 1
if True:
    let x = 2
    print(x)    # 2
print(x)        # 1 — not clobbered
```

```koatl
let make_counter = () =>
    let count = 0
    () =>
        count += 1  # no 'nonlocal' needed
        count
```

<details>
<summary>Python equivalent</summary>

```python
x = 1
if True:
    x = 2       # clobbers the outer x
    print(x)    # 2
print(x)        # 2 — surprise

def make_counter():
    count = 0
    def inner():
        nonlocal count  # easy to forget
        count += 1
        return count
    return inner

# The classic loop closure gotcha
funcs = [lambda: i for i in range(5)]
[f() for f in funcs]  # [4, 4, 4, 4, 4] — oops
```

</details>

### Pattern matching

Python 3.10 added `match`/`case`. Koatl extends that foundation with `if let` and `while let` for conditional destructuring, a `matches` operator for capture-free boolean tests, destructuring in function arguments and `for` loops, pattern-matched `except` blocks, and a `.` prefix for matching against existing variables rather than capturing new ones (see [Pattern matching](match) for the full reference).

```koatl
let expected = 200
status match:
    .expected => "ok"
    404 => "not found"
    code if code >= 500 => f"server error: {code}"
    _ => "other"

# Destructure inline
if let {name, age} = get_user():
    print(f"{name} is {age}")

# Capture-free boolean test
if response.status matches 200 | 201:
    process(response)
```

`if not let` is the inverse: the body must exit early (return, raise, break), and the captures become available in the surrounding scope — the same idea as Swift's `guard let`:

```koatl
if not let {host, port} = check load_config():
    raise ConfigError("missing config")

# host and port are safe to use here — we raised otherwise
start_server(host, port)
```

<details>
<summary>Python equivalent</summary>

```python
# Python can't match against a variable without wrapping it
from types import SimpleNamespace
ns = SimpleNamespace(expected=200)
match status:
    case ns.expected: ...
    case 404: ...
    case code if code >= 500: ...

if (user := get_user()) is not None:
    name = user["name"]  # manual destructuring
    age = user["age"]
    print(f"{name} is {age}")

if response.status in (200, 201):
    process(response)

# guard let equivalent
config = load_config()
if config is None or "host" not in config or "port" not in config:
    raise ConfigError("missing config")
host = config["host"]
port = config["port"]
start_server(host, port)
```

</details>

### Records

Records are dictionaries with unquoted keys, dot access, and optional commas in multiline definitions:

```koatl
let user = {name: "Alice", age: 30, active: True}
user.name           # "Alice"
user["name"]        # also works

let users = [
    {name: "Alice", age: 30}
    {name: "Bob", age: 25}
]
users.filter($.age > 26).map($.name)  # ["Alice"]
```

`.?` returns `None` for missing attributes instead of raising, while `?.` short-circuits through `None` values:

```koatl
let debug = config.?debug_mode ?? False
let city = response?.data?.user?.address?.city ?? "unknown"
```

<details>
<summary>Python equivalent</summary>

```python
user = {"name": "Alice", "age": 30, "active": True}
user["name"]  # quotes everywhere

users = [
    {"name": "Alice", "age": 30},
    {"name": "Bob", "age": 25},
]
[u["name"] for u in users if u["age"] > 26]

debug = getattr(config, "debug_mode", False)

# safe nested access
try:
    city = response.data.user.address.city
except (AttributeError, TypeError):
    city = "unknown"
```

</details>

### Extension methods

Extension methods attach new methods to any type, including builtins, without modifying class definitions — and [traits](extensions) are applied automatically to any type that satisfies their requirements:

```koatl
[1, 2, 3].map($ * 2).filter($ > 3)          # [4, 6]
"hello".iter.map($.upper()).join("")          # "HELLO"
{a: 1, b: 2}.map_values($ * 10)              # {a: 10, b: 20}

# Define your own
Extension.method(int, "factorial")! self =>
    if self <= 1 then 1 else self * (self - 1).factorial()

(5).factorial()  # 120
```

### Ranges & slicing

The `..` operator creates slices as first-class values that compose naturally with the rest of the language:

```koatl
(..10).iter.map($ ** 2).list()         # [0, 1, 4, 9, 16, 25, 36, 49, 64, 81]
[1, 2, 3, 4, 5][2..]              # [3, 4, 5]

(1..100).iter
    .filter($ %% 7 == 0)
    .map($ ** 2)
    .take(5)
    .list()                       # [49, 196, 441, 784, 1225]
```

<details>
<summary>Python equivalent</summary>

```python
[x ** 2 for x in range(10)]
[1, 2, 3, 4, 5][2:]

from itertools import islice
list(islice(
    (x ** 2 for x in range(1, 100) if x % 7 == 0),
    5
))
```

</details>

### Small things that add up

Koatl unifies Python's `import` and `from ... import` into dot-separated paths, adds nestable `#- block -#` comments, and drops the commas that multiline collections don't need (see [Formatting & syntax](formatting) and [Modules](modules) for details).

```koatl
import os.path.join              # from os.path import join
import collections.(Counter, defaultdict)

let config = {
    host: "localhost"
    port: 8080
    options: [
        "verbose"
        "debug"
    ]
}

#- This is a block comment.
   They nest: #- like this -# and continue. -#
```

## Putting it together

A small CLI tool that ties several features together:

```koatl
import json.loads
import os.environ

let load_config = path =>
    let raw = @check open(path).read()
    check loads(raw)

let get_port = config =>
    config match:
        {port} if port > 0 and port < 65536 => port
        {port} => raise ValueError(f"invalid port: {port}")
        _ => check int(environ["PORT"]) ?? 8080

let main = () =>
    if not let Ok(config) = load_config("config.json"):
        print("No config found, using defaults")
        config = {}

    let port = get_port(config)
    let host = config.?host ?? "localhost"
    let debug = config.?debug ?? False

    print(f"Starting server on {host}:{port} (debug={debug})")

main()
```

<details>
<summary>Python equivalent</summary>

```python
import json
import os

def load_config(path):
    try:
        with open(path) as f:
            return ("ok", json.loads(f.read()))
    except Exception as e:
        return ("err", e)

def get_port(config):
    if "port" in config:
        port = config["port"]
        if port > 0 and port < 65536:
            return port
        raise ValueError(f"invalid port: {port}")
    try:
        return int(os.environ["PORT"])
    except (KeyError, ValueError):
        return 8080

def main():
    status, value = load_config("config.json")
    if status == "ok":
        config = value
    else:
        print("No config found, using defaults")
        config = {}

    port = get_port(config)
    host = config.get("host", "localhost")
    debug = config.get("debug", False)

    print(f"Starting server on {host}:{port} (debug={debug})")

main()
```

</details>
