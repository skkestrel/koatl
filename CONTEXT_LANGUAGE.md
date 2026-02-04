# Koatl Language - Introduction & Reference

## Overview

**Koatl** is a functional-first language that transpiles to Python. It brings expressive, concise functional programming idioms to Python while maintaining full Python interoperability. The language is designed to improve code readability, reduce boilerplate, and provide powerful abstractions for data processing and control flow.

## Core Language Features

### 1. **Arrow Functions & Lambda Syntax**

Koatl replaces Python's `def` keyword with concise arrow function syntax:

```koatl
let fib = x => x matches 0 | 1 then 1 else fib(x-1) + fib(x-2)
let add = (a, b) => a + b
let greet = name => "Hello, " + name | print
```

**Function Definition Forms**:

**Unary functions** (single pattern argument):

- `x => body` - Single expression
- `[a, b] => a + b` - Pattern-matched argument
- `[x, *rest] => ...` - With spread operator

**Parenthesized functions** (multiple arguments with Python-like syntax):

```koatl
(a, b) => a + b
(a, b, *args) => ...  # Variable arguments
(a, b, **kwargs) => ...  # Keyword arguments
(a, b, *args, **kwargs) => ...  # Both
(a, *, kw_only) => ...  # Keyword-only arguments
(a, /) => ...  # Positional-only arguments
```

**Pattern Matching in Arguments**:

Parenthesized function arguments support pattern matching (but not guards):

```koatl
([x, y]) => x + y  # Destructure list argument
({name, age}) => f"{name} is {age}"  # Destructure record
([x, y], default=10) => x + y + default  # With defaults
```

**Argument Defaults** (only in parenthesized functions):

```koatl
(a, b=10) => a + b
(x=0) => x  # Defaults require parenthesized syntax
```

**Block vs. Inline**:

- Single expression: `x => x + 1`
- Inline block: `x => let y = 1; y + x`
- Multi-line block:
    ```koatl
    x =>
        let y = 1
        y + x
    ```

### 2. **Pattern Matching**

Koatl provides sophisticated pattern matching in multiple contexts:

#### Pattern Types

1. **Literal Patterns**: Match specific values

    ```koatl
    x match:
        1 => "one"
        "hello" => "greeting"
        True => "truthy"
    ```

2. **Capture Patterns**: Bind values to variables

    ```koatl
    [head, tail] => ...  # Binds elements to variables
    {name: n} => ...     # Extracts and binds fields
    x => ...             # Captures any value as x
    _ => ...             # Wildcard (ignores value)
    ```

3. **Value Patterns**: Match against variables/attributes (use `.` prefix)

    ```koatl
    y = 2
    x match:
        .y => "matched the constant 2"      # Match against variable y
        .module.value => ...                # Match against module attribute
        y => "capture any value as y"       # Different: binds new variable
    ```

4. **Sequence Patterns**: Lists and tuples

    ```koatl
    x match:
        [] => "empty list"
        [a] => "single element"
        [a, b, c] => "three elements"
        [a, *rest] => "first element and rest"
        [*rest, last] => "all but last"
    ```

5. **Mapping Patterns**: Records and dicts

    ```koatl
    x match:
        {} => "empty record"
        {a: x} => "record with key a"
        {a, b} => "shorthand for a: a, b: b"
        {**rest} => "capture remaining fields"
    ```

6. **Class Patterns**: Constructor patterns

    ```koatl
    x match:
        Point(x, y) => ...  # Match Point instance
        Exception(msg=m) => ...  # With named patterns
    ```

7. **Or Patterns**: Alternatives with `|`

    ```koatl
    x match:
        1 | 2 | 3 => "one, two, or three"
        [a, b] | {x: a, y: b} => ...  # List or record form
    ```

8. **As Patterns**: Bind after matching
    ```koatl
    x match:
        [a, *rest] as whole => ...  # Bind entire pattern to 'whole'
    ```

#### In Match Expressions

Match supports both postfix and classic syntax:

**Postfix match** (preferred for expressions):

```koatl
result = x match:
    [a, b, c] => a + b + c
    {name: n, age: a} => f"{n} is {a} years old"
    [_] => "Single element list"
    _ => "Default case (catch-all)"
```

**Classic match** (preferred for statements/side effects):

```koatl
match x:
    [a, b, c] => print(a + b + c)
    _ => print("default")
```

> **Style Note**: Use postfix match (`x match:`) when the result is used as an expression. Use classic match (`match x:`) when performing side effects or when the match is a top-level statement.

Match expressions also support **guards**:

```koatl
x match:
    [a, b] if a > b => "a is larger"
    [a, b] => "b is larger or equal"
```

#### In Function Arguments

```koatl
process = [head, *tail] => (head, tail)  # Destructure list patterns
get_value = {key: k, value: v} => v     # Destructure record patterns

# With guards
compare = (a, b) if a > b => a
compare = (a, b) => b
```

#### Pattern Matching with If-Expressions

```koatl
if x matches [a, *b]:
    print(a, b)  # Variables a and b are available in scope

if x not matches [_, _]:
    return None

# Inverse matching: must return/raise/break/continue
if [1, 2] not matches [x, y]:
    return
print(x, y)  # x and y are available (safe because we returned otherwise)
```

### 3. **Piping & Method Chaining**

Convenient piping with `|` and `.()` operators:

```koatl
# Using | pipe operator
data
    | do_something
    | do_other_thing
    | pass_into_second_arg(a, $, option="yes")

# Using .(f) for higher precedence
(..10).map($ + 1).(filter $ > 5).(print)
```

### 4. **Placeholder Variables ($)**

The `$` placeholder creates lambdas automatically:

```koatl
f(a, $, c)              # => x => f(a, x, c)
($ + 2 * y / 4)         # => x => x + 2 * y / 4
list.map($ * 2)         # => list.map(x => x * 2)
list.filter($ > 5)      # => list.filter(x => x > 5)
```

### 5. **If-Expressions**

If statements return values and support two syntaxes:

**Classic (Python-style)** - preferred for statements/side effects:

```koatl
if condition:
    do_something()
    do_other_thing()
else:
    do_alternative()
```

**Postfix (inline)** - preferred for expressions:

```koatl
y = condition then 10 else 20
x = is_valid then process(data) else default_value
```

> **Style Note**: Use postfix if (`then`/`else`) for inline expressions where you need the result. Use classic if (`if:`/`else:`) for multi-statement blocks or side effects.

### 6. **Check-Expressions**

Elegantly handle exceptions without breaking flow:

```koatl
result = check some_value           # Ok(value) or Err(exception)
result = check risky_call except ValueError()  # Catch specific exceptions
```

### 7. **Coalescing Operators**

Handle None, Err, and exceptions gracefully:

```koatl
value = check get_config() ?? default_value
result = None?.property             # => None
result = Ok([1,2,3])?[0]           # => Ok(1)
```

### 8. **Better Slicing & Ranges**

Cleaner slice syntax usable outside lists:

**Slice Syntax**: `[start..stop..step]`

```koatl
# Standard slices
first_three = [1, 2, 3, 4, 5][..3]      # [1, 2, 3]
from_two = [1, 2, 3, 4, 5][2..]        # [3, 4, 5]
middle = [1, 2, 3, 4, 5][1..4]         # [2, 3, 4]
with_step = [1, 2, 3, 4, 5][..5..2]    # [1, 3, 5] - every 2nd element

# Slices are first-class values (can be stored and reused)
my_slice = ..5
some_array[my_slice]

# Ranges (slices with only dots)
..10        # Range from 0 to 10
5..         # Range from 5 to end
1..10..2    # Range from 1 to 10 with step 2

# Ranges are iterable
for i in ..10:
    print(i)

# Can be used with iterator methods
(..100)
    .map($ * 2)
    .filter($ > 50)
    .list()
```

### 9. **Records (Enhanced Dicts)**

Javascript-like object syntax:

```koatl
x = {a: 1, b: 2, c: 3}

# Keys don't need quotes
# Records support dot notation like Javascript
x.a == 1
x["a"] == 1

# Expressions as keys
key = "my_key"
obj = {(key): "value", other: 42}

# Block expressions as keys (for complex computed keys)
obj = {
    (
        temp = compute_key()
        temp.upper()
    ): "computed key value"
}

# Records with methods and properties
x = {
    a: 1
    get_value: () => 2
    computed: Record.method& self => self.a * 2
}
```

### 10. **Module System**

Unified import/export syntax:

```koatl
# Simplified imports
import a.b.c.d              # from a.b.c import d
import a.b.c.(d, e, f)      # from a.b.c import d, e, f
import a.b.c.*              # from a.b.c import *

# With relative imports and aliasing
import a.b.c.(
    .e
    d as renamed_d
    f.g.(i, j)
)

# Exports
export import other_module.(item, other_item)
export result = compute()
private_value = 1  # Not exported
```

## Declarations & Statements

### Variable Declarations

Koatl has four declaration modifiers:

```koatl
let x = 1       # Block-scoped mutable variable (Koatl's main declaration)
const y = 2     # Block-scoped constant (Python-style, not enforced)
global z = 3    # Global scope declaration
export a = 4    # Export for module (sets __all__)
```

Multiple declarations on one line:

```koatl
let x, y, z = 1, 2, 3
```

**Key difference from Python**: `let` creates proper block scope. Unlike Python, later binding doesn't affect earlier statements:

```koatl
let a = 1
if True:
    let a = 2
    print(a)    # Prints: 2
print(a)        # Prints: 1  (not affected by inner binding!)
```

### Pattern-Based Assignments

Destructure on assignment:

```koatl
let [x, y] = [1, 2]
let {name: n, age: a} = user
let {name, age} = user  # Shorthand
```

With modifiers:

```koatl
let [a, b] = tuple
const {x, y} = point
```

### Control Flow Statements

**While loops**:

```koatl
while x > 0:
    print(x)
    x -= 1
```

**For loops** (with pattern matching):

```koatl
for x in ..10:
    print(x)

for [key, value] in items:
    print(key, value)

for {name: n, age: a} in users:
    print(n, a)
```

**Break and Continue**:

```koatl
while True:
    if condition:
        break
    if other:
        continue
    print("loop body")
```

**Return statements**:

```koatl
return value
return  # Empty return (returns None)
```

**Raise statements**:

```koatl
raise ValueError("message")
```

### Import Statements

**Basic imports**:

```koatl
import module.path.name        # from module.path import name
import module.path.(a, b, c)   # from module.path import a, b, c
import module.path.*           # from module.path import *
```

**Relative imports**:

```koatl
import .local_module           # from . import local_module
import ..parent_module         # from .. import parent_module
import ...grandparent          # from ... import grandparent
```

**With aliasing**:

```koatl
import module.path.name as alias
import module.(a as x, b as y)
```

**Exporting**:

```koatl
export import other_module.(item, other)
export my_value = 42
```

### Expression Statements

Any expression can be a statement:

```koatl
print("hello")
x + y
obj.method(arg)
check risky_operation
```

## Control Flow Expressions

These are expressions that return values and control program flow:

### 1. **If-Expressions**

If can be both expression and statement, with two syntaxes:

**Classic syntax** (Python-style):

```koatl
x = if condition:
    value_if_true
else:
    value_if_false

# Can be chained
z = if a:
    1
elif b:
    2
else:
    3
```

**Postfix syntax** (inline):

```koatl
y = condition then 10 else 20

# Can also chain
z = a then 1 else b then 2 else 3
```

**Pattern matching in conditions**:

```koatl
if x matches [a, b]:
    print(a, b)  # a and b are bound here

if x not matches pattern:
    # Must return/break/continue/raise here
    return
# Now safe to use x assuming pattern didn't match
```

### 2. **Match-Expressions**

True pattern matching with guards, supporting two syntaxes:

**Postfix match** (preferred for expressions):

```koatl
result = x match:
    1 => "one"
    2 => "two"
    [a, b] if a > b => f"pair {a} > {b}"
    [a, b] => f"pair {a} <= {b}"
    {type: "error", msg: m} => f"error: {m}"
    {type: "ok", data: d} => f"success: {d}"
    _ => "unknown"
```

**Classic match** (preferred for statements/side effects):

```koatl
match x:
    1 => print("one")
    2 => print("two")
    _ => print("unknown")
```

> **Style Note**: Use postfix match (`x match:`) when using the result as an expression. Use classic match (`match x:`) for top-level statements or side effects.

### 3. **With-Expressions**

Context managers that return values:

```koatl
content = with f = open("file.txt"):
    f.read()

# With pattern matching
with [file1, file2] = open_pair():
    file1.read() + file2.read()
```

### 4. **Try-Except-Finally Expressions**

Exception handling with pattern matching:

```koatl
result = try:
    risky_operation()
except ValueError(msg=m) if len(m) > 0 =>
    f"long error: {m}"
except ValueError() =>
    "short error"
except KeyError as e =>
    f"key error: {e}"
finally:
    cleanup()
```

### 5. **Memo-Expressions**

Memoization with automatic dependency tracking:

```koatl
# Lazy memoized computation
result = memo:
    expensive_computation()

# Async memoization
async_result = async memo:
    await fetch_data()

# In functions
fib = x =>
    x < 2 then @Memo.pure(1) else memo @fib(x-1) + @fib(x-2)
```

### 6. **Await & Yield Expressions**

```koatl
# Await expressions
result = await async_operation()

# Yield expressions
gen = x => (
    yield 1
    yield x + 2
)

# Yield from
combined = => yield from other_generator()
```

## Advanced Language Features

### **With-Expressions**

With statements return values:

```koatl
content = with f = open("file.txt", "r"):
    f.read()
```

### **Scoped Variables**

Proper lexical scoping with `let`:

```koatl
let a = 1
if True:
    let a = 2
    print(a)        # Prints: 2
print(a)            # Prints: 1
```

Unlike Python, `nonlocal` is never needed—Koatl's scoping rules prevent binding conflicts.

### **Special Identifiers for Introspection**

Koatl provides special identifiers for runtime introspection:

#### **`__locals__`**

Returns a dictionary mapping Koatl variable names to their Python runtime values in the current scope:

```koatl
test_locals = (arg1, arg2) =>
    let local = 100
    __locals__
    # Returns: {local: 100, arg1: <value>, arg2: <value>}
```

This is useful for debugging, metaprogramming, or passing local context to dynamic code.

#### **`__captures__`**

Returns a dictionary of variables captured from outer scopes (excluding globals):

```koatl
outer = () =>
    let x = 1
    let y = 2
    inner = () =>
        let z = 3
        __captures__  # Returns: {x: 1, y: 2} (not z or globals)
    inner()
```

**Key behaviors**:

- Only available inside functions (error if used at module level)
- Includes variables from parent function scopes
- Excludes global scope variables
- Excludes variables from the current function scope (use `__locals__` for those)
- Uses `globals() | locals()` to access captured values at runtime
- Handles shadowing correctly (inner scope variables override outer ones)

Both `__locals__` and `__captures__` translate Koatl names (like `x`) to their mangled Python equivalents (like `let_x_1`), making them safe for use with Python's runtime introspection.

### **Block Comments**

Nestable block comments:

```koatl
x = #- this is a #- nested -# comment -# 2
```

### **Try-Catch Pattern Matching**

Unified exception handling with pattern matching:

```koatl
try:
    do_something()
except ValueError(msg=m) | TypeError(msg=m) =>
    print(f"Error: {m}")

try:
    risky()
except NameError() as e =>
    print(e)
```

## Blocks & Expression Composition

### Indentation-Based Blocks

Koatl uses Python-style indentation for blocks:

```koatl
if True:
    statement1
    statement2
    let nested_value = inner then:
        x
    else:
        y

while condition:
    loop_body
```

### Block Expressions (Parenthesized Blocks)

Use parentheses to create expression blocks - the final expression is the value:

```koatl
x = (
    a = 2
    b = 3
    a + b    # This is the value (5)
)
```

### Semicolon Separators

Statements can be separated by semicolons for single-line composition:

```koatl
x = (let x = 123; x)  # x == 123

# Or in regular code
let a = 1; let b = 2; a + b
```

### Indentation Rules

- Opening `(` at end of line starts a block on next line
- Indented code continues the block
- Dedent ends the block
- Optional commas in multiline lists, records, and function calls
- No commas needed in multiline indented code

```koatl
my_list = [
    1
    2 +
        2
    3
]

my_record = {
    a: 1
    b: 2
    c: 3
}

function_call(
    arg1
    arg2
    kwarg=value
)
```

## Container Types

### **Records** (Enhanced Dicts)

- Javascript-like syntax with attribute access
- Auto-forwarding attribute lookup to indexing
- Support for methods, properties, and computed fields
- Multiline records without commas:
    ```koatl
    obj = {
        a: 1
        b: 2
        c: 3
    }
    ```

### **Lists**

- Python lists with improved syntax
- Multiline lists without commas:
    ```koatl
    items = [
        1
        2
        3
    ]
    ```

### **Tuples**

- Standard Python tuples: `(1, 2, 3)`
- Comma creates tuples: `a, b = 1, 2`

### **Sets**

- Use constructor: `set([1, 2, 3])`

## Prelude & Standard Library

### **std module**

Lazily-loaded standard library access:

```koatl
std.io.read_file("data.txt") | print
```

### **mod module**

Lazily-loaded Python package proxy:

```koatl
mod.numpy.array([1, 2, 3]) + 2 | print
```

### **Built-in Extensions**

- `.iter` - Makes objects iterable (delegates to `.items()` for dicts)
- `Iterable` trait - Common methods for iterators

## Common Patterns & Examples

### Data Processing Pipeline

```koatl
(..10)
    .map(fib)
    .filter($ % 2 == 0)
    .map($ * 2)
    .list()
```

### Record Transformations

```koatl
users = [
    {name: "Alice", age: 30}
    {name: "Bob", age: 25}
]

users
    .filter($.age > 26)
    .map({**$, adult: True})
    .for_each(print)
```

### API Data Processing

```koatl
fetch_repos("python") match:
    Ok(response) =>
        response["items"]
            .map(create_repo)
            .filter($.stars > 10)
            .sorted($.activity, reverse=True)
            .for_each(print)
    Err(e) =>
        print(f"Error: {e}")
```

### Game of Life Simulation

```koatl
create_cell = (x, y, alive=False) => {
    x: x, y: y, alive: alive
    neighbors: Record.method! (self, size) => [...]
    next_state: Record.method! (self, count) => [...]
}

display_grid = (grid, size) =>
    (..size).for_each(y =>
        (..size)
            .map(x => grid[(x, y)].alive then "██" else "  ")
            .join_str("")
            |print
    )
```

## Advanced Features

### **Monads**

Koatl supports several monadic patterns using the `@` bind operator:

#### **Memo Monad** (Memoization)

```koatl
fib = x => x < 2 then @Memo.pure(1) else memo @fib(x - 1) + @fib(x - 2)

fib(200).run()  # Cached computation
```

Automatically tracks dependencies based on captured variables.

#### **Result Monad** (Error Handling)

```koatl
f = () =>
    x = @get_some_value()           # Returns early if error
    y = @get_some_other_value(x)    # Returns early if error
    x + y

f() # Ok(result) or Err(exception)
```

#### **Async Monad**

```koatl
f = () =>
    print("sleepy")
    @Async.sleep(1)
    print("refreshed!")

await f()
```

#### **Env Monad** (Context/Dependency Injection)

```koatl
g = () =>
    @Env.item("config_key")

f = () =>
    x = @Env.item("first")
    y = @Env.item("second")
    x + y

f().run(config_dict)
```

### **Extension Attributes & Traits**

Non-destructively add methods/properties to any type using a virtual dispatch system powered by `vget`.

#### How Extension Attributes Work

When you access an attribute on an object, Koatl uses `vget` (virtual get) instead of Python's standard attribute lookup. The `vget` system:

1. **First tries standard Python attribute lookup** via `getattr()`
2. **If that fails**, checks two virtual tables (vtables):
    - **Type-based vtable**: Methods/properties registered for specific types
    - **Trait-based vtable**: Methods from traits with requirement checking

This allows you to add methods to any type—even built-in types like `int`, `str`, `None`—without modifying their class definitions.

#### Adding Extension Methods

```koatl
# Add a method to a specific type
Extension.method(int, "double")! self => self * 2

# Now all ints have this method
(5).double()  # => 10
```

#### Adding Extension Properties

```koatl
# Add a property (computed attribute)
Extension.property(list, "len")! self => len(self)

# Access like a regular attribute
[1, 2, 3].len  # => 3
```

#### Trait-Based Extensions

Traits allow you to add methods to multiple types that satisfy requirements. Requirements are specified using `Trait.abstract!`:

```koatl
# Define a trait with requirements
export Iterable = Extension.trait! class:
    # Abstract methods define requirements - types must have these to use the trait
    iter = Trait.abstract! self => ()

    # Concrete methods are only available if requirements are met
    map = self => f => self.iter().map(f)
    filter = self => f => self.iter().filter(f)
    list = self => list(self.iter())

# Now any type with an 'iter' method gets these trait methods automatically
[1, 2, 3].map(x => x * 2)  # Works because list has iter
"hello".map(x => x.upper())  # Works because str has iter (via extension)
```

When you mark a method with `Trait.abstract!`, it becomes a requirement that types must satisfy to use the trait's other methods.

#### Implementation Details

The virtual dispatch system is implemented in Rust for performance:

- **`fast_vset(type, name, value)`**: Register a method/property for a specific type
- **`fast_vset_trait(trait_name, requirements, name, value)`**: Register a trait method
- **`fast_vget(obj, name)`**: Look up method/property via vtables

When you call `obj.method()`, the transpiled code uses `vget(obj, "method")` which:

1. Checks Python's `__getattribute__` first
2. Falls back to type-specific vtable
3. Falls back to trait vtables (checking requirements)
4. Returns `None` if not found (or raises `AttributeError`)

#### Example: Built-in Extensions

The prelude adds many useful extensions:

```koatl
# String pattern matching
"hello world".matches(r"w\w+")  # Uses Extension.method(str, "matches")

# List operations
[1, 2, 3].map(x => x * 2)  # Uses Iterable trait extension

# Dict operations
{a: 1, b: 2}.map_values(x => x * 10)  # Extension.method(dict, "map_values")
```

## Decorators

Use `!` as a decorator shorthand:

```koatl
Foo = class:
    method = staticmethod! () => ...
    prop = property! self => self.value
```

## Attribute Access & Member Selection

### Standard Attribute Access

```koatl
obj.property             # Get attribute
obj.method(arg)          # Method call
obj[key]                 # Subscript (for dicts/lists)
obj["string_key"]        # String key subscript
```

### Safe Navigation (Optional Chaining)

Safely access attributes on potentially None/Err values:

```koatl
result = obj?.property    # None if obj is None
result = obj?[0]          # None if obj is None
result = obj?(arg)        # None if obj is None
result = result?.deeply?.nested?.property  # Chains safely
```

### Maybe Attribute (`.?`)

Try to access an attribute that may not exist, returning None instead of raising AttributeError:

```koatl
result = obj.?attr        # None if attr doesn't exist, otherwise returns the attribute
result = obj.?method()    # Can chain with method calls

# Useful for duck typing and optional attributes
config.?debug_mode ?? False  # Get debug_mode if it exists, otherwise False
```

**Important distinction** - `.?` and `?.` check different things:

- `obj.?attr` (MaybeAttribute) - Checks if `attr` exists on `obj`. Returns None if the attribute doesn't exist (no AttributeError), otherwise returns the attribute value. The object `obj` itself must not be None.
- `obj?.attr` (Safe Navigation) - Checks if `obj` is None/Err first. If `obj` is None/Err, returns None. Otherwise, accesses `attr` normally (which must exist or will raise AttributeError).

### Raw Attribute Access

Bypass `vget` and access the true attribute:

```koatl
obj::__dict__           # Get __dict__ directly, not via __getattr__
obj::__class__          # Get __class__ directly
```

## Operators Reference

### Operator Precedence (highest to lowest)

The parser processes operators in this precedence order:

| #   | Precedence  | Operators                                                                   | Associativity | Note               |
| --- | ----------- | --------------------------------------------------------------------------- | ------------- | ------------------ |
| 11  | Pipe        | `\|`                                                                        | Left-to-right | Lowest precedence  |
| 10  | Coalesce    | `??`                                                                        | Left-to-right |                    |
| 9   | Logical OR  | `or`                                                                        | Left-to-right |                    |
| 8   | Logical AND | `and`                                                                       | Left-to-right |                    |
| 7   | Comparison  | `<`, `>`, `<=`, `>=`, `==`, `<>`, `!=`, `===`, `<=>`, `!==`, `in`, `not in` | Left-to-right |                    |
| 6   | Bitwise OR  | `\|\|`                                                                      | Left-to-right |                    |
| 5   | Bitwise XOR | `^^`                                                                        | Left-to-right |                    |
| 4   | Bitwise AND | `&&`                                                                        | Left-to-right |                    |
| 3   | Shifts      | `<<`, `>>`                                                                  | Left-to-right |                    |
| 2   | Add/Sub     | `+`, `-`                                                                    | Left-to-right |                    |
| 1   | Mul/Div     | `*`, `/`, `//`, `%%`, `@@`                                                  | Left-to-right |                    |
| 0   | Power       | `**`                                                                        | Right-to-left | Highest precedence |

**Special operators** (above binary precedence, processed in order):

- `matches` / `not matches` - Pattern matching test
- `memo` - Memoization
- `with` - Context manager
- `if ... then ... else` - Conditional expression
- `match:` - Pattern matching
- `try: ... except: ... finally:` - Exception handling
- `await` / `yield` - Control flow
- `check` - Error wrapping

### Postfix Operators (highest precedence within expressions)

| Operator | Meaning                                  | Example             |
| -------- | ---------------------------------------- | ------------------- |
| `()`     | Function call                            | `f(a, b)`           |
| `[]`     | Subscript                                | `x[0]` or `x[1..3]` |
| `.attr`  | Attribute access                         | `x.property`        |
| `.?attr` | Maybe attribute (safe nav)               | `x?.property`       |
| `::attr` | Raw attribute (bypass **getattr**)       | `x::__dict__`       |
| `?[]`    | Safe subscript                           | `x?[0]`             |
| `?()`    | Safe call                                | `x?(a, b)`          |
| `.()`    | Scoped call (higher precedence than `.`) | `f.(x)`             |
| `!`      | Decorator/Function call                  | `decorator! value`  |

### Unary Prefix Operators

| Operator | Meaning          |
| -------- | ---------------- |
| `+`      | Unary plus       |
| `-`      | Unary minus      |
| `~`      | Bitwise NOT      |
| `@`      | Monadic bind     |
| `not`    | Logical negation |

### Special Operators

| Operator                                                       | Meaning                                       |
| -------------------------------------------------------------- | --------------------------------------------- |
| `=>`                                                           | Lambda/function definition                    |
| `??`                                                           | Coalesce on None/Err/Exception                |
| `..`                                                           | Range/slice syntax                            |
| `&`                                                            | Decorator shorthand (equiv. to function call) |
| `$`                                                            | Placeholder variable                          |
| `=`                                                            | Assignment                                    |
| `+=`, `-=`, `*=`, `/=`, `//=`, `%=`, `@=`, `**=`, `\|=`, `??=` | Augmented assignment                          |

### Comparison Operators

| Koatl         | Python   | Meaning                |
| ------------- | -------- | ---------------------- |
| `==`          | `==`     | Equality               |
| `<>` / `!=`   | `!=`     | Not equal              |
| `<`           | `<`      | Less than              |
| `<=`          | `<=`     | Less or equal          |
| `>`           | `>`      | Greater than           |
| `>=`          | `>=`     | Greater or equal       |
| `===`         | `is`     | Identity (same object) |
| `<=>` / `!==` | `is not` | Non-identity           |
| `in`          | `in`     | Membership             |
| `not in`      | `not in` | Non-membership         |

## Execution Modes

### As Script

```bash
koatl script.tl
```

### As Module

```python
import koatl.runtime  # Enable .tl imports
import my_script
```

### In Jupyter/IPython

```python
%load_ext koatl.notebook
```

Or with koatl-kernel:

```bash
pip install koatl-kernel
jupyter notebook  # Select Koatl kernel
```
