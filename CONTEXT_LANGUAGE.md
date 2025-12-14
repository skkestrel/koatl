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

-   `x => body` - Single expression
-   `[a, b] => a + b` - Pattern-matched argument
-   `[x, *rest] => ...` - With spread operator

**Parenthesized functions** (multiple arguments with Python-like syntax):

```koatl
(a, b) => a + b
(a, b, *args) => ...  # Variable arguments
(a, b, **kwargs) => ...  # Keyword arguments
(a, b, *args, **kwargs) => ...  # Both
(a, *, kw_only) => ...  # Keyword-only arguments
(a, /) => ...  # Positional-only arguments
```

**Argument Defaults**:

```koatl
(a, b=10) => a + b
[x=0] => x  # Defaults work with patterns too
```

**Block vs. Inline**:

-   Single expression: `x => x + 1`
-   Inline block: `x => let y = 1; y + x`
-   Multi-line block:
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

```koatl
result = x match:
    [a, b, c] => a + b + c
    {name: n, age: a} => f"{n} is {a} years old"
    [_] => "Single element list"
    default: "No match"
```

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

If statements return values:

```koatl
x = if True:
    1
else:
    2

# Or inline syntax
y = condition then 10 else 20
```

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
return  # Empty return (implicitly None)
```

**Raise statements**:

```koatl
raise ValueError("message")
raise  # Re-raise current exception
raise SomeError() from cause  # Python 3+ chaining
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

If can be both expression and statement:

**As an expression** (returns a value):

```koatl
x = if condition:
    value_if_true
else:
    value_if_false

# Inline syntax
y = condition then 10 else 20

# Can be chained
z = if a:
    1
else if b:
    2
else:
    3
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

True pattern matching with guards:

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

### 1. **Monads**

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

### 2. **With-Expressions**

With statements return values:

```koatl
content = with f = open("file.txt", "r"):
    f.read()
```

### 3. **Scoped Variables**

Proper lexical scoping with `let`:

```koatl
let a = 1
if True:
    let a = 2
    print(a)        # Prints: 2
print(a)            # Prints: 1
```

Unlike Python, `nonlocal` is never needed—Koatl's scoping rules prevent binding conflicts.

### 4. **Block Comments**

Nestable block comments:

```koatl
x = #- this is a #- nested -# comment -# 2
```

### 5. **Extension Attributes**

Non-destructively add methods/properties to any type:

```koatl
export SomeTrait = Extension.trait! class(Trait):
    required_method = Trait.abstract! self => ()
    derived_method = self => self.required_method()

Extension.method(object, "my_method")! self => "result"
Extension.property(object, "my_prop")! self => self.value

None.my_method() == "result"
```

### 6. **Try-Catch Pattern Matching**

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
    nested_value = if inner:
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

# Multiline in function calls
result = process(
    initial_setup
    configure_something
    run_main_logic
)

# In expressions
value = 2 + (
    x = 5
    y = 3
    x * y   # Value is 15
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

-   Opening `(` at end of line starts a block on next line
-   Indented code continues the block
-   Dedent ends the block
-   Optional commas in multiline lists, records, and function calls
-   No commas needed in multiline indented code

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

-   Javascript-like syntax with attribute access
-   Auto-forwarding attribute lookup to indexing
-   Support for methods, properties, and computed fields
-   Multiline records without commas:
    ```koatl
    obj = {
        a: 1
        b: 2
        c: 3
    }
    ```

### **Lists**

-   Python lists with improved syntax
-   Multiline lists without commas:
    ```koatl
    items = [
        1
        2
        3
    ]
    ```

### **Tuples**

-   Standard Python tuples: `(1, 2, 3)`
-   Comma creates tuples: `a, b = 1, 2`

### **Sets**

-   Use constructor: `set([1, 2, 3])`

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

-   `.iter` - Makes objects iterable (delegates to `.items()` for dicts)
-   `.result` - Wraps any value as Result
-   `Iterable` trait - Common methods for iterators

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

## Decorators

Use `&` as a decorator shorthand (equivalent to `function(arg)`):

```koatl
Foo = class:
    method = staticmethod& () => ...
    prop = property& self => self.value
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

### Raw Attribute Access

Bypass `__getattr__` and access the true attribute:

```koatl
obj::__dict__           # Get __dict__ directly, not via __getattr__
obj::__class__          # Get __class__ directly
```

### Scoped Attribute Access

Call method on attribute (special syntax):

```koatl
obj.(method)            # Equivalent to obj.method()
obj.(method_name)       # Call attribute method
```

This is useful with high-precedence operators:

```koatl
(..10).(map $ * 2)      # Calls the map method
result.(filter $ > 5)   # Filters result
```

### Qualified Identifiers

For value patterns, use qualified names with `.` or `::`:

```koatl
y = 2
x match:
    .y => ...                 # Value pattern - match constant y
    .module.submodule.value => ...  # Qualified value pattern
    y => ...                  # Capture pattern - bind as y
```

## Operators Reference

### Operator Precedence (highest to lowest)

The parser processes operators in this precedence order:

| #   | Precedence  | Operators                                                      | Associativity | Note               |
| --- | ----------- | -------------------------------------------------------------- | ------------- | ------------------ |
| 11  | Pipe        | `\|`                                                           | Left-to-right | Lowest precedence  |
| 10  | Coalesce    | `??`                                                           | Left-to-right |                    |
| 9   | Logical OR  | `or`                                                           | Left-to-right |                    |
| 8   | Logical AND | `and`                                                          | Left-to-right |                    |
| 7   | Comparison  | `<`, `>`, `<=`, `>=`, `==`, `<>`, `===`, `<=>`, `in`, `not in` | Left-to-right |                    |
| 6   | Bitwise OR  | `\|\|`                                                         | Left-to-right |                    |
| 5   | Bitwise XOR | `^^`                                                           | Left-to-right |                    |
| 4   | Bitwise AND | `&&`                                                           | Left-to-right |                    |
| 3   | Shifts      | `<<`, `>>`                                                     | Left-to-right |                    |
| 2   | Add/Sub     | `+`, `-`                                                       | Left-to-right |                    |
| 1   | Mul/Div     | `*`, `/`, `//`, `%%`, `@@`                                     | Left-to-right |                    |
| 0   | Power       | `**`                                                           | Right-to-left | Highest precedence |

**Special operators** (above binary precedence, processed in order):

-   `matches` / `not matches` - Pattern matching test
-   `memo` - Memoization
-   `with` - Context manager
-   `if ... then ... else` - Conditional expression
-   `match:` - Pattern matching
-   `try: ... except: ... finally:` - Exception handling
-   `await` / `yield` - Control flow
-   `check` - Error wrapping

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

| Koatl      | Python   | Meaning                |
| ---------- | -------- | ---------------------- |
| `==`       | `==`     | Equality               |
| `<>` / `!=`| `!=`     | Not equal              |
| `<`        | `<`      | Less than              |
| `<=`       | `<=`     | Less or equal          |
| `>`        | `>`      | Greater than           |
| `>=`       | `>=`     | Greater or equal       |
| `===`      | `is`     | Identity (same object) |
| `<=>` / `!==` | `is not` | Non-identity           |
| `in`       | `in`     | Membership             |
| `not in`   | `not in` | Non-membership         |

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

## Python Interoperability

-   Full Python library access via `mod` and `std`
-   Seamless integration with Python code
-   Exceptions converted to Result types when using `check`
-   .tl files importable from Python scripts
-   Transpiles to standard Python AST
