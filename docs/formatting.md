# Formatting & Syntax

Syntactic refinements over Python.

## Better continuations

Line continuations are inferred from indentation — no more trailing backslashes:

```koatl
a = 1 +
    2 +
    3

a()
    .do_thing()
    .do_other_thing()
```

## Scopes

Koatl adds block scoping to Python:

```koatl
let a = 1
if True:
    let a = 2
    print(a)
print(a)

# Prints:
# 2
# 1
```

The `nonlocal` keyword is never needed, since declarations later in a scope cannot affect previous statements:

```koatl
f = () =>
    let a = 1
    g = () =>
        a += 2 # nonlocal not needed here
        let a = 4
        print(a)
    g()
    print(a)
f()

# Prints:
# 4
# 3
```

## Nestable block comments

Koatl adds `#- -#` block comments that can nest inside each other:

```koatl
x = #- this is a #- nested -# comment -# 2
```

> Note: The syntax highlighter does not render these correctly, but they work in code.

## Block expressions

Parenthesized blocks evaluate to their final expression, like Rust. An opening `(` at the end of a line starts a new block, and statements within a block can also be separated by `;`.

```koatl
x = (
    a = 2
    b = 3
    a + b
)

x == 5
```

```koatl
x = 2 + (
    if True:
        2
    else:
        3
)
x == 4
```

```koatl
x = (let x = 123; x)
x == 123
```

```koatl
foo = x =>
    do_stuff()
    42

foo(1) == 42
```

## Optional commas

Multiline lists, records, and function calls can drop commas:

```koatl
my_list = [1, 2+2, 3]
my_list = [
    1
    2 +
        2
    3
]

# {} defines a Record (a dict subclass that behaves like Javascript objects)
my_record = {
    1: 4
    asdf: 4
    (1+5): 4
}

my_record[1] == 4
my_record["asdf"] == 4
my_record.asdf == 4
my_record[6] == 4

function_call(1, 2, callback_arg => 42)
function_call(
    1
    2
    callback_arg =>
        42
)
```

## Strings

Regular strings work as expected, though many escape sequences are not yet supported.

### Raw strings

Unlike Python, `r"..."` and `"""..."""` have identical semantics of not processing escape sequences. The latter can be extended with arbitrarily many `"` characters if needed:

```koatl
"""""Having up to four """" in this string is possible."""""
```

### f-strings

Like elsewhere in Koatl, f-strings can contain blocks:

```koatl
f"Hello, my name is {
    let name = "Maryam"
    name += " Mirzakhani"
    name
}"
```

```koatl
f"{123:.2f}"
f"{(if cond: x):.2f}"   # : inside () is not a format delimiter
```

### Verbatim f-strings

Verbatim f-strings are written as `rf"..."` or `f"""..."""`.
