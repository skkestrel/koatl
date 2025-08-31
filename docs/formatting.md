# Formatting and Whitespace

Koatl improves program readability and formatting in a few ways:

## Better continuations

Line continuations are automatically inferred with indentation: no more backslashes at the end of every line.

```koatl
a = 1 +
    2 +
    3

a()
    .do_thing()
    .do_other_thing()
```

## Scopes

Koatl adds scopes to Python:

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

The `nonlocal` keyword is never needed in Koatl, as declarations later in a scope can never affect previous statements:

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

## Nesting block comments

Koatl adds `#- -#` block comments, which can nest inside each other, allowing more commenting flexibility

```koatl
x = #- this is a #- nested -# comment -# 2
```

> Note: The syntax highligher above is broken, but they do work in code.

## Blocks-in-expressions

Blocks-in-expressions allow long inline things in expressions.
Like Rust, the final expression of a block is treated as its value.

Opening round parentheses at the end of a line starts a block-expression on the next line.

Statements can also be delimited by `;`.

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

## Optional commas in enumerations

Multiline enumerations of lists, records, and function calls can drop commas.

```koatl
my_list = [1, 2+2, 3]
my_list = [
    1
    2 +
        2
    3
]

# {} defines a Record, not a dict - they're a subclass of dict that behaves like Javascript objects
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

Regular strings work as expected, but many escape sequences are not yet supported...

### Raw strings

Unlike Python, `r"..."` and `"""..."""` have the same semantics of "no escape sequences".

The latter version can be extended with arbitrarily many `"` in a row if necessary:

```koatl
"""""Having up to four """" in this string is possible."""""
```

### fstrings

Like everywhere else, f-strings can contain blocks too:

```koatl
f"Hello, my name is {
    let name = "Maryam"
    name += " Mirzakhani"
    name
}"
```

For reasons, format specifiers should be separated using `%` instead of `:`:

```koatl
f"{123%.2f}"
```

### Verbatim fstrings

Verbatim fstrings are either `rf"..."` or `f"""..."""`.
