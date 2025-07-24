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

## Nesting block comments

Koatl adds `#- -#` block comments, which can nest inside each other, allowing more commenting flexibility

```koatl
x = #- this is a #- nested -# comment -# 2
```

> Note: The syntax highligher above is broken, but they do work in code.

## Blocks-in-expressions

Blocks-in-expressions allow blocks like `if`, `match`, and lambdas to be inline in expressions.
Like Rust, the final expression of a block is treated as its value.

Opening round parentheses on at the end of a line starts a block-expression on the next line.

```koatl
x = 2 + (
    if x:
        2
    else:
        3
)

foo = x =>
    do_stuff()
    42
```

## Optional commas in enumerations

Enumerations of anything split across multiple lines can omit commas.

```
my_list = [1, 2, 3]
my_list = [
    1
    2
    3
]

# Records replace dicts in Koatl - they behave similarly to Javascript objects
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
