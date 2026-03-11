# Containers

## Records

Records replace Python dicts with unquoted string keys, JS-style dot access, optional commas, and support for methods and properties. Wrap a key in `()` to compute it.

```koatl
x = {a: 1}
x["a"] == 1
x.a == 1

key = "my_key"
x = {(key): 1, key: 2}
x["my_key"] == 1
x["key"] == 2
```

Multiline records don't need commas — just ensure the `{` is the last character on the line so it opens a new block:

```koatl
x = {
    a: 1
    b: 2
    c: 3
}
```

Records can hold functions, methods, and properties (the `!` operator is a decorator shorthand — see [Operators](operators)):

```koatl
x = {
    a: 1
    get_global_value: () => 2
    get_own_a: Record.method! self => self.a
    own_a_prop: Record.property! self => self.a
}

x.get_global_value() == 2
x.get_own_a() == 1
x.own_a_prop == 1
```

## Lists

Lists behave like Python's, except that multiline lists don't need commas.

```koatl
x = [
    1
    2
    3
]
```

## Tuples

Tuples behave like Python's, but multiline parenthesized expressions are interpreted as blocks rather than tuples:

```koatl
x = 1, 2
x = (1, 2)
x = (1, 2,  # this is still a tuple -
     3, 4)  # this line is interpreted as a continuation of above

x = (       # this is not a tuple! this is a block-expression,
    1       # and x will get the value 3 which is the last expression in the block
    2
    3
)
```

## Sets

Koatl has no syntax for defining literal sets; use `set([...])`.
