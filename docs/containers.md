# Containers

## Records

Koatl uses `{}` to define Record literals, instead of Python's dict.

Records are a subclass of dict, with the difference that Record forwards attribute lookup to indexing - just like Javascript!

Also, string keys in Records don't need quotes, making it easy to construct Records. Wrap the key in round brackets `()` to stick an expression in instead.

```koatl
x = {a: 1}
x["a"] == 1
x.a == 1

key = "my_key"
x = {(key): 1, key: 2}
x["my_key"] == 1
x["key"] == 2
```

Multiline records don't need commas - just ensure that the `{` is the last character on the first line, to open a new block:

```koatl
x = {
    a: 1
    b: 2
    c: 3
}
```

Records can be imbued with 1) functions, 2) methods, and 3) properties:

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

Lists are the same as Python, except that multiline lists don't need commas either.

```koatl
x = [
    1
    2
    3
]
```

## Tuples

Tuples are the same as Python, except you can't have multiline tuples - those are interpreted as blocks instead.

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
