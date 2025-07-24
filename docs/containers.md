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

As mentioned before, multiline records don't need commas - just remember that the `{` has to be the last character on the first line:

```koatl
x = {
    a: 1
    b: 2
    c: 3
}
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

## Better destructuring

Koatl allows you to destructure both lists and Records (which Python unfortunately doesn't).

```koatl
[a, b] = [1, 2]

{1: a, 2: b} = {1: 1, 2: 2}

{a, b, c} = {a: "1", b: "2", c: "3"}

# and as much nesting as you want

[a, {b, c}] = [42, {b: "b", c: "c"}]
```
