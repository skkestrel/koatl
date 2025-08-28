# Pattern matching

## Pattern matching in for loops and function arguments

Koatl allows for incredible flexibility in function arguments and for-loops by automatically matching complex constructions:

```koatl
f = [x, [y, z]] => x + y + z

f([1, [2, 3]]) == 6

f(1) # raises MatchError


# this looks strange, but just ensures that the first argument matches the constant 1
f = 1 => 1

f(1) == 1
f("asdf") # MatchError
```

```koatl
for x, y in [[1, 2]]:
    print(x, y)

for {1: x} in [1]: # MatchError
    print(x)

# rudimentary argument validation by matching the type `str`
for str(x) in ["x", "y", "z"]:
    print(x)
```

## Matching against local values

Koatl uses the same rules as Python pattern matching, with one improvement: matching against unqualified values.

In Python, this won't work:

```python
y = 2
match x:
    case y:
        # this treats y as a capture
        # instead of a value to match against!
```

This is the correct way:

```python
ns = SimpleNamespace(y=2)
match x:
    case ns.y:
        # this matches correctly
```

In Koatl, matching against _captures_ versus _values_ is distinguished by a leading `.`:

```koatl
y = 2
x match:
    .y => print("matched the constant y")
    .module.value => print("matched a module constant")
    y => print("capture any value to a new variable y")
```

Like if-expressions, matches are also expressions in Koatl, and introduces the `default` keyword that is a bit more readable than `_ =>`:

```koatl
result = x match:
    [_] => True
    default: False

result = x match [_] => True default False
```

## Try-catch and check-expressions with pattern matching exceptions

Koatl unifies exception blocks with the same syntax as pattern matching:

```koatl
try:
    x
except NameError() as x =>
    ()

try:
    do_something()
except CustomException(msg=msg) | OtherException(msg=msg) =>
    print(msg)
```

Check-expressions (see [Operators](operators)) also have an optional match-filter to limit the types of exceptions caught:

```koatl
x = check a except NameError() # caught
y = check a except ValueError() # exception will be raised!
```

## Matches-expressions and guards

Matches-expressions resolve to either True or False depending on whether the value matches the provided pattern,
and the bound names are available in the following block:

```koatl
>>> x = [1, 2, 3]
>>> if x matches [a, *b]:
>>>    print(a, b)
1 [2, 3]
```

`not matches` is the inverse; and bound names are available afterwards, but only if the type of the next block is Never (i.e., it raises, returns, breaks, or continues at the end)

```koatl
>>> if [1, 2] not matches [x, y]:
>>>     raise
>>> print(x, y)
1 2
```

These restrictions can be avoided by making sure to not bind any names in the pattern:

```koatl
>>> x = [1, 2] not matches [_, _]
```
