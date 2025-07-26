# Pattern matching

## Pattern matching in for loops and function arguments

Koatl allows for incredible flexibility in function arguments and for-loops by automatically matching complex constructions:

```koatl
f = [x, [y, z]] => x + y + z

f([1, [2, 3]]) == 6

f(1) # raises MatchError
```

```koatl
for x, y in [[1, 2]]:
    print(x, y)

# raises MatchError
for {1: x} in [1]:
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

Koatl introduces a shorthand syntax for matching values:

```koatl
y = 2
match x: # or "x match:"
    .y => print("matched")
    y => print("fallback")
```

Like if-expressions, matches are also expressions in Koatl, and introduces the `default:` keyword that is "longhand" for `_ =>`:

```koatl
result = x match:
    [_ as item] => True
    default: False

result = x match [_] => True default False
```

## Try-catch and try-expressions with pattern matching exceptions

Koatl unifies exception blocks with the same syntax as pattern matching:

```koatl
try:
    x
except NameError() as x:
    ()

try:
    do_something()
except CustomException(msg=msg) | OtherException(msg=msg):
    print(msg)
```

Try-expressions (see [Operators](operators)) also have an optional match-filter to limit the types of exceptions caught:

```koatl
x = try a except NameError() # caught
y = try a except ValueError() # exception will be raised!
```

## Matches-expressions

Matches-expressions resolve to either True or False depending on whether the value matches the provided pattern:

```koatl
>>> x = [1, 2, 3]
>>> if x matches [a, *b]:
>>>    print(a, b)
1 [2, 3]
```

`matches not` is just the inverse of `matches`.

```koatl
>>> 1 matches 1
True
>>> 1 matches not 1
False
```

```koatl
>>> [1, 2, 3] matches not [x, y]
True
```
