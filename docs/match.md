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

Like if-expressions, matches are also expressions in Koatl:

```koatl
result = x match:
    [_] => True
    _ => False
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

## If Let and While Let

`if let` destructures a value with pattern matching, entering the then-block only if the pattern matches.
Captured variables are scoped to the then-block:

```koatl
>>> x = [1, 2, 3]
>>> if let [a, *b] = x:
>>>    print(a, b)
1 [2, 3]
```

`if not let` is the inverse; captured variables leak to the surrounding scope, but the then-block
must be of Never type (i.e., it raises, returns, breaks, or continues at the end):

```koatl
>>> if not let [x, y] = [1, 2]:
>>>     raise
>>> print(x, y)
1 2
```

### While Let

`while let` loops while the pattern continues to match:

```koatl
data = [("Some", 1), ("Some", 2), ("Some", 3), ("None", 0)]
idx = 0
while let ("Some", val) = data[idx]:
    print(val)
    idx = idx + 1
# prints 1, 2, 3
```

### Matches operator

The `matches` / `not matches` operator returns a boolean that checks for a pattern match without captures:

```koatl
>>> x = [1, 2, 3]
>>> x matches [_, _, _]
True
```

Using captures in `matches` is a compile error — use `if let` instead:

```koatl
# Error: use 'if let' for patterns with captures
if x matches [a, b]:
    ...

# Correct:
if let [a, b] = x:
    print(a, b)
```
