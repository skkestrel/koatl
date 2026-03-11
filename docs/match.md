# Pattern Matching

Pattern matching is available everywhere in Koatl: function arguments, for loops, if/while conditions, and standalone match expressions.

## In function arguments and for loops

Function arguments and for-loop variables support full pattern matching:

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

Python's `match` can't match against a plain variable — you need a namespace workaround. Koatl uses a leading `.` to distinguish captures from value matches:

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

## Try-catch with pattern matching

Exception handlers use the same pattern syntax:

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

`if let` enters a block only when a destructuring pattern matches. `if not let` is the inverse — the block must diverge (return/raise/break/continue), and captures leak to the surrounding scope:

```koatl
>>> x = [1, 2, 3]
>>> if let [a, *b] = x:
>>>    print(a, b)
1 [2, 3]
```

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
