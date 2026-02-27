# Operators

Koatl adds a number of operators that allow for much greater expressivity and convenience.

## Lambdas

Koatl has no need for `def` as all functions can be defined using `=>`:

```koatl
let f = (a, b, *args, **kwargs) =>
    other_func(a + 2, b / a, *args, **kwargs)
```

## Placeholder variables

The placeholder variable `$` allows constructing a lambda from any expression, making interfacing with external code extremely easy

```koatl
f(a, $, c)
# x => f(a, x, c)

($ + 2 * y / 4)
# x => x + 2 * y / 4
```

Rules:

1. A bare `$` as an argument to a function call, i.e. `fn(a, $, b)`, creates `x => fn(a, x, b)`.
2. Any other `$` turns its containing expression into a function, up to the nearest function call, i.e., `fn(a, $.value*3+2, b)` becomes `x => fn(a, x.value*3+2)`.
3. When in doubt, use an arrow function.

## Piping

`x | f` means `f(x)`, and can be chained, for intuitive piping syntax

```koatl
data
    | do_some_thing
    | do_some_other_thing
    | pass_into_second_arg(a, $, option="yes")
```

`x.(f)` means the same thing, but with higher precedence.

## Check-expressions

Check-expressions elegantly interface with the outside world without breaking the flow of a program with a try-catch block, instead returning exceptions as a regular value wrapped in Result:

```koatl
>>> check a
Err(NameError(...))
>>> check 1
Ok(1)
```

## If-expressions

Most statements, including ifs, can act as expressions:

```koatl
x =
    if True:
        1
    else:
        2
```

The `then` keyword can be used as an inline alternative to `:` in `if`/`elif`:

```koatl
x = if condition then 10 else 20

if a then do_something()
elif b then do_other_thing()
else:
    fallback()
```

> **Note**: `then` introduces a single inline expression — it cannot be followed by `:`. Use `:` for block bodies. Only valid in `if`/`elif`, not `while`/`for`.

## With-expressions

Similar to above, with also yields a value

```koatl
x = with f = open("my_file.txt", "r"):
    f.read()
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

`while let` loops while the pattern matches:

```koatl
while let ("Some", val) = data[idx]:
    process(val)
    idx = idx + 1
```

See [Pattern matching](match) for more details.

## Matches-expressions

Matches-expressions resolve to either True or False, using Python pattern matching (see [Pattern matching](match)).
With `matches`, patterns must be capture-free (use `if let` for captures):

```koatl
>>> x = [1, 2, 3]
>>> x matches [_, _, _]
True
```

This makes regex matching especially convenient with `if let`:

```koatl
>>> if let Ok([a, b]) = "(\\d+).(\\d+)".match("123.456"):
>>>    print(a, b)
123 456
```

If-not-let expressions can be used to conditionally destructure values with a guard:

```koatl
if not let str(x) = 123:
    # this block must be of bottom type, i.e., return, break, continue, or throw
    return None

# x is a string
x.join(["a", "b"])
```

## Coalescing operators

We can use coalescing operators to work with try-expressions and the Result monad.
They lazily evaluate the RHS default value on Err, None, and Exceptions.

```koatl
config_option = check get_config_value() ?? default_value
```

### Mapping operators

Mapping operators `?.`, `?()`, `?[]`, work as usual, on both Results and regular values.

```koatl
>>> None?.prop
None

>>> Ok([1, 2, 3])?[0]
Ok[1]

>>> Err(ValueError())?.prop
Err(ValueError())

>>> Ok(None)?.prop
<...raised AttributeError...>
```

## Better slices

Koatl uses `..` to represent slices, and they can occur outside lists too:

```koatl
up_to_three = [1, 2, 3, 4, 5][..3]

odds_only = [1, 2, 3, 4, 5][1..5..2]

my_saved_slice = ..5
some_other_array[my_saved_slice]
```

Slices implement the Iterable trait (which is distinct, but related to, `__iter__`), so they can be used as ranges like normal:

```koatl
for i in ..10:
    print(i)
```

## Primitive operators

Use `===` or `!==` instead of `is` and `is not`.

## Decorators

As a synonym for calling a one-argument function, the `!` operator can be used to attach decorators:

```koatl
Foo = class:
    do_something = staticmethod! () => ...
```

`a! b` is equivalent to `a(b)`.
