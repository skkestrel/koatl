# Operators & Expressions

Quick reference for Koatl's operators. See the [Introduction](intro) for motivated examples.

## Lambdas

Koatl replaces `def` with `=>` for all function definitions:

```koatl
let f = (a, b, *args, **kwargs) =>
    other_func(a + 2, b / a, *args, **kwargs)
```

## Placeholder variables

The placeholder `$` constructs a lambda from any expression, making interfacing with external code straightforward:

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

`x |> f` means `f(x)` and can be chained for intuitive piping syntax:

```koatl
data
    |> do_some_thing
    |> do_some_other_thing
    |> pass_into_second_arg(a, $, option="yes")
```

## Method pipe

`x->f(args)` means `f(x, args)` — it pipes `x` in as the **first** argument of a call. This is useful when the function you're calling is not a method on `x`, or when you want to chain free functions in a readable left-to-right sequence:

```koatl
data->process(opts)          # process(data, opts)
data->clean()->transform()   # transform(clean(data))
```

The right-hand side can be any of:

| Form                 | Meaning                               |
| -------------------- | ------------------------------------- |
| `x->f(args)`         | `f(x, args)`                          |
| `x->f.g(args)`       | `f.g(x, args)`                        |
| `x->(expr)(args)`    | `expr(x, args)`                       |
| `x->f` _(no parens)_ | `partial(f, x)` — partial application |

`x?->f(args)` is the optional variant: if `x` is `Ok(v)`, applies `f(v, args)` and wraps the result in `Ok`; if `x` is `Err` or `None`, passes through unchanged.

```koatl
(check parse(raw))?->process(opts)     # Ok(process(parsed, opts)) or Err
(check open(path))?->transform()       # short-circuits on file error
```

The `.()` form also works for piping inline lambdas where `->` would be awkward:

```koatl
results.(x => x * 2)       # (x => x * 2)(results)
```

## Check-expressions

Check-expressions interface with the outside world without breaking program flow — instead of a try-catch block, exceptions are returned as a regular `Result` value:

```koatl
>>> check a
Err(NameError(...))
>>> check 1
Ok(1)
```

## If-expressions

Most statements, including `if`, can act as expressions:

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

`with` also yields a value:

```koatl
x = with f = open("my_file.txt", "r"):
    f.read()
```

## If Let and While Let

`if let` destructures a value with pattern matching, entering the then-block only if the pattern matches. Captured variables are scoped to the then-block:

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

The `matches` / `not matches` operator returns a boolean check against a pattern (see [Pattern matching](match)). Patterns must be capture-free — use `if let` for captures:

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

`if not let` can be used to conditionally destructure values with a guard:

```koatl
if not let str(x) = 123:
    # this block must be of bottom type, i.e., return, break, continue, or throw
    return None

# x is a string
x.join(["a", "b"])
```

## Coalescing operators

Coalescing operators work with try-expressions and the Result monad, lazily evaluating the right-hand default on `Err`, `None`, and exceptions:

```koatl
config_option = check get_config_value() ?? default_value
```

### Mapping operators

The mapping operators `?.`, `?()`, `?[]`, and `?->` work on both `Result` values and regular values:

```koatl
>>> None?.prop
None

>>> Ok([1, 2, 3])?[0]
Ok(1)

>>> Err(ValueError())?.prop
Err(ValueError())

>>> Ok(None)?.prop
<...raised AttributeError...>

>>> Ok(data)?->process(opts)
Ok(process(data, opts))

>>> Err(e)?->process(opts)
Err(e)

>>> None?->process(opts)
None
```

## Better slices

Koatl uses `..` to represent slices, and they can occur outside lists too:

```koatl
up_to_three = [1, 2, 3, 4, 5][..3]

odds_only = [1, 2, 3, 4, 5][1..5..2]

my_saved_slice = ..5
some_other_array[my_saved_slice]
```

Slices implement the `Iterable` trait (which is distinct from, but related to, `__iter__`), so they can be used as ranges. Call `.iter` to enter the `Iterator` pipeline:

```koatl
for i in ..10:
    print(i)
```

## Primitive operators

Use `===` or `!==` instead of `is` and `is not`.

## Decorators

The `!` operator calls a one-argument function, serving as a compact decorator syntax:

```koatl
Foo = class:
    do_something = staticmethod! () => ...
```

`a! b` is equivalent to `a(b)`. See also [Extension methods](extensions) for how this is used with traits and `Record.method`.
