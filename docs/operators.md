# Operators

Koatl adds a number of operators that allow for much greater expressivity and convenience.

## Lambdas

Koatl has no `def` statement; they're completely replaced by the new `=>` syntax:

```
f = (a, b, *args, **kwargs) =>
    other_func(a + 2, b / a, *args, **kwargs)
```

## Placeholder variables

The placeholder variable `$` allows constructing a lambda from any expression, making interfacing with external code extremely easy

```
f(a, $, c)
# x => f(a, x, c)

($ + 2 * y / 4)
# x => x + 2 * y / 4
```

...the rules are a bit weird (to be detailed later), but they should "just work" in most situations.

## Piping

`x | f` means `f(x)`, and can be chained, which lets us understand complex transformations of data at a glance

```
data
    | do_some_thing
    | do_some_other_thing
    | pass_into_second_arg(a, $, option="yes")
```

`x.(f)` means the same thing, but with higher precedence.

## Try-expressions

Try-expressions elegantly interface with the outside world without breaking the flow of a program with a try-catch block, instead returning exceptions as a regular value:

```
>>> try a
NameError(...)
>>> try 1
1
```

## Coalescing operators

We can use coalescing operators to work with try-expressions.
They coalesce on None and objects deriving from Exception

```
config_option = try get_config_value() ?? default_value
other_option = list_or_none?[1] ?? default_value
optional_callback?.()
```

## Better slices

Koatl uses `..` to represent slices, and they can occur outside lists too:

```
up_to_three = [1, 2, 3, 4, 5][..3]

odds_only = [1, 2, 3, 4, 5][1....2]

my_saved_slice = ..5
some_other_array[my_saved_slice]
```
