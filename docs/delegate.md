# Argument Delegation

The `delegate` keyword copies argument names and defaults from another function's signature into your own. This reduces boilerplate when writing wrapper or forwarding functions.

## Basic Usage

`delegate` appears inside a parenthesized argument list, after `*` or `*args`:

```koatl
>>> let target = (*, x=10, y=20) => x + y

>>> let f = (a, *, delegate target(x)) => (a, x)
>>> f(1)
(1, 10)
>>> f(1, x=5)
(1, 5)

>>> let g = (*, delegate target(x, y)) => (x, y)
>>> g()
(10, 20)
>>> g(x=1, y=2)
(1, 2)
```

The delegated arguments become keyword-only parameters with defaults copied from the target at decoration time.

## Aliases

Rename a delegated argument with `as`:

```koatl
>>> let target = (*, x=10) => x

>>> let h = (*, delegate target(x as local_x)) => local_x
>>> h()
10
>>> h(local_x=99)
99
```

The caller uses the alias name (`local_x`), while the original name (`x`) is used to look up the default in the target's signature.

## Default Overrides

Override the target's default with `=`:

```koatl
>>> let target = (*, x=10) => x

>>> let j = (*, delegate target(x=42)) => x
>>> j()
42
```

Aliases and default overrides can be combined: `delegate target(x as local_x=42)`.

## `**kwargs` Spread

Use `**name` inside `delegate(...)` to collect the target's remaining keyword arguments into a dict:

```koatl
>>> let target = (*, a=1, b=2, c=3) => a + b + c

>>> let m = (*, delegate target(a, **kw)) => (a, kw)
>>> m()
(1, {'b': 2, 'c': 3})
>>> m(a=10, b=20)
(10, {'b': 20, 'c': 3})
```

The extra args (`b`, `c`) are expanded into the wrapper's signature so callers get autocomplete and type checking. At call time they are collected into the `kw` dict.

### Catch-All Behavior

If the target itself accepts `**kwargs`, the delegate's `**kwargs` acts as a catch-all — absorbing unknown keyword arguments too:

```koatl
>>> let target = (a=3, **kwargs) => (a, kwargs)

>>> let t = (*, delegate target(a, **kw)) => (a, kw)
>>> t(a=5, unknown=99)
(5, {'unknown': 99})
```

## Dotted Targets

The delegate target can be any expression (not just an identifier) — attribute access, subscripts, etc.:

```koatl
>>> let ns = {func: (*, x=10, y=20) => x + y}

>>> let f = (*, delegate ns.func(x, y)) => (x, y)
>>> f()
(10, 20)
```

## Multiple Delegates

A single function can delegate from multiple targets:

```koatl
>>> let target_a = (*, p=1, q=2) => p + q
>>> let target_b = (*, r=3, s=4) => r + s

>>> let n = (*, delegate target_a(p, q), delegate target_b(r, s)) => (p, q, r, s)
>>> n()
(1, 2, 3, 4)
```

## Forwarding Pattern

A common use case is forwarding delegated args to the target:

```koatl
>>> let base = (*, x=1, y=2, z=3) => x + y + z

>>> let wrapper = (*, delegate base(x, **kw)) =>
...     base(x=x, **kw)
>>> wrapper()
6
>>> wrapper(x=10, y=20)
33
```

This also works with chained delegation (delegate-to-delegate).

## Contextual Keyword

`delegate` is a contextual keyword — it only has special meaning inside argument lists. You can use it as a regular identifier elsewhere:

```koatl
>>> let delegate = 42
>>> delegate
42
```

## Rules

- `delegate` must appear after `*` or `*args` (delegated args are always keyword-only).
- Only one `**kwargs` is allowed per delegate.
- No arguments are allowed after `**kwargs` within a single delegate.
- Multiple `delegate` clauses are allowed in the same argument list.
- `delegate` can be mixed with regular keyword-only args, `**kwargs`, etc.
