# Intro

[Koatl](https://github.com/skkestrel/koatl) is a functional-first language transpiling to Python.

```koatl
>>> # Proper scoping and variable capture rules
>>> # |
>>> # |        Arrow syntax for functions (multiline lambdas also supported)
>>> # |        |
>>> # |        |        Easy pattern matching expressions
>>> # v        v        v
>>> let fib = x => x matches 0 | 1 then 1 else fib(x-1) + fib(x-2)

>>> # Painlessly define slices and ranges
>>> # |
>>> # |  Chain iterators directly
>>> # v     v
>>> (..10).map(fib).list()
[1, 1, 2, 3, 5, 8, 13, 21, 34, 55]

>>> # Convenient syntax for small lambdas
>>> #         |
>>> #         |  Catch exceptions into a Result with try-expressions
>>> #         v                v
>>> (..5).map($ - 1).map(x => try [1, 2, 3][x]).list()
[Err(IndexError(...)), Ok(1), Ok(2), Ok(3), Err(IndexError(...))]

>>> #      Quickly create records with Javascript-like syntax
>>> #                              |
>>> #                              |              Coalesce errors and Nones
>>> #                              v                           v
>>> ["key0", "key1"].map(x => try { key0: "Queried value" }[x] ?? "Default").list()
["Queried value", "Default"]

>>> # Memoize functions effortlessly
>>> #              v
>>> let fib = Memo.fn& x =>
>>>     if x < 2: 1
>>>     else: memo @fib(x - 1) + @fib(x - 2)
>>> #          ^   ^             ^
>>> #          |  use monadic bind to nest memo functions
>>> #          |
>>> # ...and memoize arbitrary expressions as well,
>>> #    with automatic dependency detection

>>> fib(500).run()
453973694165307953197296969697410619233826
```

> Note: Koatl is under heavy development. Features may be broken, added, or removed at any time.

## Quick Start

```bash
pip install koatl koatl-kernel
```

...and optionally install the `quetzal-koatl` extension on VSCode for rudimentary syntax highlighting.

Try this:

```koatl
# hello_world.tl

(..10).map("hello world" | print)
```

```bash
koatl hello_world.tl
```

## In IPython

With the `koatl-kernel` module, Koatl also supports Jupyter notebooks.
Select it as your kernel in Jupyter, or start an interactive session with `koatl` in the terminal.

Or, from an IPython kernel, run

```python
%load_ext koatl.notebook
```

to convert the current notebook to use Koatl instead.

## Using Koatl from Python

`.tl` files can be imported from Python and vice-versa, making integration with existing projects seamless.

```python
import koatl.runtime # enables importing .tl files
import hello_world
```
