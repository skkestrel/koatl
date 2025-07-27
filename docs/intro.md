# Intro

[Koatl](https://github.com/skkestrel/koatl) is a functional-first language transpiling to Python.

```koatl
>>> #       Arrow syntax for functions (multiline lambdas also supported)
>>> #       |
>>> #       |        Easy pattern matching expressions
>>> #       v        v
>>> fib = x => x matches (0 | 1) then 1 else fib(x-1) + fib(x-2)

>>> # Painlessly define slices and ranges
>>> # |
>>> # |     Iteration as a first-class abstraction
>>> # v     v
>>> (..10).map(fib).list()
[1, 1, 2, 3, 5, 8, 13, 21, 34, 55]

>>> #         Convenient syntax for defining small functions
>>> #         |
>>> #         |                Get errors as values instead of
>>> #         v                v      guarding with try/except
>>> (..5).map($ - 1).map(x => try [1, 2, 3][x]).list()
[IndexError(...), 1, 2, 3, IndexError(...)]

>>> #  Quickly create records with Javascript-like syntax
>>> #                              |
>>> #                              |   Coalesce errors and Nones
>>> #                              v                           v
>>> ["key0", "key1"].map(x => try { key0: "Queried value" }[x] ?? "Default").list()
["Queried value", "Default"]
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
