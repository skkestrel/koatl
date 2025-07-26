# Intro

[Koatl](https://github.com/skkestrel/koatl) is a functional-first language transpiling to Python.

`.tl` files can be imported from Python and vice-versa, making integration with existing projects seamless.

Koatl also supports Jupyter notebooks out of the box.

```koatl
>>> fib = x => x matches (0 | 1) then 1 else fib(x-1) + fib(x-2)
>>> (..10)!map(fib)!list()
[1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
```

> Note: Koatl is under heavy development. Features may be broken, added, or removed at any time.

## Quick Start

```bash
pip install koatl koatl-kernel
```

...and optionally install the `quetzal-koatl` extension on VSCode for rudimentary syntax highlighting.

Start an interactive console session with `koatl` in your terminal!

## Using Koatl from Python

From any Python file, run:

```python
import koatl.runtime
```

Now, we can import Koatl `.tl` files just like regular Python modules.

## Viewing transpiled output

```koatl
import ast
import koatl.transpile

my_tl_code = """
problematic(koatl(code))
"""

# prints out the transpiled Python code
my_tl_code | transpile | ast.unparse | print

# prints out the transpiled Python AST
my_tl_code | transpile | ast.dump($, indent=4) | print
```
