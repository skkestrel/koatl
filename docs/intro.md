# Intro

Koatl is a functional-first language transpiling to Python.

`.tl` files can be imported from Python and vice-versa, making integration with existing projects seamless.

Koatl also supports Jupyter notebooks out of the box.

```koatl
>>> fib = x => x match 0 | 1 => 1 default fib(x-1) + fib(x-2)
>>> (..10).map(fib).list()
[1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
```

> Note: Koatl is under heavy development. Features may be broken, added, or removed at any time.

## Quick Start

```
pip install koatl koatl-kernel
```

...and optionally install the `quetzal-koatl` extension on VSCode for rudimentary syntax highlighting.

Start an interactive console session with `koatl` in your terminal!
