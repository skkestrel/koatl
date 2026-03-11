# Prelude & Runtime

The prelude is auto-imported before every Koatl script and provides common global names.

## std and mod

`std` is a lazily-loaded proxy for `koatl.std`, giving you access to the standard library without explicit imports:

```koatl
std.io.read_file("data.txt") | print
```

`mod` does the same for any installed Python package — use it inline without an import statement:

```koatl
mod.numpy.array([1, 2, 3]) + 2 | print
```

## Runtime

The runtime is also auto-imported, but is much more barebones — its purpose is to provide the basic machinery that Koatl features depend on. Importing the runtime from a Python script enables `.tl` files to be imported in that script.
