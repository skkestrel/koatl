# Modules & Imports

## Imports

Koatl unifies `import` and `from ... import` into dot-separated paths. Use `()` to import multiple names from a module, `*` for wildcard, and `.` prefix for relative imports.

```koatl
import a.b.c.d
# from a.b.c import d

import a.b.c.(d, e, f)
# from a.b.c import d, e, f

import a.b.c.*
# from a.b.c import *
```

Inside a `(...)` group, each entry is resolved relative to the preceding path:

- A plain name `d` → import `d` from the current prefix
- A dotted path `f.g.(i, j)` → extend the prefix by `f.g`, then import `i` and `j`
- A `.` prefix pops one level: `.e` inside `a.b.c.(...)` → `from a.b import e`
- A bare `.` imports the prefix module itself: `.` inside `a.b.c.(...)` → `from a.b import c`

```koatl
import a.b.c.(
    d           # from a.b.c import d
    f.g.(i, j)  # from a.b.c.f.g import i, j
    .e          # from a.b import e
    .           # from a.b import c
)
```

## Exports

Koatl auto-populates `__all__` from `export` declarations:

```koatl
# module.tl

export import other_module.(item, other_item)
export a = 2
b = 3

# elsewhere

module.__all__ == ("item", "other_item", "a")
```
