# Modules

## Imports

Koatl unifies Python's `import` and `from ... import` syntax into a single statement:

```koatl
import a.b.c.d
# equivalent to Python's:
# from a.b.c import d

import a.b.c.(d, e, f)
# equivalent to Python's:
# from a.b.c import d, e, f

import a.b.c.*
# equivalent to Python's:
# from a.b.c import *

import a
# equivalent to Python's:
# import a

import a.b.c.d
import a
# equivalent to Python's:
# import a.b.c.d
```

## Exports

Koatl slightly improves upon Python modules by automatically setting `__all__` based on _exported values_:

```koatl
# module.tl

export import other_module.(item, other_item)
export a = 2
b = 3

# elsewhere

module.__all__ == ("item", "other_item", "a")
```
