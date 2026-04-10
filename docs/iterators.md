# Iterators

Koatl separates iteration into two traits:

- **`Iterable`** — anything with an `.iter` property (lists, sets, tuples, dicts, ranges, strings). Provides `.traverse()` and `.debug_iter()`.
- **`Iterator`** — anything with `__next__` (the result of calling `.iter`, or any Python iterator). Provides the full method suite: `map`, `filter`, `fold`, `sum`, etc.

The split means you always know whether an operation is **eager** (returns a concrete container) or **lazy** (returns an iterator you consume once).

## Eager vs. lazy

Concrete containers have eager `.map()` and `.filter()` overrides that return their own type:

```koatl
[1, 2, 3].map($ * 2)              # [2, 4, 6]  — list in, list out
[1, 2, 3].filter($ > 1)           # [2, 3]
set([1, 2, 3]).map($ * 2)         # {2, 4, 6}  — set in, set out
(1, 2, 3).map($ * 2)              # (2, 4, 6)  — tuple in, tuple out
{a: 1, b: 2}.map([k, v] => [v, k])  # {1: "a", 2: "b"}  — dict in, dict out
```

For anything else — aggregations, slicing, chaining, reordering — call `.iter` first to enter the lazy Iterator pipeline:

```koatl
[1, 2, 3, 4, 5].iter.sum()                    # 15
[3, 1, 4, 1, 5].iter.sorted()                 # [1, 1, 3, 4, 5]
[1, 2, 3].iter.flat_map(x => [x, x * 10])     # lazy iterator
(..100).iter.filter($ % 7 == 0).take(5).list()   # [0, 7, 14, 21, 28]
```

Ranges always require `.iter`:

```koatl
(..10).iter.map($ ** 2).list()     # [0, 1, 4, 9, ..., 81]
(1..100).iter.sum()                # 4950
```

### Return types

Most lazy Iterator methods return iterators, so they chain freely. A few collect into concrete types:

| Returns iterator                           | Returns concrete                           |
| ------------------------------------------ | ------------------------------------------ |
| `map`, `filter`, `flat_map`, `filter_map`  | `sorted` → list                            |
| `take`, `skip`, `take_while`, `skip_while` | `reversed` → list_reverseiterator          |
| `chain`, `zip`, `enumerate`, `product`     | `list`, `set`, `tuple`, `dict`, `record`   |
| `cycle`, `unique`                          | `associate`, `group_by`, `count_by` → dict |

When a method returns a concrete type, you may need `.iter` again to continue chaining Iterator methods:

```koatl
# .sorted() returns a list, so use slicing or .iter to continue
names.iter.sorted()[..5]
names.iter.sorted().iter.for_each(print)
```

## Iterable methods

These are available on any type with `.iter` (lists, dicts, ranges, etc.) without calling `.iter` first:

### `.traverse(f)`

Applies an applicative function to each element. Short-circuits for `Result` and gathers for `Async`:

```koatl
[1, 2, 3].traverse(x => Ok(x + 1))   # [Ok(2), Ok(3), Ok(4)]
[1, 2, -1].traverse(validate)         # Err(...) on first failure
```

### `.debug_iter(**kwargs)`

Wraps the iterable in a `DebugIterator` for pipeline visualization. See [Debugging pipelines](#debugging-pipelines) below.

## Iterator method reference

All methods below are available on iterators (the result of `.iter`, or any lazy pipeline step).

### Transformations

| Method                    | Description                                                                                |
| ------------------------- | ------------------------------------------------------------------------------------------ |
| `.map(f)`                 | Apply `f` to each element                                                                  |
| `.filter(f)`              | Keep elements where `f` returns true                                                       |
| `.filter_map(f?)`         | Map then filter out `Err` results. Without `f`, filters `Ok` values from a Result iterator |
| `.flat_map(f?)`           | Map then flatten one level. Without `f`, just flattens                                     |
| `.enumerate(start=0)`     | Yield `(index, value)` pairs                                                               |
| `.zip(*others)`           | Pair elements with other iterables (shortest wins)                                         |
| `.chain(*others)`         | Concatenate with other iterables                                                           |
| `.product(*others)`       | Cartesian product                                                                          |
| `.cycle()`                | Repeat infinitely                                                                          |
| `.unique()`               | Deduplicate (preserving order)                                                             |
| `.reversed()`             | Reverse (materializes into a list first)                                                   |
| `.sorted(key?, reverse?)` | Sort (materializes into a list)                                                            |

### Slicing

| Method           | Description                                 |
| ---------------- | ------------------------------------------- |
| `.take(n)`       | First `n` elements                          |
| `.skip(n)`       | Drop first `n` elements                     |
| `.take_while(f)` | Take while `f` is true, then stop           |
| `.skip_while(f)` | Drop while `f` is true, then yield the rest |

### Aggregations

These consume the iterator and return a single value.

| Method           | Description                                         |
| ---------------- | --------------------------------------------------- |
| `.fold(init, f)` | Reduce with initial value                           |
| `.sum()`         | Sum of elements                                     |
| `.mean()`        | Arithmetic mean                                     |
| `.min(key?)`     | Minimum element                                     |
| `.max(key?)`     | Maximum element                                     |
| `.tally(f?)`     | Count elements (optionally only those matching `f`) |
| `.join(sep="")`  | Join as string                                      |
| `.all(f)`        | True if `f` holds for every element                 |
| `.any(f)`        | True if `f` holds for at least one element          |
| `.find(f)`       | `Ok(first match)` or `Err()`                        |
| `.first()`       | `Ok(first element)` or `Err()`                      |
| `.last(f)`       | Last element matching `f`                           |
| `.at(index)`     | Element at position                                 |
| `.for_each(f)`   | Execute `f` on each element (for side effects)      |

### Collectors

These consume the iterator and return a container.

| Method          | Description                                          |
| --------------- | ---------------------------------------------------- |
| `.list()`       | Collect into a list                                  |
| `.set()`        | Collect into a set                                   |
| `.tuple()`      | Collect into a tuple                                 |
| `.dict()`       | Collect into a dict (expects `[key, value]` pairs)   |
| `.record()`     | Collect into a Record (expects `[key, value]` pairs) |
| `.associate(f)` | Build a dict mapping each element to `f(element)`    |
| `.group_by(f)`  | Group into a dict by key function                    |
| `.count_by(f?)` | Count occurrences by key function                    |

## Dict-specific extensions

Dicts have additional eager methods beyond `map` and `filter`:

```koatl
{a: 1, b: 2}.map_values($ * 10)      # {a: 10, b: 20}
{a: 1, b: 2}.map_keys($.upper())     # {A: 1, B: 2}
{a: 1, b: 2}.filter_keys($ != "a")   # {b: 2}
{a: 1, b: 2}.filter_values($ > 1)    # {b: 2}
```

## Debugging pipelines

`.debug_iter()` wraps any iterable in a `DebugIterator` that tracks every value as it flows through a chain of transformations, then renders an ASCII grid showing what happened at each stage.

```koatl
[1, 2, 3, 4, 5]
    .debug_iter()
    .map($ * 2)
    .filter($ > 4)
    .show()
```

```
DebugIterator (5 consumed, 3 stages)
┌──────┬──────────┬──────────┬──────────┐
│      │  source  │   map    │  filter  │
├──────┼──────────┼──────────┼──────────┤
│    0 │     1    │     2    │  [skip]  │
│    1 │     2    │     4    │  [skip]  │
│    2 │     3    │     6    │     6    │
│    3 │     4    │     8    │     8    │
│    4 │     5    │    10    │    10    │
└──────┴──────────┴──────────┴──────────┘
```

Every row is one value from the source. Columns show its state at each pipeline stage. `[skip]` means the value was rejected by a filter.

### Invocation

`.debug_iter()` is available on any `Iterable` — lists, ranges, dicts, strings:

```koatl
[1, 2, 3].debug_iter()
(..10).debug_iter()
{a: 1, b: 2}.debug_iter()
```

| Argument  | Default | Description                                             |
| --------- | ------- | ------------------------------------------------------- |
| `checked` | `False` | Error handling mode (see [Checked mode](#checked-mode)) |
| `history` | `5`     | Max rows to retain. `None` for unbounded.               |

### Using the pipeline

`DebugIterator` is a normal iterator — chain it like any other. All Iterator methods listed above are tracked in the grid:

```koatl
let di = (..20).debug_iter(history=10)
    .map($ ** 2)
    .filter($ % 2 == 0)
    .take(5)

let results = list(di)     # [0, 4, 16, 36, 64]
di.show()                   # prints the grid
```

### `.tap()` for side-effect inspection

`.tap(f)` runs a function on each value without altering the pipeline. The result is recorded in the grid as a column with dashed borders:

```koatl
(..10).debug_iter(history=None)
    .map($ * 10)
    .tap($ > 50).label("big?")
    .filter($ > 30)
    .show()
```

### `.label(name)`

Renames the most recent stage in the grid:

```koatl
.map($ * 2).label("double")
```

### History

By default only the last 5 consumed values are kept:

```koatl
# Keep all rows
(..100).debug_iter(history=None).map($ * 2).show()

# Keep last 20
(..100).debug_iter(history=20).map($ * 2).show()
```

When older rows are evicted, the grid shows a `...` row at the top. The total count is always tracked:

```koatl
let di = (..1000).debug_iter(history=3).map($ + 1)
list(di)
print(di.consumed_count)   # 1000
print(len(di.history))     # 3
```

### Programmatic access

After consuming, inspect without rendering:

```koatl
let di = [10, 20, 30].debug_iter(history=None).map($ + 1)
list(di)

di.stages           # ["source", "map"]
di.consumed_count   # 3
di.history          # list of DebugIteratorValue objects
di.format_grid()    # returns grid as a string
```

Each `DebugIteratorValue` has `.value`, `.is_skipped`, and `.lineage` (a list of `(value, stage_index)` tuples showing the full transformation path).

### Error handling

**Unchecked (default):** when a transform raises, the grid is printed and `DebugIteratorError` is raised with the grid and original exception attached:

```koatl
import koatl.std.debug_iter.DebugIteratorError

try:
    list([1, 0, 3].debug_iter(history=None).map(x => 10 / x))
except DebugIteratorError(cause=cause) =>
    print(f"Failed at: {cause}")
```

**Checked mode:** pass `checked=True` to catch errors and continue. Failed values appear as `[ERR: ...]` in the grid:

```koatl
let di = [2, 0, 3].debug_iter(checked=True, history=None)
    .map(x => 10 / x)

list(di)    # [5.0, 3.333...]
di.show()   # grid shows ZeroDivisionError for value 0
```

### Grid cell reference

| Cell         | Meaning                                      |
| ------------ | -------------------------------------------- |
| `42`         | Actual value (truncated to 20 chars)         |
| `[skip]`     | Rejected by filter, take, skip, etc.         |
| `---`        | Value didn't reach this stage                |
| `...`        | Repeated parent (flat_map/product children)  |
| `[?]`        | Unknown provenance (from a chained iterable) |
| `[ERR: ...]` | Exception (checked mode)                     |

### Flat map output

`flat_map` and `product` produce child values with hierarchical indices. The grid renders parent values once, with `...` for children:

```koatl
[1, 2].debug_iter(history=None)
    .flat_map(x => [x, x * 10])
    .show()
```

```
DebugIterator (4 consumed, 2 stages)
┌──────────┬──────────┬──────────┐
│          │  source  │ flat_map │
├──────────┼──────────┼──────────┤
│      0.0 │     1    │     1    │
│      0.1 │    ...   │    10    │
│      1.0 │     2    │     2    │
│      1.1 │    ...   │    20    │
└──────────┴──────────┴──────────┘
```
