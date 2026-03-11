# Monads (experimental)

Koatl uses `@` as a bind operator to chain monadic computations. Four monads are built in: **Memo**, **Result**, **Async**, and **Env**.

Inside a function, `@expr` yields-and-binds the monadic value, producing flat, sequential-looking code for inherently nested operations:

```koatl
f = () =>
    x = @get_value()     # unwraps or short-circuits
    y = @process(x)
    x + y
```

Due to limitations of generators (they cannot be copied), `@` specifically requires `bind_once(self, f)` instead of the usual `bind(self, f)` — the difference is that `f` should be called at most once. This represents a deterministic monad, ruling out the List monad.

## Memo

The Memo monad allows computations to be cached, together with the `memo` keyword.

```koatl
f = () =>
    let a = memo 1 + 2
    let b = memo a * 2
    let c = memo:
        let temp = 2
        a * b * 2 + temp
    a + b + c

f().run(Memo.Cache()) # or f().run()

g = Memo.fn! x =>
    2 + 2

g().run()
```

A `Memo` instance can be constructed using `Memo.fn(function_to_memoize)` or `Memo.value(unique_id, dependencies, function)`. The `memo` keyword automatically constructs `@Memo.value(id, deps, fn)`, where `deps` is inferred from variables _directly_ captured by the following expression (excluding global captures and captures by inner nested functions).

## Result

The Result type has two subtypes, `Ok` and `Err`. The `Result` constructor maps a value to one of them automatically:

```koatl
>>> Result(1)
Ok(1)
>>> Result(0)
Ok(0)
>>> Result(None)
Err(None)
>>> Result(ValueError())
Err(ValueError())
```

The Result monad represents error handling with early return:

```koatl
f = () =>
    x = @get_some_value()
    y = @get_some_other_value(x)
    x + y

print(f())
```

is similar to

```python
def f():
    x = get_some_value()
    if isinstance(x, (NoneType, BaseException)):
        return x

    y = get_some_other_value(x)
    if isinstance(x, (NoneType, BaseException)):
        return y

    return x + y
```

or perhaps more familiarly in Rust:

```rust
fn f() -> Result<T, E> {
    let x = get_some_value_or_none().ok_or_else(...)?;
    let y = get_some_other_value_or_error(x)?;
    Ok(x + y)
}
```

To explicitly mark an exception or None as an Ok value, simply use `Ok(None)`.

While errors aren't typically returned from functions in Python, the `check` operator (see [Operators](operators)) makes it straightforward to interface with external code using these constructions.

Result provides a default `bind_once` implementation for ALL types that don't otherwise define it, which means `(1).bind_once(...)` works — and therefore the `@` operator also works with bare non-Result values using Result semantics.

```koatl
external_function().(Result) match:
    Ok(value) => ...
    Err(value) => ...
```

## Async

```koatl
f = () =>
    print("sleepy")
    @Async.sleep(1)
    print("refreshed!")

>>> f()
Async(...)
>>> f().run()   # creates a new event loop
sleepy
refreshed!

>>> # Async instances can be awaited,
>>> # so if inside a notebook,
>>> # or if using Async from regular Python, do this instead:
>>> await f()
```

## Env

The Env monad provides access to an external context object, replacing the need to thread a parameter through every function. Instead of:

```koatl
g = ctx =>
    ctx["third_num"]

f = ctx =>
    ctx["first_num"] + ctx["second_num"] + g(ctx)

f(ctx)
```

you can use the Env monad:

```koatl
g = () =>
    @Env.item("third_num")

f = () =>
    @Env.item("first_num") + @Env.item("second_num") + @g()

f().run(ctx)
```
