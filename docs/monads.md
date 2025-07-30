# Monads

Koatl uses `@` as a bind-operator to simulate some common and useful monads:

```koatl
f = () =>
    @x
    @y
```

becomes

```python
@__tl__.do
def f():
    yield x
    return (yield y)
```

Due to limitations of generators (they can't be copied), `@` specifically requires `bind_once(self, f)` instead of the usual `bind(self, f)`;
the difference is that `f` should called at most once in `bind_once(self, f)`.

## Result

Koatl defines a pseudo-class called Ok, which is used to check that a value is not None and not an error:

```koatl
None matches Ok() == False
ValueError() matches Ok() == False
1 matches Ok() == True
```

`Err` is the same as `BaseException`:

```koatl
ValueError() matches Err() == True
```

The Result monad represents error handling and early return - `Ok` is success, while `None` and `Err` types are failure.

> Note: unlike Rust's Result, `None` and `Err` are distinct error-ish types.

```koatl
f = () =>
    x = @get_some_value_or_none()
    y = @get_some_other_value_or_error(x)
    x + y

print(f())
```

can be thought of as the same as

```python
def f():
    x = get_some_value_or_none()
    if isinstance(x, (NoneType, BaseException)):
        return

    y = get_some_other_value_or_error(x)
    if isinstance(x, (NoneType, BaseException)):
        return y

    return x + y
```

or, perhaps more familiarly in Rust,

```rust
fn f() -> Result<T, E> {
    let x = get_some_value_or_none().ok_or_else(...)?;
    let y = get_some_other_value_or_error(x)?;
    Ok(x + y)
}
```

To explicitly mark an exception or None as an Ok value, simply use `Ok(error)`.

While errors typically aren't returned from functions in Python, the `try` operator (see [Operators](operators)) makes it very easy to use these constructions to interface with external code.

Note that Result provides the default `bind_once` implementation for types that don't otherwise have `bind_once`.

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

## Reader

The Reader monad allows interfacing with an external context object.
Instead of having to pass around a context object as an explicit parameter:

```koatl
g = ctx =>
    ctx["third_num"]

f = ctx =>
    ctx["first_num"] + ctx["second_num"] + g(ctx)

f(ctx)
```

We can use the Reader monad:

```koatl
{ ask } = Reader

g = () =>
    @ask("third_num")

f = () =>
    @ask("first_num") + @ask("second_num") + @g()

f().run(ctx)
```
