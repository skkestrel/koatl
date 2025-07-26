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

## Ok

Koatl defines a pseudo-class called Ok, which is used to check that a value is not None and not an error:

```koatl
isinstance(None, Ok) == False
# same as
None matches Ok() == False

ValueError() matches Ok() == False
1 matches Ok() == True
```

Ok is the default monad that all types are coerced to:

```koatl
f = () =>
    x = @get_some_value_or_none()
    y = @get_some_other_value_or_error(x)
    x + y

print(f())
```

The essence is that the function will immediately return whenever @ "sees" a value that is not Ok, but continue otherwise.
It's the same as:

```python
def f():
    x = get_some_value_or_none()
    if x is None:
        return

    y = get_some_other_value_or_error(x)
    if isinstance(y, BaseException):
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

While errors typically aren't returned from functions in Python, the `try` operator (see [Operators](operators)) makes it very easy to use these constructions to interface with external code.

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
>>> # or interfacing with Python code, do this instead:
>>> await f()
```

## Reader

The Reader monad allows easy passing around with, or interfacing with an external context object.
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
