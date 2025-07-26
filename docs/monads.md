# Generators in Koatl

Koatl uses generators to simulate some common and useful monads.

Python's `yield` keyword is represented by the prefix `@` operator in Koatl:

```koatl
f = () =>
    @1
    @2
    @3

f() | list == [1, 2, 3]
```

## Ok

Koatl defines a pseudo-class called Ok, which is used to check that a value is not None and not an error:

```koatl
Ok(None) == False
Ok(ValueError()) == False
Ok(1) == True
```

To use the Ok monad, wrap a generator function in `Ok.do`:

```koatl
f = Ok.do(() =>
    x = @get_some_value_or_none()
    y = @get_some_other_value_or_error(x)
    x + y
)

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

Koatl uses the same syntax and mechanisms to write async code, where `@` can be thought of as `await`:

```koatl
f = Async.do(() =>
    print("sleepy")
    @Async.sleep(1)
    print("refreshed!")
)

>>> f()
Async(...)
>>> f().run()
sleepy
refreshed!
```

This is equivalent to the Python code:

```python
async def f():
    print("sleepy")
    await asyncio.sleep(1)
    print("refreshed!")

asyncio.run(f())
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
g = Reader.do(() =>
    @"third_num"
)

f = Reader.do(() =>
    @"first_num" + @"second_num" + @g()
)

f().run(ctx)
```

Specifically, here `@` means "`bind` the argument if it's a Reader, otherwise get it from the context object's `__getitem__`.
