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
This represents a deterministic monad, ruling out the List monad.

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

g = Memo.fn& x =>
    2 + 2

g().run()
```

A Memo instance can be constructed using `Memo.fn(function_to_memoize)`, or `Memo.value(unique_id, dependencies, function)`.
Using the `memo` keyword automatically constructs a `@Memo.value(id, deps, fn)`, where `deps` is inferred using variables _directly_ captured by the proceeding expression (not including global captures, or captures by inner nested functions).

## Result

The Result type has two subtypes, Ok and Err. Using the Result constructor will automatically turn a value into one of the two subtypes:

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

The Result monad represents error handling and early return:

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

or, perhaps more familiarly in Rust,

```rust
fn f() -> Result<T, E> {
    let x = get_some_value_or_none().ok_or_else(...)?;
    let y = get_some_other_value_or_error(x)?;
    Ok(x + y)
}
```

To explicitly mark an exception or None as an Ok value, simply use `Ok(None)`.

While errors typically aren't returned from functions in Python, the `try` operator (see [Operators](operators)) makes it very easy to use these constructions to interface with external code.

Important: Result provides a default `bind_once` implementation for ALL types that don't otherwise define it;
this means that (1).bind_once(...) will work, and therefore, the @ operator will also work with bare non-Result values using Result semantics.

`.result` is also registered as a v-property on all objects, so the following will work:

```koatl
external_function().result match:
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

The Env monad allows interfacing with an external context object.
Instead of having to pass around a context object as an explicit parameter:

```koatl
g = ctx =>
    ctx["third_num"]

f = ctx =>
    ctx["first_num"] + ctx["second_num"] + g(ctx)

f(ctx)
```

We can use the Env monad:

```koatl
g = () =>
    @Env.item("third_num")

f = () =>
    @Env.item("first_num") + @Env.item("second_num") + @g()

f().run(ctx)
```
