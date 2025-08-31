import functools.wraps


export do = f => wraps(f)! (*args, **kwargs) =>
    let gen = f(*args, **kwargs)

    let m
    try:
        m = gen.send(None)
    except StopIteration() as e =>
        raise ValueError(
            "Returning before `@` is not allowed. "
                + "Use `return @MonadType.pure(value)` instead."
        )

    try:
        # TODO: this is a workaround to avoid recursion.
        # is it possible to derive bind_gen directly from bind_once?

        return m.bind_gen(gen)
    except AttributeError() =>
        None

    let recurse = v =>
        try:
            m = gen.send(v)
            return m.bind_once(recurse)
        except StopIteration() as e =>
            return m.pure(e.value)

    try:
        return m.bind_once(recurse)
    except AttributeError() =>
        raise ValueError("@ can only be used with an object that has `bind_once`.")