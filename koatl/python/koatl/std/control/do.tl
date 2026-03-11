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

    let bind_gen = try:
        m.bind_gen
    except AttributeError() =>
        None

    if bind_gen !== None:
        # TODO: this is a workaround to avoid recursion.
        # is it possible to derive bind_gen directly from bind_once?
        return bind_gen(gen)

    let recurse = v =>
        try:
            m = gen.send(v)
            return m.bind_once(recurse)
        except StopIteration() as e =>
            return m.pure(e.value)

    let bind_once = try:
        m.bind_once
    except AttributeError() =>
        None

    if bind_once !== None:
        return bind_once(recurse)
    else:
        raise ValueError("@ can only be used with an object that has `bind_once`.")