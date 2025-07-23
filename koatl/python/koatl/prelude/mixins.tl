export create_mixin = (mixin_name, methods) =>
    import inspect

    for name, method in methods:
        method.__name__ = name
        method.__qualname__ = f"{mixin_name}.{name}"

        sig = inspect.signature(method)
        params = sig.parameters.values() | list
        params[0].replace(name="self")
        sig = sig.replace(parameters=params)
        method.__signature__ = sig

    typ = type(mixin_name, (), methods)
    typ._methods = methods
    typ

export curse_mixin = (mixin, types) =>
    # TODO avoid cursing?
    try:
        import forbiddenfruit.curse

        for name, method in mixin._methods:
            for typ in types:
                curse(typ, name, method)
    except e:
        print("Failed to curse:", e)