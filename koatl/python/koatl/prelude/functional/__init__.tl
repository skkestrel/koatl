export import .algebra.*
export import .result.*
export import .async.*
export import .reader.*
export import .memo.*
export import .list.*

named = name => fn =>
    fn.__name__ = name
    fn.__qualname__ = name
    fn

methods = {
    id: named("id")& x => x

    compose: named("compose")& (*args) =>
        args match:
            [] => raise ValueError("At least one function is required for composition")
            [f] => f
            [*fs] =>
                let composed = (*args, **kwargs) =>
                    let value = fs[-1](*args, **kwargs)
                    for f in fs[..-1..-1]:
                        value = f(value)
                    value

                composed.__name__ = "<Fn.compose()>"
                composed.__qualname__ = composed.__name__
                composed.signature = fs[-1].signature

                composed
            default raise ValueError("Invalid arguments for Fn.compose()")
}

globals().update(methods)

__all__ = methods.keys() | tuple