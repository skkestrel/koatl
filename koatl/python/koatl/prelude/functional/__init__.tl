export import .algebra.*
export import .result.*
export import .async.*
export import .env.*
export import .memo.*
export import .list.*

export id = x => x

export compose = (*args) =>
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