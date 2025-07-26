import functools.wraps
import koatl.runtime.traits.register_global_attr

pseudo_type_meta = (instancecheck) =>
    meta = type(
        "PseudoMeta"
        (type,)
        {
            __new__: (mcs, name, bases, attrs) =>
                attrs["__new__"] = (cls, value) =>
                    instancecheck(value)
                type.__new__(mcs, name, bases, attrs)

            __instancecheck__: (cls, instance) => instancecheck(instance)
        }
    )

    meta

export Ok = class(metaclass=pseudo_type_meta(__tl__.ok)):
    do = staticmethod(
        f => (*args, **kwargs) =>
                gen = f(*args, **kwargs)

                try:
                    x = gen.send(None)
                    while True:
                        if not Ok(x):
                            return x

                        x = gen.send(x)
                except StopIteration(value=value) as e:
                    return value
            | wraps(f)
    )

    assert = staticmethod(
        value =>
            value match:
                BaseException() as e => raise e
                None => raise ValueError("Expected a value, got None")
                default value
    )

export Err = class(metaclass=pseudo_type_meta(v => v matches BaseException())):
    None

export Some = class(metaclass=pseudo_type_meta(v => v <=> None)):
    None

export NotErr = class(metaclass=pseudo_type_meta(v => not (v matches BaseException()))):
    None

export NotOk = class(metaclass=pseudo_type_meta(v => not __tl__.ok(v))):
    None

register_global_attr(
    object,
    "map_err",
    (self, f) =>
        if self matches Err():
            return f(self)
        else:
            return self
)