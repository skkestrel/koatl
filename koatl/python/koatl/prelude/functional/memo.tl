import collections.defaultdict
import functools.wraps
import .result.Ok

export MemoCtx = class:
    __init__ = self =>
        self.cache = defaultdict(dict)

    get_or_compute = (self, name, args, f) =>
        if try self.cache[name][args] except KeyError() matches (Ok() as v):
            return v

        let value = f()
        self.cache[name][args] = value
        value

    __repr__ = self => f"MemoCtx({self.cache})"

export Memo = class:
    __init__ = (self, id, deps, f) =>
        self.id = id
        self.deps = deps
        self.f = f

    __repr__ = self => f"Memo(...)"

    run = (self, ctx=MemoCtx()) =>
        ctx.get_or_compute(self.id, tuple(self.deps), self.f)

    fn = staticmethod& f => wraps(f)& (*args, **kwargs) =>
        let m = object.__new__(Memo)
        let name = f"{f.__module__}.{f.__qualname__}"
        m.run = (ctx=MemoCtx()) =>
            let arg_key = (tuple(args), tuple(kwargs.items()))

            ctx.get_or_compute(name, arg_key, () =>
                let v = f(*args, **kwargs)
                if v matches Memo():
                    v = v.run(ctx)
                v
            )
        m

    bind_gen = (self, gen) =>
        let m = object.__new__(Memo)
        m.run = (ctx=MemoCtx()) =>
            self = self.run(ctx)
            try:
                while True:
                    self = gen.send(self)
                    if self matches Memo():
                        self = self.run(ctx)
            except StopIteration(value=value):
                return value
        m

__tl__.memo = Memo