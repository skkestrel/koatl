import functools.wraps

export Reader = class:
    __init__ = (self, generator) =>
        self.generator = generator

    __repr__ = self =>
        return "Reader(...)"

    run = (self, ctx) =>
        gen = self.generator

        try:
            x = gen.send(None)
            while True:
                if x matches Reader():
                    to_send = x.run(ctx)
                else:
                    to_send = ctx[x]

                x = gen.send(to_send)
        except StopIteration(value=value) as e:
            return value

    do = staticmethod(f => wraps(f)((*args, **kwargs) => Reader(f(*args, **kwargs))))