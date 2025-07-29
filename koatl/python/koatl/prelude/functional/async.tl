import functools.wraps
import asyncio

import .async_util
import .monad.Monad

export Async = class(Monad):
    __init__ = (self, awaitable) => self.generator = awaitable.__await__()

    __await__ = self => self.generator

    __repr__ = self => "Async(...)"

    from_generator_fn = staticmethod& (generator_fn, *args, **kwargs) =>
        let m = object.__new__(Async)
        m.generator = generator_fn(*args, **kwargs)
        return m

    run = self => asyncio.run(async_util.to_coro(self))

    bind_once = (self, f) => Async.from_generator_fn& () =>
        let result = f(yield from self.__await__())

        if hasattr(result, "__await__"):
            return yield from result.__await__()

        return result

    bind_gen = (self, gen) => Async.from_generator_fn& () =>
        try:
            while True:
                self = gen.send(yield from self.__await__())
        except StopIteration(value=value):
            return value

    pure = staticmethod& x => Async.from_generator_fn& () =>
        return x
        yield None

    sleep = staticmethod& x => Async(asyncio.sleep(x))

    #- TODO: Why is asyncio.gather eager? -#
    gather = staticmethod& (*args) => Async.from_generator_fn& () => yield from asyncio.gather(*args)