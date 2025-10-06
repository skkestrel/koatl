import functools.wraps
import asyncio

import .base.MonadOnce

export Async = class(MonadOnce):
    __init__ = (self, awaitable) => self.generator = awaitable.__await__()

    __await__ = self => self.generator

    __repr__ = self => "Async(...)"

    from_generator_fn = staticmethod! (generator_fn, *args, **kwargs) =>
        let m = object.__new__(Async)
        m.generator = generator_fn(*args, **kwargs)
        return m

    run = self => asyncio.run((() => await self)())

    bind_once = (self, f) => Async.from_generator_fn! () =>
        let result = f(yield from self.__await__())

        if not hasattr(result, "__await__"):
            raise ValueError(f"Expected the binder to return an Awaitable, but got {type(result).__name__}")

        yield from result.__await__()

    bind_gen = (self, gen) => Async.from_generator_fn! () =>
        try:
            while True:
                self = gen.send(yield from self.__await__())
                if not hasattr(self, "__await__"):
                    raise ValueError(f"Expected the binder to return an Awaitable, but got {type(self).__name__}")
        except StopIteration(value=value) =>
            return value

    pure = staticmethod! x => Async.from_generator_fn! () =>
        return x
        yield None

    sleep = staticmethod! x => Async(asyncio.sleep(x))

    # These execute immediately, so we need to wrap them in a generator
    # (otherwise, there might not be an event loop running)

    gather = staticmethod! (*args) => Async.from_generator_fn! () =>
        yield from asyncio.gather(*args)

    from_sync = staticmethod! (f, *args, **kwargs) => Async.from_generator_fn! () =>
        yield from asyncio.get_running_loop().run_in_executor(None, () => f(*args, **kwargs))
