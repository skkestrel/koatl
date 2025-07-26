import functools.wraps
import asyncio
import .async_util

export Async = class:
    __init__ = (self, awaitable) => self.generator = awaitable.__await__()

    __await__ = self => self.generator

    __repr__ = self => "Async(...)"

    run = self => asyncio.run(async_util.await_one(self))

    bind_once = (self, f) => Async(async_util.bind_async(self, f))
    pure = &[staticmethod] x => Async(async_util.pure(x))

    sleep = &[staticmethod] x => Async(asyncio.sleep(x))
    gather = &[staticmethod] (*args) => Async(async_util.defer_await(asyncio.gather, *args))