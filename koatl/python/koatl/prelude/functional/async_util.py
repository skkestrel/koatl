"""
At the time of writing, I haven't implemented async def
in Koatl, so this is a workaround to provide implementations
for Async.

(In practice, async def shouldn't be needed anyways,
since we have the Async monad.)
"""


async def await_one(awaitable):
    return await awaitable


async def defer_await(fn, *args, **kwargs):
    return await fn(*args, **kwargs)


async def bind_async(awaitable, func):
    result = func(await awaitable)

    if hasattr(result, "__await__"):
        return await result

    return result


async def pure(value):
    return value
