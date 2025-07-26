"""
At the time of writing, I haven't implemented async def
in Koatl, so this is a workaround.

This function converts a generic awaitable (perhaps a
vanilla generator) into a coroutine that can be used
by asyncio.run, which expects strictly a coroutine.

(In practice, async def shouldn't be needed anyways,
since we have the Async monad.)
"""


async def to_coro(awaitable):
    return await awaitable
