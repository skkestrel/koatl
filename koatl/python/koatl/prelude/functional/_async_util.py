"""
At the time of writing, I haven't implemented async def
in Koatl, so this is a workaround.

(In practice, async def should never be needed anyways, since we have Async.do)
"""


async def wrap(tl_async):
    await tl_async
