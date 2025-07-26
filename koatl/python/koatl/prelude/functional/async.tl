import functools.wraps
import asyncio
import .ok.Ok

import ._async_util

#-
    Wraps a raw Python awaitable into an Async object.
    - creates a generator object that yields a single value
-#
wrap_awaitable = v => Async.do((() => @v))

export Async = class:
    __init__ = (self, generator) =>
        self.generator = generator

    __await__ = self =>
        for future in self.generator:
            @@(try future.__await__() except AttributeError())
                !map_err(e => TypeError(f"Object {future} is not awaitable: {e}"))
                .(Ok.assert)

    __repr__ = self =>
        return "Async(...)"

    run = self => self | _async_util.wrap | asyncio.run

    do = staticmethod(
        f => wraps(f)((*args, **kwargs) => Async(f(*args, **kwargs)))
    )

    sleep = staticmethod(do(x => @asyncio.sleep(x)))
    create_task = staticmethod(do(x => @asyncio.create_task(x)))
    gather = staticmethod(do((*args) => @asyncio.gather(*args)))
    wait = staticmethod(do((*args) => @asyncio.wait(*args)))