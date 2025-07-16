import builtins.iter
import functools.wraps

export do = () =>
    f =>
        (*args, **kwargs) =>
            gen = f(*args, **kwargs)

            try:
                x = next(gen)
                while True:
                    if x === None:
                        return None

                    x = gen.send(x)
            except StopIteration as e:
                return e.value
            
          | wraps(f)
