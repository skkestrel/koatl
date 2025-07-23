import builtins.iter
import functools.wraps

# export is_ok = __coalesces
# export untry = x => isinstance(x, BaseException) then raise x else x

# export do = () =>
#     f =>
#         (*args, **kwargs) =>
#             gen = f(*args, **kwargs)

#             try:
#                 x = next(gen)
#                 while True:
#                     if x.(is_ok):
#                         return x

#                     x = gen.send(x)
#             except StopIteration(value=value):
#                 return value

#           | wraps(f)