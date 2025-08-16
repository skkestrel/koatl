import koatl.std.iter.Iterable

Extension.trait(Iterable)

import koatl.std.data.List

Extension.property(list, "len")& List.len.fget

import koatl.std.alg.*

Extension.property(object, "result")& self => Result(self)

__tl__.Ok = Ok
__tl__.Err = Err
__tl__.Result = Result

__tl__.op_coal = (x, f) =>
    if x matches Result():
        return x.coalesce(f)

    if not koatl.std.alg.result.infer_ok(x):
        return f()
    return x

__tl__.op_map = (x, f) =>
    if x matches Result():
        return x.map(f)

    if koatl.std.alg.result.infer_ok(x):
        return f(x)
    return x

__tl__.memo_value = Memo.value
__tl__.async_memo_value = AsyncMemo.value

import koatl.std.re.Pattern

Extension.method(str, "match")& (regex, str) => Pattern(regex).match(str)

# Note: the below methods have arguments in reverse order to Python's re module.
Extension.method(str, "matches")& (str, regex) => Pattern(regex).match(str)
Extension.method(str, "search")& (str, regex) => Pattern(regex).search(str)

import koatl.std.lazy_module.LazyModule

export Result, Ok, Err, Env, Memo, Async, AsyncMemo
export std = LazyModule("koatl.std")

ModuleProxy = class:
    __init__ = (self) =>
        self._modules = {}

    __getattr__ = (self, attr) =>
        if attr not in self._modules:
            self._modules[attr] = LazyModule(attr)
        return self._modules[attr]

    __repr__ = self => f"ModuleProxy({self._modules})"

export mod = ModuleProxy()