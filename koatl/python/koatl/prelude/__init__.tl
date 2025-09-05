export import koatl.std.(
    trait.*
    ext.*
    data.*
    iter.*
    alg.(Result, Ok, Err, Env, Memo, Async, AsyncMemo)
    alg.do
)

import koatl.std.(
    re.Pattern
    lazy_module.(LazyModule, RootModulesProxy)
)

export std = LazyModule("koatl.std")
export mod = RootModulesProxy()

Extension.trait(Iterable)

Extension.property(list, "len")! List.len.fget

Extension.property(object, "result")! self => Result(self)

Extension.method(str, "match")! (regex, str) => Pattern(regex).match(str)

# Note: the below methods have arguments in reverse order to Python's re module.
Extension.method(str, "matches")! (str, regex) => Pattern(regex).match(str)
Extension.method(str, "search")! (str, regex) => Pattern(regex).search(str)

__tl__.do = koatl.std.alg.do.do

__tl__.op_map = koatl.std.alg.result.op_map
__tl__.op_coal = koatl.std.alg.result.op_coal

__tl__.Ok = koatl.std.alg.result.Ok
__tl__.Err = koatl.std.alg.result.Err
__tl__.Result = koatl.std.alg.result.Result

__tl__.MatchError = koatl.std.MatchError

__tl__.memo_value = koatl.std.alg.memo.Memo.value
__tl__.async_memo_value = koatl.std.alg.memo.AsyncMemo.value