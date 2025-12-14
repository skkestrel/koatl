export import koatl.std.(
    trait.*
    ext.*
    data.*
    iter.*
    control.(Result, Ok, Err, Env, Memo, Async, AsyncMemo, do)
)

import koatl.std.(
    re.Pattern
    lazy_module.(LazyModule, RootModulesProxy)
)

export std = LazyModule("koatl.std")
export mod = RootModulesProxy()

# Extensions

Extension.trait(Iterable)

Extension.method(str, "match")! (regex, str) => Pattern(regex).match(str)
Extension.method(str, "matches")! (str, regex) => Pattern(regex).match(str)

Extension.property(list, "len")! List.len.fget
Extension.method(list, "map")! (self, f) => self.iter.map(f).list()
Extension.method(list, "filter")! (self, f) => self.iter.filter(f).list()

Extension.method(dict, "map")! (self, f) => self.iter.map(f).dict()
Extension.method(dict, "map_values")! (self, f) => self.iter.map([k, v] => [k, f(v)]).dict()
Extension.method(dict, "map_keys")! (self, f) => self.iter.map([k, v] => [f(k), v]).dict()
Extension.method(dict, "filter")! (self, f) => self.iter.filter(f).dict()
Extension.method(dict, "filter_keys")! (self, f) => self.iter.filter([k, v] => f(k)).dict()
Extension.method(dict, "filter_values")! (self, f) => self.iter.filter([k, v] => f(v)).dict()

# Implementations for language features

__tl__.do = koatl.std.control.do.do

__tl__.op_map = koatl.std.control.result.op_map
__tl__.op_coal = koatl.std.control.result.op_coal

__tl__.Ok = koatl.std.control.result.Ok
__tl__.Err = koatl.std.control.result.Err
__tl__.Result = koatl.std.control.result.Result

__tl__.MatchError = koatl.std.MatchError

__tl__.memo_value = koatl.std.control.memo.Memo.value
__tl__.async_memo_value = koatl.std.control.memo.AsyncMemo.value