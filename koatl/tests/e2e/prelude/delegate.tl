import util.assert_eq

# is Basic delegate: copy a single arg with its default ===

let target = (*, x=10, y=20) => x + y

let f = (a, delegate target(x)) => (a, x)
assert_eq(f(1), (1, 10))
assert_eq(f(1, x=5), (1, 5))

# is Delegate multiple args ===

let g = (delegate target(x, y)) => (x, y)
assert_eq(g(), (10, 20))
assert_eq(g(x=1), (1, 20))
assert_eq(g(x=1, y=2), (1, 2))

# is Delegate with alias (as) ===

let h = (delegate target(x as local_x)) => local_x
assert_eq(h(), 10)
assert_eq(h(local_x=99), 99)

# is Delegate with default override ===

let j = (delegate target(x=42)) => x
assert_eq(j(), 42)
assert_eq(j(x=7), 7)

# is Delegate with alias and default override ===

let k = (delegate target(x as my_x=100)) => my_x
assert_eq(k(), 100)
assert_eq(k(my_x=3), 3)

# is Delegate with **kwargs: extra kwonly args from target appear in kwargs dict ===

let target2 = (*, a=1, b=2, c=3) => a + b + c

let m = (delegate target2(a, **kw)) => (a, kw)
assert_eq(m(), (1, {"b": 2, "c": 3}))
assert_eq(m(a=10), (10, {"b": 2, "c": 3}))
assert_eq(m(a=10, b=20), (10, {"b": 20, "c": 3}))
assert_eq(m(a=10, b=20, c=30), (10, {"b": 20, "c": 30}))

# is Multiple delegates ===

let target_a = (*, p=1, q=2) => p + q
let target_b = (*, r=3, s=4) => r + s

let n = (delegate target_a(p, q), delegate target_b(r, s)) => (p, q, r, s)
assert_eq(n(), (1, 2, 3, 4))
assert_eq(n(p=10, s=40), (10, 2, 3, 40))

# is Delegate combined with regular kwonly args ===

let o = (a, *, delegate target(x), z=99) => (a, x, z)
assert_eq(o(1), (1, 10, 99))
assert_eq(o(1, x=5, z=50), (1, 5, 50))

# is Delegate with *args ===

let p = (*args, delegate target(x)) => (args, x)
assert_eq(p(1, 2, 3), ((1, 2, 3), 10))
assert_eq(p(1, 2, 3, x=5), ((1, 2, 3), 5))

# is Delegate arg with no default in target ===

let target3 = (*, req) => req

let q = (delegate target3(req)) => req
# req has no default, must be provided
assert_eq(q(req=42), 42)

# is Delegate **kwargs with no extra params (empty dict default) ===

let target4 = (*, x=1) => x

let r = (delegate target4(x, **kw)) => (x, kw)
assert_eq(r(), (1, {}))
assert_eq(r(x=5), (5, {}))

# is Delegate with real **kwargs on the outer function ===

let target5 = (*, a=1, b=2) => a + b

let s = (delegate target5(a, **t5_kw), **kwargs) => (a, t5_kw, kwargs)
assert_eq(s(), (1, {"b": 2}, {}))
assert_eq(s(a=10, b=20), (10, {"b": 20}, {}))
assert_eq(s(a=10, b=20, extra=99), (10, {"b": 20}, {"extra": 99}))

# is Catch-all: target has **kwargs, delegate absorbs all unknown kwargs ===

let target6 = (a=3, b=4, **kwargs) => (a, b, kwargs)

let t = (delegate target6(a as f, **kw), **real_kwargs) =>
    (f, kw, real_kwargs)

# g=3 should go to kw (target has **kwargs), not real_kwargs
# b=4 also appears in kw since it's a posorkw param of target
assert_eq(t(g=3), (3, {"b": 4, "g": 3}, {}))
assert_eq(t(f=5, g=3), (5, {"b": 4, "g": 3}, {}))
assert_eq(t(f=5, b=7, g=3), (5, {"b": 7, "g": 3}, {}))

# is Delegate with only **kwargs, no explicit args ===

let target7 = (*, a=1, b=2) => a + b

let u = (delegate target7(**kw)) => kw
assert_eq(u(), {"a": 1, "b": 2})
assert_eq(u(a=10), {"a": 10, "b": 2})
assert_eq(u(a=10, b=20), {"a": 10, "b": 20})

# is Multiple delegates both with **kwargs ===

let target8a = (*, p=1, q=2) => p + q
let target8b = (*, r=3, s=4) => r + s

let v = (delegate target8a(**kw_a), delegate target8b(**kw_b)) => (kw_a, kw_b)
assert_eq(v(), ({"p": 1, "q": 2}, {"r": 3, "s": 4}))
assert_eq(v(p=10, s=40), ({"p": 10, "q": 2}, {"r": 3, "s": 40}))

# is Target with positional-only params — shouldn't leak into **kwargs ===

let target9 = (a, b, /, *, c=3) => (a, b, c)

let w = (delegate target9(**kw)) => kw
# a and b are positional-only, shouldn't appear in kw; only c should
assert_eq(w(), {"c": 3})
assert_eq(w(c=10), {"c": 10})

# is Target with *args — VAR_POSITIONAL shouldn't appear ===

let target10 = (*args, x=1) => (args, x)

let x_fn = (delegate target10(**kw)) => kw
assert_eq(x_fn(), {"x": 1})
assert_eq(x_fn(x=5), {"x": 5})

# is Delegate forwarding: actually call target with delegated kwargs ===

let target11 = (*, x=10, y=20, z=30) => x + y + z

let forwarder = (delegate target11(x, **kw)) =>
    target11(x=x, **kw)

assert_eq(forwarder(), 60)
assert_eq(forwarder(x=1), 51)
assert_eq(forwarder(x=1, y=2), 33)
assert_eq(forwarder(x=1, y=2, z=3), 6)

# is All named params explicit + **kwargs catch-all (target has **kwargs) ===

let target12 = (a=1, **kwargs) => (a, kwargs)

let y_fn = (delegate target12(a, **kw)) => (a, kw)
assert_eq(y_fn(), (1, {}))
assert_eq(y_fn(a=5), (5, {}))
assert_eq(y_fn(a=5, extra=99), (5, {"extra": 99}))

# is Delegate to a delegate: chained delegation ===

let base = (*, x=1, y=2, z=3) => x + y + z

let middle = (delegate base(x, **kw)) =>
    base(x=x, **kw)

let outer = (delegate middle(x, **kw)) =>
    middle(x=x, **kw)

assert_eq(outer(), 6)
assert_eq(outer(x=10), 15)
assert_eq(outer(x=10, y=20), 33)
assert_eq(outer(x=10, y=20, z=30), 60)

# Verify defaults flow through the chain
let outer2 = (delegate middle(x=100, **kw)) =>
    middle(x=x, **kw)

assert_eq(outer2(), 105)
assert_eq(outer2(y=20, z=30), 150)

# is "delegate" as a regular identifier ===

let delegate = 42
assert_eq(delegate, 42)

let use_delegate = (delegate) => delegate + 1
assert_eq(use_delegate(10), 11)

let kw_delegate = (*, delegate=5) => delegate
assert_eq(kw_delegate(), 5)
assert_eq(kw_delegate(delegate=9), 9)

# is Delegate with dotted target (attribute access) ===

let ns = {
    func: (*, x=10, y=20) => x + y
}

let dot_fn = (delegate ns.func(x, y)) => (x, y)
assert_eq(dot_fn(), (10, 20))
assert_eq(dot_fn(x=1, y=2), (1, 2))

let dot_fn2 = (delegate ns.func(x as local_x, y=99)) => (local_x, y)
assert_eq(dot_fn2(), (10, 99))
assert_eq(dot_fn2(local_x=5, y=7), (5, 7))

# is Stacked delegates with explicit args + **kwargs ===

let stk_a = (*, x=1, y=2) => x + y
let stk_b = (*, z=3, w=4) => z + w

let stk_fn = (delegate stk_a(x, **kw_a), delegate stk_b(z, **kw_b)) => (x, kw_a, z, kw_b)
assert_eq(stk_fn(), (1, {"y": 2}, 3, {"w": 4}))
assert_eq(stk_fn(x=10, z=30), (10, {"y": 2}, 30, {"w": 4}))
assert_eq(stk_fn(x=10, y=20, z=30, w=40), (10, {"y": 20}, 30, {"w": 40}))

# is Stacked delegates with alias + default override + **kwargs ===

let stk2_a = (*, orig=1, extra_a=2) => orig + extra_a
let stk2_b = (*, val=3, extra_b=4) => val + extra_b

let stk2_fn = (delegate stk2_a(orig as local=100, **kw_a), delegate stk2_b(val, **kw_b)) =>
    (local, kw_a, val, kw_b)
assert_eq(stk2_fn(), (100, {"extra_a": 2}, 3, {"extra_b": 4}))
assert_eq(stk2_fn(local=10, val=30), (10, {"extra_a": 2}, 30, {"extra_b": 4}))
assert_eq(stk2_fn(local=10, extra_a=20, val=30, extra_b=40), (10, {"extra_a": 20}, 30, {"extra_b": 40}))

# is Mixed stacking: delegate without **kw before delegate with **kw ===

let mix_a = (*, x=1) => x
let mix_b = (*, y=3, extra=4) => y + extra

let mix_fn = (delegate mix_a(x), delegate mix_b(y, **kw)) => (x, y, kw)
assert_eq(mix_fn(), (1, 3, {"extra": 4}))
assert_eq(mix_fn(x=10), (10, 3, {"extra": 4}))
assert_eq(mix_fn(x=10, y=30, extra=40), (10, 30, {"extra": 40}))
