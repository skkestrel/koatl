import util.assert_eq

counts = 0

fib = Memo.fn& x =>
    if x < 2:
        return 1

    counts = counts + 1

    let a = @fib(x - 1)
    let b = @fib(x - 2)

    a + b

assert_eq(fib(10).run(), 89)
assert_eq(counts < 20, True)

counts = 0

f = x =>
    memo:
        counts = counts + 1
        x * 2

ctx = MemoCtx()
assert_eq(f(10).run(ctx), 20)
assert_eq(f(10).run(ctx), 20)
assert_eq(f(5).run(ctx), 10)
assert_eq(counts, 2)
