import util.assert_eq
import time

counts = 0

fib = Memo.fn! x =>
    if x < 2:
        return @Memo.pure(1)

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

ctx = Memo.Cache()
assert_eq(f(10).run(ctx), 20)
assert_eq(f(10).run(ctx), 20)
assert_eq(f(5).run(ctx), 10)
assert_eq(f(5).run(ctx), 10)
assert_eq(counts, 2)

# Indirect dependencies should be tracked as well.

counts = 0

f = (x, y) =>
    memo:
        counts = counts + 1
        let other = () => y
        x * other()

ctx = Memo.Cache()
assert_eq(f(10, 20).run(ctx), 200)
assert_eq(f(10, 20).run(ctx), 200)
assert_eq(f(10, 30).run(ctx), 300)
assert_eq(f(10, 30).run(ctx), 300)
assert_eq(counts, 2)

fib = x => x < 2 then @Memo.pure(1) else memo @fib(x - 1) + @fib(x - 2)

now = time.time()
fib(35).run()

# Improperly memoized fib will take much longer to run.
assert_eq(time.time() - now < 0.1, True)


# async memo

f = () =>
    async memo:
        @Async.sleep(0.1)
        123

now = time.time()
ctx = Memo.Cache()
assert_eq(f().run(ctx), 123)
assert_eq(f().run(ctx), 123)
assert_eq(time.time() - now < 0.12, True)

# AsyncMemo.fn

f = AsyncMemo.fn! () =>
    @Async.sleep(0.1)
    123

now = time.time()
ctx = Memo.Cache()
assert_eq(f().run(ctx), 123)
assert_eq(f().run(ctx), 123)
assert_eq(time.time() - now < 0.12, True)


now = time.time()
ctx = Memo.NullCache()
assert_eq(f().run(ctx), 123)
assert_eq(f().run(ctx), 123)
assert_eq(time.time() - now > 0.19, True)