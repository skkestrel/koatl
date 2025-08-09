let fib = n =>
    n match:
        0 => @Memo.pure(0)
        1 => @Memo.pure(1)
        _ => memo @fib(n - 1) + @fib(n - 2)

let ctx = Memo.Cache()

let result = (..)
    .map(i => fib(i).run(ctx))
    .take_while($ <= 4_000_000)
    .filter($ % 2 == 0)
    .sum()

print(result)