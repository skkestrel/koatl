fib = x =>
    if x < 2:
        1
    else:
        fib(x-1) + fib(x-2)

for i in range(20):
    print(i.(fib))