print([
    1
    2
    *[3]
    4
])

print([1, 2, 3, 4])
print([1, 2,
    3, 4])

apply = (x, y) => x(y)
apply(
    x =>
        print(x)
        print(x)
    2
)