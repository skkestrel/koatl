print("hello world")

(
    () =>
        raise ValueError("hello exception")
)()