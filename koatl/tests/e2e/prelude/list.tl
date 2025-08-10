import util.assert_eq
import time

# Async traversal must be concurrent.

now = time.time()

assert_eq(
    (..10).traverse(x =>
        @Async.sleep(0.1)
        x
    ).run()
    (..10).list()
)

assert_eq(time.time() - now < 0.3, True)

# Result traversal must short-circuit.

# Bare Result traversal should not be wrapped in Ok.

assert_eq(
    [1, 2, 3].traverse(x => Ok(x + 1))
    [2, 3, 4].map(Ok).list()
)

counts = 0
assert_eq(
    (..20).traverse(x =>
        counts += 1
        if x == 10:
            return Err()
        Ok(x)
    )
    Err()
)
assert_eq(counts, 11)

counts = 0
assert_eq(
    (..10).traverse(x =>
        counts += 1
        Ok(x)
    )
    (..10).map(Ok).list()
)
assert_eq(counts, 10)
