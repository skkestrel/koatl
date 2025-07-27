import util.assert_eq
import time

x = []

f = () =>
    x.append(1)
    @Async.sleep(0.01)
    x.append(1)
    @Async.sleep(0.01)
    x.append(1)
    @Async.sleep(0.01)
    x.append(1)
    @Async.sleep(0.01)

f().run()
assert_eq(x, [1, 1, 1, 1])


x = []
f = () =>
    x.append(1)
    @Async.sleep(0.05)
    x.append(1)
    @Async.sleep(0.05)

start = time.time()
(..20).map(_ => f()) | (Async.gather(*$)) | Async.run
end = time.time()

assert_eq(x | len, 40)
assert_eq(end - start < 0.3, True)