import util.assert_eq

let content = with f = open("base/data.txt", "r"):
    f.read()

assert_eq(content, "123")