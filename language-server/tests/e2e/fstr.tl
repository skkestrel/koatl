import util.assert_eq

assert_eq(f"{1}", "1")

x = 1

fstr = f"x = {x+1}"
assert fstr == "x = 2",
    fstr

fstr = f"x = {(y => y)(x)}"
assert fstr == "x = 1",
    fstr

fstr = f"={
    a = 1
    b = 2
    c = 3
    a * b * c
}="
assert fstr == "=6=",
    fstr