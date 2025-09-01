import util.assert_eq

assert_eq(f"{1}", "1")

x = 1

fstr = f"x = {x+1}"
assert_eq(fstr, "x = 2")

fstr = f"x = {(y => y)(x)}"
assert_eq(fstr, "x = 1")

fstr = f"={(
    let a = 1
    let b = 2
    let c = 3
    a * b * c
)}="
assert_eq(fstr, "=6=")

assert_eq(f"{{}}", "{}")
assert_eq(f"{{{1}}}", "{1}")

assert_eq(f"""

{{1}}
{2 * 5}""", "\n\n{1}\n10")