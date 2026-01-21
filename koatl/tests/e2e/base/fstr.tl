import util.assert_eq

assert_eq(f"{1}", "1")

x = 1

fstr = f"x = {x + 1}"
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

# Test format specifiers
num = 42
assert_eq(f"{num%d}", "42")
assert_eq(f"{num%05d}", "00042")
assert_eq(f"{num%>5d}", "   42")
assert_eq(f"{num%<5d}", "42   ")
assert_eq(f"{num%^5d}", " 42  ")

pi = 3.14159
assert_eq(f"{pi%.2f}", "3.14")
assert_eq(f"{pi%.4f}", "3.1416")

hex_num = 255
assert_eq(f"{hex_num%x}", "ff")
assert_eq(f"{hex_num%X}", "FF")
assert_eq(f"{hex_num%#x}", "0xff")

# Test multiple format specs in one string
assert_eq(f"{num%05d} and {pi%.2f}", "00042 and 3.14")

# Test format spec with expressions
assert_eq(f"{1 + 1%d}", "2")
assert_eq(f"{(10 * 5)%03d}", "050")