import util.assert_eq

# Top-level semicolons
a = 1; b = 2; c = a + b
assert_eq(c, 3)

# Semicolons inside indented block
if True:
    let x = 10; let y = 20
    assert_eq(x + y, 30)

# Inline if with semicolons (expression statements)
results = []
if True: results.append(1); results.append(2)
assert_eq(results, [1, 2])

# Inline if-then with semicolons
results2 = []
if True then results2.append(5); results2.append(10)
assert_eq(results2, [5, 10])

# Arrow block with semicolons
let compute = x => let r = x * 2; r + 1
assert_eq(compute(3), 7)

# For loop with semicolons in body
total = 0
for i in [1, 2, 3]:
    let x = i * 10; total += x
assert_eq(total, 60)

# Match arms with semicolons
let val = 2
let result = match val:
    1 => let a = 10; a + 1
    2 => let b = 20; b + 2
    _ => 0
assert_eq(result, 22)

# elif / else with semicolons
out = []
if False: out.append(1); out.append(2)
elif True: out.append(3); out.append(4)
else: out.append(5); out.append(6)
assert_eq(out, [3, 4])

# While loop with semicolons
counter = 0; total2 = 0
while counter < 3: total2 += counter; counter += 1
assert_eq(total2, 3)
assert_eq(counter, 3)

# Nested inline blocks — semicolons bind to innermost
nested = []
if True: if True: nested.append(1); nested.append(2)
assert_eq(nested, [1, 2])

# Semicolons in for body with inline if
collected = []
for i in [1, 2, 3]:
    if i > 1: collected.append(i); collected.append(i * 10)
assert_eq(collected, [2, 20, 3, 30])

# Deeply nested blocks
deep = []
if True:
    for i in [1, 2]:
        if i == 1: deep.append("a"); deep.append("b")
assert_eq(deep, ["a", "b"])
