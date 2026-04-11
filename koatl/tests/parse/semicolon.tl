# Top-level semicolon separation
a = 1; b = 2; c = 3

# In indented blocks
if True:
    let x = 1; let y = 2

# Inline if with colon
if True: print(1); print(2)

# Inline if with then
if True then print(1); print(2)

# Arrow block
let f = x => let y = x + 1; y * 2

# For loop inline
for i in [1]: print(i); print(i + 1)

# While loop inline
while False: print(1); break

# Match arms
let x = 1
match x:
    1 => let a = 10; a + 1
    _ => 0

# elif / else with semicolons
if False: print(1); print(2)
elif True: print(3); print(4)
else: print(5); print(6)

# With statement
with f = open("/dev/null"): print(f); print("done")

# Try/except
try: print(1); print(2)
except Exception() => print("err")

# Nested inline blocks
if True: if False: print(1); print(2)

# Semicolons in for body with nested if
for i in [1, 2, 3]:
    if i > 1: print(i); print(i * 10)

# Deeply nested
if True:
    for i in [1]:
        if i == 1: print(i); print(i + 1)
