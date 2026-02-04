import util.assert_eq

# Test basic elif
x = 2
let result
if x == 1:
    result = "one"
elif x == 2:
    result = "two"
else:
    result = "other"
assert_eq(result, "two")

# Test multiple elif clauses
score = 75
let grade = if score >= 90:
    "A"
elif score >= 80:
    "B"
elif score >= 70:
    "C"
elif score >= 60:
    "D"
else:
    "F"
assert_eq(grade, "C")

# Test elif without else
value = 5
output = "default"
if value < 0:
    output = "negative"
elif value == 0:
    output = "zero"
elif value > 0:
    output = "positive"
assert_eq(output, "positive")

# Test elif with expressions
a, b = 10, 20
let relation
if a > b:
    relation = "greater"
elif a < b:
    relation = "less"
elif a == b:
    relation = "equal"
assert_eq(relation, "less")

# Test nested if with elif
outer_val = 2
inner_val = 3
let result2
if outer_val == 1:
    result2 = "outer1"
elif outer_val == 2:
    if inner_val == 1:
        result2 = "inner1"
    elif inner_val == 2:
        result2 = "inner2"
    elif inner_val == 3:
        result2 = "inner3"
    else:
        result2 = "inner_else"
else:
    result2 = "outer_else"
assert_eq(result2, "inner3")

# Test elif in expression context (as part of block expression)
num = 15
msg = if num < 10:
    "small"
elif num < 20:
    "medium"
else:
    "large"
assert_eq(msg, "medium")

# Test elif with complex conditions
x, y = 5, 11
let result3
if x > 10 and y > 10:
    result3 = "both large"
elif x > 10 or y > 10:
    result3 = "one large"
elif x == y:
    result3 = "equal"
else:
    result3 = "both small"
assert_eq(result3, "one large")

print("All elif tests passed!")
