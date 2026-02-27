# if let — block form
if let Some(x) = expr:
    x

# if let — with else
if let Some(x) = expr:
    x
else:
    0

# if not let — with else
if not let Some(x) = expr:
    raise
else:
    x

# if let — colon block
if let [a, b] = pair:
    a + b

# if let — bare then inline
if let [a, b] = pair then a + b

# while let — block form
while let Some(x) = iter.next():
    process(x)

# while let — colon block
while let (ok, val) = source[i]:
    let total = total + val

# while let — colon inline
while let (k, v) = pairs[i]:
    handle(k, v)

# if with colon — block
if cond:
    body

# if with then keyword — inline
if cond then body

# if/elif with then
if a:
    1
elif b:
    2
elif c then 3
else:
    4

# mixed colon and then
if a:
    1
elif b:
    2
elif c then 3
else:
    4
