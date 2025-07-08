import ast

print(ast.dump(ast.parse("""
def fn(x, y, z):
    return 42
fn.a[1].b = 2
"""), indent=4))