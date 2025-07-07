import ast

print(ast.dump(ast.parse("""
def fn(x, y, z):
    return 42
fn
"""), indent=4))