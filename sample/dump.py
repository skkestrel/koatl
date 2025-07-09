import ast

print(ast.dump(ast.parse("""
fn(*a, **b, c=4, **e)
"""), indent=4))