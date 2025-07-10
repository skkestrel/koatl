import ast

print(ast.dump(ast.parse("""
from a import *
from a.b import *
"""), indent=4))