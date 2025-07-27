# Troubleshooting

```koatl
import ast
import koatl.transpile

my_tl_code = """
problematic(koatl(code))
"""

# prints out the transpiled Python code
my_tl_code | transpile | ast.unparse | print

# prints out the transpiled Python AST
my_tl_code | transpile | ast.dump($, indent=4) | print
```
