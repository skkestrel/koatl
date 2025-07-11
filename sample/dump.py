import ast

print(ast.dump(ast.parse("""
try:
    pass
except Exception as e:
    pass
except Exception:
    pass
except:
    pass
"""), indent=4))