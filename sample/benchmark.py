import ast
import timeit

# The code we want to represent
source_string = "x = 10 + 5"

# --- Method 1: Compile from string ---
def compile_from_string():
    return compile(source_string, '<string>', 'exec')

# --- Method 2: Construct AST manually and compile ---
def compile_from_manual_ast():
    node = ast.Module(
        body=[
            ast.Assign(
                targets=[ast.Name(id='x', ctx=ast.Store())],
                value=ast.BinOp(
                    left=ast.Constant(value=10),
                    op=ast.Add(),
                    right=ast.Constant(value=5)
                )
            )
        ],
        type_ignores=[]
    )
    ast.fix_missing_locations(node)
    return compile(node, '<string>', 'exec')

# --- Benchmarking ---
n_times = 100_000

time1 = timeit.timeit(compile_from_string, number=n_times)
print(f"Compiling from string: {time1:.4f} seconds")

time2 = timeit.timeit(compile_from_manual_ast, number=n_times)
print(f"Constructing and compiling AST: {time2:.4f} seconds")

print(f"\nManual AST construction is roughly {time2/time1:.1f}x slower for this example.")

# --- Note: Execution time is the same ---
code1 = compile_from_string()
code2 = compile_from_manual_ast()

exec_time1 = timeit.timeit(lambda: exec(code1), number=n_times)
exec_time2 = timeit.timeit(lambda: exec(code2), number=n_times)

print(f"\nExecution time for string-compiled code: {exec_time1:.4f} seconds")
print(f"Execution time for AST-compiled code:   {exec_time2:.4f} seconds")