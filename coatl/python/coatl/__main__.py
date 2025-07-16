import sys

if len(sys.argv) < 2:
    print(f"Usage: python -m coatl <filename.(py|tl)>")
    sys.exit(1)

script_path = sys.argv[1]
sys.argv = sys.argv[1:]

try:
    with open(script_path, "r") as f:
        original_script_code = f.read()
except FileNotFoundError:
    print(f"Error: File not found at '{script_path}'")
    sys.exit(1)

if script_path.endswith('.tl'):
    import coatl.cli
    coatl.cli.run_from_source(original_script_code, mode="script", script_path=script_path)
else:
    injected_code = \
"""
# -- injected code --
from coatl.runtime import *
"""

    full_code = injected_code + original_script_code
    code_obj = compile(full_code, script_path, 'exec')

    script_globals = {
        '__name__': '__main__',
    }

    exec(code_obj, script_globals)