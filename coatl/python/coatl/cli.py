def transpile_from_source(source, mode="script", script_path="<string>"):
    from coatl import transpile
    import ast

    transpiled_code = transpile(source, mode=mode)

    return ast.unparse(transpiled_code)

def run_from_source(source, mode="script", script_path="<string>"):
    from coatl import transpile

    transpiled_code = transpile(source, mode=mode)
    code_obj = compile(transpiled_code, script_path, 'exec')

    script_globals = {
        '__name__': '__main__',
    }

    exec(code_obj, script_globals)

def run_from_path(script_path, mode="script"):
    with open(script_path, "r") as f:
        original_script_code = f.read()
    run_from_source(original_script_code, mode=mode, script_path=script_path)