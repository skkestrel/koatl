from IPython.core.magic import register_line_magic

from coatl import transpile
from .magic import coatl_cell_magic, CoatlSystemAssign, CoatlMagicAssign, CoatlEscapedCommand

import ast

def source_code_transformer(lines):
    source = "".join(lines)
    py_ast = transpile(source, mode="interactive")
    transformed_source = ast.unparse(py_ast)

    lines = [line + '\n' for line in transformed_source.splitlines()]

    return lines


def import_prelude(namespace):
    exec("from coatl.runtime import *", namespace)
    exec("from coatl.prelude import *", namespace)


def load_ipython_extension(ipython):
    import_prelude(ipython.user_ns)
    print("coatl enabled")

    ttm = ipython.input_transformer_manager
    
    def activate_compiler():
        if source_code_transformer not in ipython.input_transformers_post:
            ipython.input_transformers_post.append(source_code_transformer)

        if coatl_cell_magic not in ttm.line_transforms:
            ttm.line_transforms.insert(0, coatl_cell_magic)

        for cls in [CoatlMagicAssign, CoatlEscapedCommand, CoatlSystemAssign]:
            if cls not in ttm.token_transformers:
                ttm.token_transformers.insert(0, cls)

        ipython.coatl_active = True

    def deactivate_compiler():
        if source_code_transformer in ipython.input_transformers_post:
            ipython.input_transformers_post.remove(source_code_transformer)

        if coatl_cell_magic in ttm.line_transforms:
            ttm.line_transforms.remove(coatl_cell_magic)

        for cls in [CoatlMagicAssign, CoatlEscapedCommand, CoatlSystemAssign]:
            if cls in ttm.token_transformers:
                ttm.token_transformers.remove(cls)

        ipython.coatl_active = False

    @register_line_magic
    def coatl(line):
        """
        Magic command to toggle the custom compiler.
        Usage:
          %coatl on
          %coatl off
          %coatl status
        """
        command = line.strip().lower()
        if command == 'on':
            activate_compiler()
        elif command == 'off':
            deactivate_compiler()
        elif command == 'status':
            status = "active" if ipython.custom_compiler_active else "inactive"
            print(f"coatl is {status}.")
        else:
            print("Usage: %coatl <on|off|status>")
    
    activate_compiler()


def unload_ipython_extension(ipython):
    if CoatlMagicAssign in ipython.input_transformer_manager.token_transformers:
        ipython.token_transformers.remove(0, CoatlMagicAssign)
    if CoatlSystemAssign in ipython.input_transformer_manager.token_transformers:
        ipython.token_transformers.remove(1, CoatlSystemAssign)
    if source_code_transformer in ipython.input_transformers_post:
        ipython.input_transformers_post.remove(source_code_transformer)

    print("coatl disabled.")
