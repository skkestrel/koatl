from IPython.core.magic import register_line_magic

from koatl import transpile
from .magic import (
    koatl_cell_magic,
    KoatlSystemAssign,
    KoatlMagicAssign,
    KoatlEscapedCommand,
)

import ast


def source_code_transformer(lines):
    source = "".join(lines)
    py_ast = transpile(source, mode="interactive")
    transformed_source = ast.unparse(py_ast)

    lines = [line + "\n" for line in transformed_source.splitlines()]

    return lines


def try_import_prelude(namespace):
    try:
        exec("from koatl.runtime import *", namespace)
        exec("from koatl.prelude import *", namespace)
    except Exception as e:
        import traceback

        print(
            "There was an error importing the Koatl prelude. Some features may not work."
        )
        traceback.print_exc()


def load_ipython_extension(ipython):
    print("Switched notebook to Koatl.")

    try_import_prelude(ipython.user_ns)

    ttm = ipython.input_transformer_manager

    def activate_compiler():
        if source_code_transformer not in ipython.input_transformers_post:
            ipython.input_transformers_post.append(source_code_transformer)

        if koatl_cell_magic not in ttm.line_transforms:
            ttm.line_transforms.insert(0, koatl_cell_magic)

        for cls in [KoatlMagicAssign, KoatlEscapedCommand, KoatlSystemAssign]:
            if cls not in ttm.token_transformers:
                ttm.token_transformers.insert(0, cls)

        ipython.koatl_active = True

    def deactivate_compiler():
        if source_code_transformer in ipython.input_transformers_post:
            ipython.input_transformers_post.remove(source_code_transformer)

        if koatl_cell_magic in ttm.line_transforms:
            ttm.line_transforms.remove(koatl_cell_magic)

        for cls in [KoatlMagicAssign, KoatlEscapedCommand, KoatlSystemAssign]:
            if cls in ttm.token_transformers:
                ttm.token_transformers.remove(cls)

        ipython.koatl_active = False

    @register_line_magic
    def koatl(line):
        """
        Magic command to toggle the custom compiler.
        Usage:
          %koatl on
          %koatl off
          %koatl status
        """
        command = line.strip().lower()
        if command == "on":
            activate_compiler()
        elif command == "off":
            deactivate_compiler()
        elif command == "status":
            status = "active" if ipython.custom_compiler_active else "inactive"
            print(f"koatl is {status}.")
        else:
            print("Usage: %koatl <on|off|status>")

    activate_compiler()


def unload_ipython_extension(ipython):
    if KoatlMagicAssign in ipython.input_transformer_manager.token_transformers:
        ipython.token_transformers.remove(0, KoatlMagicAssign)
    if KoatlSystemAssign in ipython.input_transformer_manager.token_transformers:
        ipython.token_transformers.remove(1, KoatlSystemAssign)
    if source_code_transformer in ipython.input_transformers_post:
        ipython.input_transformers_post.remove(source_code_transformer)

    print("koatl disabled.")
