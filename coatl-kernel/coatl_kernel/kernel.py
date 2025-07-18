from ipykernel.ipkernel import IPythonKernel
from IPython.core.inputtransformer2 import find_last_indent
from coatl.notebook import source_code_transformer, import_prelude
from coatl.notebook.magic import coatl_cell_magic
from .shell import CoatlShell

class CoatlKernel(IPythonKernel):
    implementation = 'Coatl'
    implementation_version = '1.0'
    language = 'coatl'
    language_version = '0.1'
    language_info = {
        'name': 'coatl',
        'mimetype': 'text/x-coatl',
        'file_extension': '.tl',
    }

    shell_class = CoatlShell

    @property
    def banner(self):
        return "Coatl Kernel - Interactive Coatl Environment"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)

        # TODO: these depend on the python tokenizer which don't work

        # itm.token_transformers.insert(0, CoatlMagicAssign)
        # itm.token_transformers.insert(1, CoatlEscapedCommand)
        # itm.token_transformers.insert(2, CoatlSystemAssign)

        itm = self.shell.input_transformer_manager

        itm.do_token_transforms = lambda lines: lines
        itm.line_transforms.insert(0, coatl_cell_magic)
        itm.check_complete = self._itm_check_complete

        import_prelude(self.shell.user_ns)

    def _itm_check_complete(self, cell):
        import re
        lines = cell.splitlines(keepends=True)

        if lines[-1].endswith(":") or lines[-1].endswith("=>") or lines[-1].endswith("[") or lines[-1].endswith("("):
            return "incomplete", find_last_indent(lines) + 2

        if lines[-1].startswith(" "):
            if len(lines) > 2 and re.match(r"^\s*$", lines[-2]) and re.match(r"^\s*$", lines[-1]):
                return "complete", None
            return "incomplete", find_last_indent(lines)

        return "complete", None

    async def do_execute(self, code, *args, **kwargs):
        return await super().do_execute(code, *args, **kwargs)