from ipykernel.ipkernel import IPythonKernel
from IPython.core.inputtransformer2 import leading_empty_lines, leading_indent
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
        if self.shell:
            return self.shell.banner
        return None

    def __init__(self, **kwargs):
        super().__init__(**kwargs)

        # TODO: these depend on the python tokenizer which don't work

        # itm.token_transformers.insert(0, CoatlMagicAssign)
        # itm.token_transformers.insert(1, CoatlEscapedCommand)
        # itm.token_transformers.insert(2, CoatlSystemAssign)

        itm = self.shell.input_transformer_manager

        itm.do_token_transforms = lambda lines: lines
        itm.line_transforms.insert(0, coatl_cell_magic)

        import_prelude(self.shell.user_ns)

    async def do_execute(self, code, *args, **kwargs):
        return await super().do_execute(code, *args, **kwargs)