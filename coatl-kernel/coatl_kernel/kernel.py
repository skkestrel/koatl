from ipykernel.ipkernel import IPythonKernel
import ast
from coatl.notebook import source_code_transformer
from coatl.notebook.magic import coatl_cell_magic, CoatlSystemAssign, CoatlMagicAssign, CoatlEscapedCommand


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

    @property
    def banner(self):
        if self.shell:
            return self.shell.banner
        return None

    def __init__(self, **kwargs):
        super().__init__(**kwargs)

        self.shell.input_transformers_post.append(source_code_transformer)
        self.shell.input_transformer_manager.line_transforms.insert(0, coatl_cell_magic)
        self.shell.input_transformer_manager.token_transformers.insert(0, CoatlMagicAssign)
        self.shell.input_transformer_manager.token_transformers.insert(1, CoatlEscapedCommand)
        self.shell.input_transformer_manager.token_transformers.insert(2, CoatlSystemAssign)