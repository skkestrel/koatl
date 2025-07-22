from ipykernel.ipkernel import IPythonKernel
from IPython.core.inputtransformer2 import find_last_indent
from koatl.notebook import source_code_transformer, import_prelude
from koatl.notebook.magic import koatl_cell_magic
from .shell import KoatlShell


class KoatlKernel(IPythonKernel):
    implementation = "Koatl"
    implementation_version = "1.0"
    language = "koatl"
    language_version = "0.1"
    language_info = {
        "name": "koatl",
        "mimetype": "text/x-koatl",
        "file_extension": ".tl",
    }

    shell_class = KoatlShell

    @property
    def banner(self):
        return "Koatl Kernel - Interactive Koatl Environment"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)

        # TODO: these depend on the python tokenizer which don't work

        # itm.token_transformers.insert(0, KoatlMagicAssign)
        # itm.token_transformers.insert(1, KoatlEscapedCommand)
        # itm.token_transformers.insert(2, KoatlSystemAssign)

        itm = self.shell.input_transformer_manager

        itm.do_token_transforms = lambda lines: lines
        itm.line_transforms.insert(0, koatl_cell_magic)
        itm.check_complete = self._itm_check_complete

        import_prelude(self.shell.user_ns)

    def _itm_check_complete(self, cell):
        import re

        lines = cell.splitlines(keepends=True)

        if (
            lines[-1].endswith(":")
            or lines[-1].endswith("=>")
            or lines[-1].endswith("[")
            or lines[-1].endswith("(")
        ):
            return "incomplete", find_last_indent(lines) + 2

        if lines[-1].startswith(" "):
            if (
                len(lines) > 2
                and re.match(r"^\s*$", lines[-2])
                and re.match(r"^\s*$", lines[-1])
            ):
                return "complete", None
            return "incomplete", find_last_indent(lines)

        return "complete", None

    async def do_execute(self, code, *args, **kwargs):
        return await super().do_execute(code, *args, **kwargs)
