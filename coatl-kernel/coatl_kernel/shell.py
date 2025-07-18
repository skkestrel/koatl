import coatl
import builtins as builtin_mod
import sys
import warnings
from typing import List as ListType, Dict as DictType, Any as AnyType
from typing import Optional, Sequence, Tuple
from ipykernel.compiler import XCachingCompiler
from ipykernel.zmqshell import ZMQInteractiveShell
from IPython.core.error import InputRejected
from IPython.core.interactiveshell import *

class CoatlXCachingCompiler(XCachingCompiler):
    def cache(self, transformed_code, number=0, raw_code=None):
        print(self.shell._coatl_cell)
        return super().cache(self.shell._coatl_cell, number, raw_code)


class CoatlShell(ZMQInteractiveShell):
    def __init__(self, *args, **kwargs):
        kwargs["compiler_class"] = CoatlXCachingCompiler
        super().__init__(*args, **kwargs)
        self.compile.shell = self

    def transform_cell(self, raw_cell):
        cell = super().transform_cell(raw_cell)

        self._coatl_cell = cell

        py_ast = coatl.transpile(cell, mode="interactive")
        cell = ast.unparse(py_ast)

        return cell