import koatl
import builtins as builtin_mod
import sys
import warnings
from typing import List as ListType, Dict as DictType, Any as AnyType
from typing import Optional, Sequence, Tuple
from ipykernel.compiler import XCachingCompiler
from ipykernel.zmqshell import ZMQInteractiveShell
from IPython.core.error import InputRejected
from IPython.core.interactiveshell import *
from IPython.core.ultratb import FrameInfo

_source_map_override = {}
_orig_frame_info_init = None

if _orig_frame_info_init is None:
    _orig_frame_info_init = FrameInfo.__init__

    def _new_init(self, *args, **kwargs):
        _orig_frame_info_init(self, *args, **kwargs)

        # only override for ipykernel frames
        if "ipykernel_" in self.filename:
            old_lineno = self.lineno
            self.lineno = _source_map_override.get(old_lineno, 0)
            self._sd.lineno = _source_map_override.get(old_lineno, 0)

    FrameInfo.__init__ = _new_init


class KoatlXCachingCompiler(XCachingCompiler):
    def cache(self, transformed_code, number=0, raw_code=None):
        return super().cache(self.shell._koatl_cell, number, raw_code)


class KoatlShell(ZMQInteractiveShell):
    def __init__(self, *args, **kwargs):
        kwargs["compiler_class"] = KoatlXCachingCompiler
        super().__init__(*args, **kwargs)
        self.compile.shell = self

    def _get_exc_info(self, exc_tuple=None):
        ret = super()._get_exc_info(exc_tuple)
        self._exc_info = ret
        return ret

    def transform_cell(self, raw_cell):
        cell = super().transform_cell(raw_cell)

        self._koatl_cell = cell
        (cell, sourcemap) = koatl.transpile(cell, mode="interactive", sourcemap=True)
        _source_map_override.clear()

        last_seen_line = 0
        for i in range(len(cell.splitlines()) + 1):
            if i in sourcemap:
                _source_map_override[i] = sourcemap[i]
                last_seen_line = sourcemap[i]
            else:
                _source_map_override[i] = last_seen_line

        return cell
