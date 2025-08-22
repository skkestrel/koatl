from collections import defaultdict
import koatl
from ipykernel.compiler import XCachingCompiler
from ipykernel.zmqshell import ZMQInteractiveShell
from IPython.core.interactiveshell import *
from IPython.core.ultratb import FrameInfo

_source_map_override = defaultdict(dict)
_orig_frame_info_init = None

if _orig_frame_info_init is None:
    _orig_frame_info_init = FrameInfo.__init__

    def _new_init(self, *args, **kwargs):
        _orig_frame_info_init(self, *args, **kwargs)

        if self.filename in _source_map_override:
            mapper = _source_map_override[self.filename]

            old_lineno = self.lineno
            self.lineno = mapper.get(old_lineno, 1)
            if self._sd is not None:
                self._sd.lineno = mapper.get(old_lineno, 1)

    FrameInfo.__init__ = _new_init


class KoatlXCachingCompiler(XCachingCompiler):
    def cache(self, transformed_code, number=0, raw_code=None):
        if not hasattr(self, "shell"):
            return super().cache(transformed_code, number, raw_code)

        filename = super().cache(self.shell._koatl_cell, number, raw_code)

        sourcemap = {}
        last_seen_line = 1
        for i in range(len(transformed_code.splitlines()) + 1):
            if i in self.shell._koatl_sourcemap:
                sourcemap[i] = self.shell._koatl_sourcemap[i]
                last_seen_line = self.shell._koatl_sourcemap[i]
            else:
                sourcemap[i] = last_seen_line

        _source_map_override[filename] = sourcemap

        return filename


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

        cell, sourcemap = koatl.transpile_raw(cell, mode="interactive")
        self._koatl_sourcemap = sourcemap

        return cell
