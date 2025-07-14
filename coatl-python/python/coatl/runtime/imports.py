import sys
import os
import linecache
from importlib.abc import MetaPathFinder, Loader
from importlib.util import spec_from_loader

from coatl._rs import transpile

class TlFinder(MetaPathFinder):
    def find_spec(self, fullname, path, target=None):
        if path is None:
            path = sys.path

        module_name = fullname.split('.')[-1]
        for entry in path:
            if os.path.isdir(entry):
                for filename in os.listdir(entry):
                    if filename == f"{module_name}.tl":
                        filepath = os.path.join(entry, filename)
                        return spec_from_loader(fullname, TlLoader(filepath))
        return None

class TlLoader(Loader):
    def __init__(self, filepath):
        self.filepath = filepath

    def create_module(self, spec):
        return None  # Use default module creation

    def exec_module(self, module):
        module.__file__ = self.filepath

        with open(self.filepath, 'r') as f:
            source_code = f.read()

        linecache.cache[self.filepath] = (
            len(source_code),
            None,
            [line + '\n' for line in source_code.splitlines()],
            self.filepath,
        )

        transpiled_code = transpile(source_code)
        code = compile(transpiled_code, self.filepath, "exec")

        exec(code, module.__dict__)


def install_hook():
    sys.meta_path.insert(0, TlFinder())