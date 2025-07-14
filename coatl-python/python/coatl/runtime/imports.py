import sys
import os
import subprocess
from importlib.abc import MetaPathFinder, Loader
from importlib.util import spec_from_loader

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
        try:
            result = subprocess.run(
                ["coatl", "trans", self.filepath],
                capture_output=True,
                text=True,
                check=True
            )

            exec(result.stdout, module.__dict__)
        except Exception as e:
            raise e from None

def install_hook():
    sys.meta_path.insert(0, TlFinder())