import sys
import os
import linecache
from importlib.abc import MetaPathFinder, Loader
from importlib.util import spec_from_loader

from koatl import transpile


class TlFinder(MetaPathFinder):
    def find_spec(self, fullname, path, target=None):
        if path is None:
            path = sys.path

        module_name = fullname.split(".")[-1]

        for entry in path:
            # 1. Check for a regular module file: {entry}/{module_name}.tl
            file_path = os.path.join(entry, f"{module_name}.tl")
            if os.path.isfile(file_path):
                return spec_from_loader(fullname, TlLoader(file_path))

            # 2. Check for a package: {entry}/{module_name}/__init__.tl
            package_path = os.path.join(entry, module_name)
            init_path = os.path.join(package_path, "__init__.tl")

            if os.path.isdir(package_path) and os.path.isfile(init_path):
                spec = spec_from_loader(fullname, TlLoader(init_path), is_package=True)
                spec.submodule_search_locations = [package_path]
                return spec

        return None


class TlLoader(Loader):
    def __init__(self, filepath):
        self.filepath = filepath

    def create_module(self, spec):
        return None

    def exec_module(self, module):
        module.__file__ = self.filepath

        with open(self.filepath, "r") as f:
            source_code = f.read()

        linecache.cache[self.filepath] = (
            len(source_code),
            None,
            [line + "\n" for line in source_code.splitlines()],
            self.filepath,
        )

        ver = (sys.version_info.major, sys.version_info.minor)

        if module.__name__.startswith("koatl"):
            transpiled_code = transpile(
                source_code,
                mode="no_prelude",
                filename=self.filepath,
                target_version=ver,
            )
        else:
            transpiled_code = transpile(
                source_code, mode="module", filename=self.filepath, target_version=ver
            )

        code = compile(transpiled_code, self.filepath, "exec")

        exec(code, module.__dict__)


def install_hook():
    sys.meta_path.insert(0, TlFinder())
