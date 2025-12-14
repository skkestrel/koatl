"""
Pytest configuration for Koatl tests.
This allows pytest to discover and execute .tl files as test modules.
"""
import pytest
import koatl
import sys
from pathlib import Path

# Add e2e directory to Python path so util module can be imported
sys.path.insert(0, str(Path(__file__).parent / "e2e"))


def pytest_collect_file(parent, file_path):
    """
    Custom collector to discover .tl files as test modules.
    """
    if file_path.suffix == ".tl" and file_path.name.startswith("test_"):
        return TlModule.from_parent(parent, path=file_path)


class TlModule(pytest.Module):
    """
    Custom pytest Module for .tl files.
    Transpiles the .tl file and collects test functions from it.
    """
    
    def collect(self):
        """
        Transpile the .tl file and collect test functions.
        """
        with open(self.path, "r") as f:
            source = f.read()
        
        try:
            # Transpile to Python AST in module mode
            py_ast = koatl.transpile(source, mode="module")
            
            # Create a namespace for the module
            module_name = self.path.stem
            module_namespace = {
                "__name__": module_name,
                "__file__": str(self.path),
            }
            
            # Compile and execute the Python AST in the namespace
            # This defines all test_* functions
            code_obj = compile(py_ast, str(self.path), "exec")
            exec(code_obj, module_namespace)
            
            # Collect test functions from the namespace
            for name, obj in module_namespace.items():
                if name.startswith("test_") and callable(obj):
                    func = pytest.Function.from_parent(
                        self, 
                        name=name
                    )
                    func.obj = obj
                    yield func
        except Exception as e:
            # If transpilation fails, create an error item
            raise self.CollectError(f"Error transpiling {self.path}: {e}") from e
