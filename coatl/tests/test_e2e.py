import pytest
import sys
import coatl.cli
from pathlib import Path

sys.path.append(str(Path(__file__).parent / "e2e"))

def get_test_data():
    data_dir = Path(__file__).parent / "e2e"
    
    test_cases = []
    for file_path in data_dir.glob("*.tl"):
        test_cases.append(pytest.param(file_path, id=str(file_path)))
        print(file_path)
            
    return test_cases

@pytest.mark.parametrize("test_file", get_test_data())
def test_e2e(test_file):
    coatl.cli.run_from_path(test_file, mode="script")

@pytest.mark.parametrize("test_file", get_test_data())
def test_e2e_native_emit(test_file):
    import linecache
    
    with open(test_file, "r") as f:
        source = f.read()
    source, source_map = coatl.transpile(source, mode="script", sourcemap=True)

    global_dict = {}

    try:
        linecache.cache["<string>"] = (len(source), None, source.splitlines(), "<string>")
        codeobj = compile(source, "<string>", "exec")
        exec(codeobj, global_dict, global_dict)
    except Exception as e:
        print(source)
        raise

    print("end", test_file)