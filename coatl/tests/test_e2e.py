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
            
    return test_cases[:5]

@pytest.mark.parametrize("test_file", get_test_data())
def test_e2e(test_file):
    print("start", test_file)
    coatl.cli.run_from_path(test_file, mode="script")
    print("end", test_file)