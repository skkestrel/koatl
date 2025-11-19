import pytest
import koatl.cli
from pathlib import Path


def get_test_data():
    data_dirs = [
        Path(__file__).parent / "fail-parse",
    ]

    test_cases = []
    for data_dir in data_dirs:
        for file_path in data_dir.glob("*.tl"):
            test_cases.append(pytest.param(file_path, id=str(file_path)))

    return test_cases


@pytest.mark.parametrize("test_file", get_test_data())
def test_parse(test_file):
    try:
        koatl.cli.transpile_from_path(test_file, mode="script")
    except:
        return

    raise AssertionError("Expected parse to fail but it succeeded.")
