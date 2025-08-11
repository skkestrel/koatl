import pytest
import sys
import os
import koatl.cli
from pathlib import Path

sys.path.append(str(Path(__file__).parent / "e2e"))
os.chdir(Path(__file__).parent / "e2e")


def get_test_data(dirs):
    data_dirs = [
        Path(__file__).parent / "e2e" / dirs,
    ]

    test_cases = []
    for data_dir in data_dirs:
        for file_path in data_dir.glob("*.tl"):
            test_cases.append(pytest.param(file_path, id=str(file_path)))

    return test_cases


@pytest.mark.parametrize("test_file", get_test_data("base"))
def test_e2e_native_emit_base(test_file):
    e2e_native_emit(test_file, "no_prelude")


@pytest.mark.parametrize("test_file", get_test_data("prelude"))
def test_e2e_native_emit_prelude(test_file):
    e2e_native_emit(test_file, "script")


@pytest.mark.parametrize("test_file", get_test_data("base"))
def test_e2e_base(test_file):
    e2e(test_file, "no_prelude")


@pytest.mark.parametrize("test_file", get_test_data("prelude"))
def test_e2e_prelude(test_file):
    e2e(test_file, "script")


def e2e_native_emit(test_file, mode):
    import linecache

    with open(test_file, "r") as f:
        source = f.read()
    source, source_map = koatl.transpile_raw(source, mode=mode)

    global_dict = {}

    try:
        linecache.cache["<string>"] = (
            len(source),
            None,
            source.splitlines(),
            "<string>",
        )
        codeobj = compile(source, "<string>", "exec")
        exec(codeobj, global_dict, global_dict)
    except Exception as e:
        print(source)
        raise

    print("end", test_file)


def e2e(test_file, mode):
    koatl.cli.run_from_path(test_file, mode=mode)
