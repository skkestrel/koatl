export read_file = (filename) =>
    with desc = open(filename, "r"):
        desc.read()