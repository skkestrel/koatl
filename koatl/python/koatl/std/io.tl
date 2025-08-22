export read_file = (filename, binary=False) =>
    with desc = open(filename, binary then "rb" else "r"):
        desc.read()

export write_file = (filename, content, binary=False) =>
    with desc = open(filename, binary then "wb" else "w"):
        desc.write(content)