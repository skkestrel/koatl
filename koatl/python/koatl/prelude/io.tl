export read = (filename) =>
    let desc = open(filename, "r")
    let data = desc.read()
    desc.close()
    data