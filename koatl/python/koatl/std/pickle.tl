import pickle
import koatl.std.io

export write_file = (filename, obj, protocol=None) =>
    with f = open(filename, "wb"):
        pickle.dump(obj, f, protocol=protocol ?? pickle.DEFAULT_PROTOCOL)

export load_file = filename =>
    with f = open(filename, "rb"):
        pickle.load(f)