import json
import koatl.std.io
import koatl.std.data.Record

export JSONEncoder = class(json.JSONEncoder):
    default = (self, o) =>
        o match:
            Record() => o.__dict__
            default => super().default(o)

export JSONDecoder = class(json.JSONDecoder):
    __init__ = (self, **kwargs) =>
        let object_hook = d =>
            d match:
                dict() => Record(d)
                default => d

        super().__init__(object_hook=object_hook, **kwargs)

export parse = json_str => JSONDecoder().decode(json_str)

export stringify = (obj, **kwargs) => JSONEncoder(**kwargs).encode(obj)

export pretty = obj => stringify(obj, indent=2)

export compact = obj => stringify(obj, separators=(",", ":"))

export read_file = filename =>
    let s = io.read_file(filename)
    parse(s)

export write_file = (filename, obj, indent=2, sort_keys=True) =>
    let s = stringify(obj, indent=indent, sort_keys=sort_keys)
    io.write_file(filename, s)