from . import _rs

def transpile(*args, **kwargs):
    try:
        return _rs.transpile(*args, **kwargs)
    except SyntaxError as e:
        raise SyntaxError(e.args[0].decode("utf8")) from None