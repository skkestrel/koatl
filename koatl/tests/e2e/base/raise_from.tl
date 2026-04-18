# raise from — exception chaining
try:
    try:
        raise ValueError("original")
    except ValueError() as e =>
        raise RuntimeError("wrapped") from e
except RuntimeError() as e =>
    assert str(e) == "wrapped"
    assert type(e.__cause__) == ValueError
    assert str(e.__cause__) == "original"

# raise from None — suppress context
try:
    try:
        raise ValueError("hidden")
    except ValueError() =>
        raise RuntimeError("clean") from None
except RuntimeError() as e =>
    assert str(e) == "clean"
    assert e.__cause__ is None
    assert e.__suppress_context__ == True
