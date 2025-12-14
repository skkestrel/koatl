def assert_eq(received, expected):
    assert received == expected, f"Expected {expected}, but got {received}"

def assert_contains(haystack, needle):
    assert needle in haystack, f"Expected to find {needle} in {haystack}"

def assert_true(value, message=None):
    assert value is True, message or f"Expected True, but got {value}"

def fail(message):
    raise AssertionError(message)