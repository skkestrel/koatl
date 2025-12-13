def assert_eq(received, expected):
    assert received == expected, f"Expected {expected}, but got {received}"

def assert_contains(haystack, needle):
    assert needle in haystack, f"Expected to find {needle} in {haystack}"
