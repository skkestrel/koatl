import util.(assert_eq, fail)
import koatl.std.re.(Pattern, Match)
import koatl.std.control.(Ok, Err)

# Basic pattern compilation and matching
test_pattern_from_string = () =>
    let pat = Pattern("hello")
    assert_eq(pat.pattern.pattern, "hello")

test_pattern_from_pattern = () =>
    import re
    let py_pat = re.compile("test")
    let pat = Pattern(py_pat)
    assert_eq(pat.pattern.pattern, "test")

test_pattern_from_pattern_copy = () =>
    let pat1 = Pattern("copy")
    let pat2 = Pattern(pat1)
    assert_eq(pat2.pattern.pattern, "copy")

# Pattern.match() tests
test_pattern_match_success = () =>
    let pat = Pattern("\\d+")
    let result = pat.match("123abc")
    assert_eq(result.ok, True)

test_pattern_match_failure = () =>
    let pat = Pattern("\\d+")
    let result = pat.match("abc")
    assert_eq(result.ok, False)

test_pattern_match_groups = () =>
    let pat = Pattern("(\\d+)\\.(\\d+)")
    if pat.match("123.456") matches Ok(m):
        assert_eq(list(m), ["123", "456"])
        assert_eq(m.match, "123.456")

test_pattern_match_no_groups = () =>
    let pat = Pattern("hello")
    if pat.match("hello world") matches Ok(m):
        assert_eq(list(m), [])
        assert_eq(m.match, "hello")

# String extension method tests
test_str_match_success = () =>
    let result = "\\w+".match("hello")
    assert_eq(result.ok, True)
    if result matches Ok(m):
        assert_eq(m.match, "hello")

test_str_match_failure = () =>
    let result = "[a-z]+".match("123")
    assert_eq(result.ok, False)

test_str_match_with_groups = () =>
    let result = "(\\d{3})-(\\d{4})".match("123-4567")
    if result matches Ok(m):
        assert_eq(m.match, "123-4567")
        assert_eq(m[1], "123")
        assert_eq(m[2], "4567")
        assert_eq(list(m), ["123", "4567"])

test_str_matches_alias = () =>
    # matches is an alias for match
    let result1 = "\\w+".match("test")
    let result2 = "test".matches("\\w+")
    assert_eq(result1.ok, True)
    assert_eq(result2.ok, True)

# Match object tests
test_match_sequence_behavior = () =>
    let pat = Pattern("(\\w+)@(\\w+)\\.(\\w+)")
    if pat.match("user@example.com") matches Ok(m):
        assert_eq(len(m), 3)
        assert_eq(m[1], "user")
        assert_eq(m[2], "example")
        assert_eq(m[3], "com")

test_match_iteration = () =>
    let pat = Pattern("(\\d+),(\\d+),(\\d+)")
    if pat.match("10,20,30") matches Ok(m):
        let groups = []
        for group in m:
            groups.append(group)
        assert_eq(groups, ["10", "20", "30"])

test_match_full_match_property = () =>
    let pat = Pattern("hello (\\w+)")
    if pat.match("hello world!") matches Ok(m):
        assert_eq(m.match, "hello world")
        assert_eq(m[1], "world")

# Complex regex patterns
test_email_pattern = () =>
    let email_pat = Pattern("([\\w.-]+)@([\\w.-]+)\\.([a-z]{2,})")
    if email_pat.match("user.name@example.co.uk") matches Ok(m):
        assert_eq(m[1], "user.name")
        assert_eq(m[2], "example.co")
        assert_eq(m[3], "uk")

test_url_pattern = () =>
    let url_pat = Pattern("https?://([\\w.-]+)(?:/([\\w/.-]*))?")
    if url_pat.match("https://example.com/path/to/page") matches Ok(m):
        assert_eq(m[1], "example.com")
        assert_eq(m[2], "path/to/page")

test_phone_pattern = () =>
    let phone_pat = Pattern("\\(?(\\d{3})\\)?[-.]?(\\d{3})[-.]?(\\d{4})")
    if phone_pat.match("(555) 123-4567") matches Ok(m):
        assert_eq(list(m), ["555", "123", "4567"])

# Edge cases
test_empty_pattern = () =>
    let pat = Pattern("")
    let result = pat.match("anything")
    assert_eq(result.ok, True)
    if result matches Ok(m):
        assert_eq(m.match, "")
        assert_eq(list(m), [])

test_match_at_start_only = () =>
    let pat = Pattern("\\d+")
    # match() only matches at the beginning of the string
    assert_eq(pat.match("123abc").ok, True)
    assert_eq(pat.match("abc123").ok, False)

test_special_characters = () =>
    let pat = Pattern("\\$\\d+\\.\\d{2}")
    if pat.match("$19.99 total") matches Ok(m):
        assert_eq(m.match, "$19.99")

test_unicode_pattern = () =>
    let pat = Pattern("[α-ω]+")
    if pat.match("αβγδε") matches Ok(m):
        assert_eq(m.match, "αβγδε")

# Named groups aren't directly exposed in Match, but the full match object is accessible
test_multiple_groups = () =>
    let pat = Pattern("(\\d{4})-(\\d{2})-(\\d{2})")
    if pat.match("2025-12-13") matches Ok(m):
        assert_eq(m[1], "2025")
        assert_eq(m[2], "12")
        assert_eq(m[3], "13")
        assert_eq(len(m), 3)

# Result monad integration
test_result_map = () =>
    let result = "(\\d+)".match("123").map(m => m[1])
    if result matches Ok(value):
        assert_eq(value, "123")

test_result_chaining = () =>
    let result = "(\\d+)\\.(\\d+)".match("3.14")
        .map(m => [m[1], m[2]])
        .map([a, b] => [int(a), int(b)])

    if result matches Ok([major, minor]):
        assert_eq(major, 3)
        assert_eq(minor, 14)

test_result_or_else = () =>
    let result = "\\d+".match("abc")
    if result matches Err(_):
        assert_eq(True, True)
    else:
        fail()

# Pattern matching with matches expression
test_matches_expression_success = () =>
    if "(\\d+)".match("123") matches Ok(m):
        assert_eq(m[1], "123")
    else:
        fail()

test_matches_expression_failure = () =>
    if "(\\d+)".match("abc") matches Ok(_):
        fail()
    else:
        assert_eq(True, True)

# Practical examples
test_extract_version_numbers = () =>
    let version_pat = Pattern("v?(\\d+)\\.(\\d+)\\.(\\d+)")
    if version_pat.match("v1.2.3") matches Ok(m):
        let [major, minor, patch] = list(m)
        assert_eq(major, "1")
        assert_eq(minor, "2")
        assert_eq(patch, "3")

test_parse_csv_line = () =>
    let csv_pat = Pattern("([^,]+),([^,]+),([^,]+)")
    if csv_pat.match("John,Doe,30") matches Ok(m):
        assert_eq(list(m), ["John", "Doe", "30"])

test_validate_hex_color = () =>
    let hex_pat = Pattern("#([0-9a-fA-F]{6})")
    assert_eq(hex_pat.match("#FF5733").ok, True)
    assert_eq(hex_pat.match("#GG5733").ok, False)
    assert_eq(hex_pat.match("FF5733").ok, False)

test_extract_quoted_string = () =>
    let quote_pat = Pattern('"([^"]*)"')
    if quote_pat.match('"hello world"') matches Ok(m):
        assert_eq(m[1], "hello world")
