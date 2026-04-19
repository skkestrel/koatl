import util.assert_eq

# Basic escape sequences
assert_eq("\n", "\n")
assert_eq("\t", "\t")
assert_eq("\r", "\r")
assert_eq("\\", "\\")
assert_eq("\"", "\"")
assert_eq("\'", "'")

# Additional Python escape sequences
assert_eq("\a", "\a")
assert_eq("\b", "\b")
assert_eq("\f", "\f")
assert_eq("\v", "\v")

# Hex escapes
assert_eq("\x41", "A")
assert_eq("\x61", "a")
assert_eq("\x00", "\0")
assert_eq("\x0a", "\n")

# Unicode escapes (4 hex digits)
assert_eq("\u0041", "A")
assert_eq("\u00e9", "é")
assert_eq("\u2603", "☃")

# Unicode escapes (8 hex digits)
assert_eq("\U00000041", "A")
assert_eq("\U0001F600", "😀")

# Octal escapes
assert_eq("\0", "\x00")
assert_eq("\7", "\x07")
assert_eq("\77", "?")
assert_eq("\101", "A")
assert_eq("\141", "a")
assert_eq("\012", "\n")

# Mixed in strings
assert_eq("hello\tworld", "hello\tworld")
assert_eq("line1\nline2", "line1\nline2")
assert_eq("path\\to\\file", "path\\to\\file")

# Escape sequences in f-strings
x = "world"
assert_eq(f"hello\t{x}", "hello\tworld")
assert_eq(f"a\nb", "a\nb")
