import re
import collections
import .functional.Result

export Regex = class:
    __slots__ = ("pattern",)

    __init__ = (self, pattern) =>
        pattern match:
            re.Pattern() => self.pattern = pattern
            str() => self.pattern = re.compile(pattern)
            Regex(pattern=pattern) => self.pattern = pattern
            default => raise TypeError(
                f"Expected a str or pattern, but got {type(pattern).__name__}"
            )

    __repr__ = self => f"Regex({self.pattern})"

    match = (self, string) =>
        if not isinstance(string, str):
            raise TypeError(f"Expected a string, but got {type(string).__name__}")

        self.pattern.match(string).result.map(Regex.Match)

    search = (self, string) =>
        if not isinstance(string, str):
            raise TypeError(f"Expected a string, but got {type(string).__name__}")

        self.pattern.search(string).result.map(Regex.Match)


    Match = collections.abc.Sequence.register& class:
        __slots__ = ("_match",)
        __match_args__ = ("match",)

        __init__ = (self, match) =>
            if not isinstance(match, re.Match):
                raise TypeError(f"Expected a re.Match, but got {type(match).__name__}")

            self._match = match

        __iter__ = self => iter(self._match.groups())
        __len__ = self => len(self._match.groups())
        __getitem__ = (self, index) => self._match[index]

        match = property& self => self._match.group(0)

        __repr__ = self => f"Match({self._match})"


Extension.method(str, "match")& (regex, str) => Regex(regex).match(str)


# Note: the below methods have arguments in reverse order to Python's re module.

Extension.method(str, "matches")& (str, regex) =>
    Regex(regex).match(str)

Extension.method(str, "search")& (str, regex) =>
    Regex(regex).search(str)
