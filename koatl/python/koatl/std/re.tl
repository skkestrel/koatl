import re
import collections
import .control.Result

export Pattern = class:
    __slots__ = ("pattern",)

    __init__ = (self, pattern) =>
        pattern match:
            re.Pattern() => self.pattern = pattern
            str() => self.pattern = re.compile(pattern)
            Pattern(pattern=pattern) => self.pattern = pattern
            default => raise TypeError(
                f"Expected a str or pattern, but got {type(pattern).__name__}"
            )

    __repr__ = self => f"Pattern({self.pattern})"

    match = (self, string, flags=0) =>
        if not isinstance(string, str):
            raise TypeError(f"Expected a string, but got {type(string).__name__}")

        self.pattern.match(string, flags).result.map(Match)

export Match = collections.abc.Sequence.register! class:
    __slots__ = ("_match",)
    __match_args__ = ("match",)

    __init__ = (self, match) =>
        if not isinstance(match, re.Match):
            raise TypeError(f"Expected a re.Match, but got {type(match).__name__}")

        self._match = match

    __iter__ = self => iter(self._match.groups())
    __len__ = self => len(self._match.groups())
    __getitem__ = (self, index) => self._match[index]

    match = property! self => self._match.group(0)

    __repr__ = self => f"Match({self._match})"
