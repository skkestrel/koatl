import functools
import re

__all__ = ["Record"]


class Record(dict):
    def __repr__(self):
        return self._repr_with_visited(set())

    def __getattr__(self, name):
        try:
            attr = self[name]
        except KeyError:
            return super().__getattr__(name)

        if hasattr(attr, "_property"):
            return attr(self)
        elif hasattr(attr, "_method"):
            return functools.partial(attr, self)

        return attr

    def __hash__(self):
        return hash(tuple(sorted(self.items())))

    def __eq__(self, other):
        return super().__eq__(other)

    @property
    def len(self):
        return len(self)

    @staticmethod
    def method(fn):
        fn._method = True
        return fn

    @staticmethod
    def property(fn):
        fn._property = True
        return fn

    def _repr_with_visited(self, visited):
        # Handle cycles by checking if this object is already being processed
        obj_id = id(self)
        if obj_id in visited:
            return "{...}"

        visited.add(obj_id)
        try:
            if not self:
                return "{}"

            items = []
            for key, value in self.items():
                key_str = self._format_key(key)

                # Handle value representation with cycle detection
                if isinstance(value, Record):
                    value_str = value._repr_with_visited(visited.copy())
                elif hasattr(value, "__dict__") and hasattr(value, "__class__"):
                    # For other objects that might contain cycles, use a simple repr
                    value_str = repr(value)
                else:
                    value_str = repr(value)

                items.append(f"{key_str}: {value_str}")

            return "{" + ", ".join(items) + "}"
        finally:
            visited.remove(obj_id)

    def _format_key(self, key):
        if isinstance(key, str):
            if self._is_identifier(key):
                # If key is an identifier, drop the quotes
                return key
            return f'"{key}"'

        elif isinstance(key, (int, float, bool, type(None))):
            # If key is a literal like 0, 1, True, False, None, use repr
            return repr(key)

        else:
            # Otherwise, use f"({repr(key)})"
            return f"({repr(key)})"

    def _is_identifier(self, s):
        return (
            isinstance(s, str)
            and re.match(r"^[a-zA-Z_][a-zA-Z0-9_]*$", s)
            and s not in koatl_keywords
        )


koatl_keywords = {
    "if",
    "else",
    "while",
    "for",
    "def",
    "class",
    "return",
    "import",
    "from",
    "as",
    "with",
    "try",
    "except",
    "finally",
    "raise",
    "assert",
    "async",
    "await",
}
