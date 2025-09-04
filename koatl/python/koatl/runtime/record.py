import collections
from collections.abc import MutableMapping
import functools
import inspect
import re

_property = property

__all__ = ["Record"]


@collections.abc.MutableMapping.register
class Record:
    def __init__(self, data=None, /, **kwargs):
        if data is None:
            self.__dict__ = kwargs
        else:
            self.__dict__ = dict(data, **kwargs)

    @classmethod
    def from_dict_ref(cls, dict_obj):
        self = cls.__new__(cls)
        self.__dict__ = dict_obj
        return self

    @staticmethod
    def method(fn):
        fn._method = True
        return fn

    @staticmethod
    def property(fn):
        fn._property = True
        return fn

    def __getattribute__(self, name):
        dict = object.__getattribute__(self, "__dict__")

        try:
            attr = dict[name]

            if hasattr(attr, "_property"):
                return attr(self)
            elif hasattr(attr, "_method"):
                return functools.partial(attr, self)
        except KeyError:
            pass

        return object.__getattribute__(self, name)

    @_property
    def iter(self):
        return self.items()

    # MutableMapping
    def __len__(self):
        return len(self.__dict__)

    def __getitem__(self, key):
        try:
            return self.__dict__[key]
        except KeyError as e:
            raise e

    def __setitem__(self, key, item):
        self.__dict__[key] = item

    def __delitem__(self, key):
        del self.__dict__[key]

    def __iter__(self):
        return iter(self.__dict__)

    def __contains__(self, key):
        return key in self.__dict__

    def get(self, key, default=None):
        return self.__dict__.get(key, default)

    def items(self):
        return self.__dict__.items()

    def keys(self):
        return self.__dict__.keys()

    def values(self):
        return self.__dict__.values()

    # dict
    def __repr__(self):
        return repr(self.__dict__)

    def __or__(self, other):
        if isinstance(other, Record):
            return self.__class__(self.__dict__ | other.__dict__)
        if isinstance(other, dict):
            return self.__class__(self.__dict__ | other)
        return NotImplemented

    def __ror__(self, other):
        if isinstance(other, Record):
            return self.__class__(other.__dict__ | self.__dict__)
        if isinstance(other, dict):
            return self.__class__(other | self.__dict__)
        return NotImplemented

    def __ior__(self, other):
        if isinstance(other, Record):
            self.__dict__ |= other.__dict__
        else:
            self.__dict__ |= other
        return self

    def __copy__(self):
        inst = self.__class__.__new__(self.__class__)
        inst.__dict__ = self.__dict__.copy()
        return inst

    def copy(self):
        if self.__class__ is Record:
            return Record(self.__dict__.copy())
        import copy

        data = self.__dict__
        try:
            self.__dict__ = {}
            c = copy.copy(self)
        finally:
            self.__dict__ = data
        c.update(self)
        return c

    @_property
    def len(self):
        return len(self)

    # Other

    def __repr__(self):
        return self._repr_with_visited(set())

    def __hash__(self):
        return hash(tuple(sorted(self.items())))

    def __eq__(self, other):
        if isinstance(other, Record):
            return self.__dict__ == other.__dict__

        if isinstance(other, dict):
            return self.__dict__ == other

        return NotImplemented

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

        elif isinstance(key, (int, float, bool, type(None), tuple)):
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


for key, value in MutableMapping.__dict__.items():
    # copy over mixin methods from MutableMapping
    if key.startswith("__"):
        continue

    if not inspect.isfunction(value):
        continue

    if hasattr(value, "__isabstractmethod__") and value.__isabstractmethod__:
        continue

    if key not in Record.__dict__:
        setattr(Record, key, value)


koatl_keywords = {
    "if",
    "then",
    "else",
    "check",
    "import",
    "export",
    "as",
    "class",
    "while",
    "for",
    "in",
    "break",
    "continue",
    "with",
    "yield",
    "global",
    "return",
    "raise",
    "try",
    "except",
    "finally",
    "and",
    "or",
    "not",
    "await",
    "let",
    "const",
    "with",
}
