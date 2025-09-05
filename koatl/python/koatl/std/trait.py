import abc
import collections
import inspect

from koatl.runtime import vhas


@collections.abc.Mapping.register
class MappingMeta(type):
    """
    A metaclass that makes a class behave like a read-only mapping.
    Useful for destructuring static methods from a class.
    """

    def __getitem__(self, key):
        return getattr(self, key)

    def __iter__(self):
        return self.keys().__iter__()

    def __len__(self):
        return len(self.items())

    def __contains__(self, key):
        return hasattr(self, key)

    def get(self, key, default=None):
        if hasattr(self, key):
            return getattr(self, key)
        else:
            return default

    def keys(self):
        return [k for k, _ in self.items()]

    def values(self):
        return [v for _, v in self.items()]

    def items(self):
        return [(k, v) for k, v in inspect.getmembers(self) if not k.startswith("_")]


# A utility base class to inherit from that implements Mapping on the type and thus destructuring.
class Class(metaclass=MappingMeta):
    pass


# temporary value, since TraitMeta needs to reference Trait.
Trait = None


class TraitMeta(MappingMeta):
    def __init__(cls, name, bases, namespace):
        super().__init__(name, bases, namespace)

        if Trait in bases:
            cls._trait_reqs = []
            cls._own_methods = {}

            for key, value in inspect.getmembers(cls):
                if (
                    hasattr(value, "__isabstractmethod__")
                    and value.__isabstractmethod__
                ):
                    cls._trait_reqs.append(key)

            for key, value in namespace.items():
                if (
                    hasattr(value, "__isabstractmethod__")
                    and value.__isabstractmethod__
                ):
                    # Abstract methods are not considered "own methods".
                    continue

                if callable(value) and not key.startswith("_"):
                    cls._own_methods[key] = value
                elif isinstance(value, property):
                    cls._own_methods[key] = value.fget

            def new(*args, **kwargs):
                raise TypeError(
                    f"Trait cannot be instantiated, since abstract methods {cls._trait_reqs} are not implemented."
                )

            if len(cls._trait_reqs) > 0:
                cls.__init__ = new

    def __instancecheck__(cls, instance):
        # Check if the exact class has _trait_reqs, in which case it's a trait.
        if "_trait_reqs" in cls.__dict__:
            for req in cls._trait_reqs:
                if not vhas(instance, req):
                    return False

            return True
        else:
            return type.__instancecheck__(cls, instance)


class Trait(metaclass=TraitMeta):
    @staticmethod
    def property(fn):
        """Decorator to mark a method as a trait property."""
        fn._property = True
        return property(fn)

    @staticmethod
    def abstract(value):
        return abc.abstractmethod(value)


__all__ = ["Class", "Trait"]
