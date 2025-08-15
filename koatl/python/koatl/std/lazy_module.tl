import importlib
import collections

export LazyModule = class:
    __init__ = (self, name) =>
        self._name = name
        self._module = None
        self._submodules = {}

    __getattr__ = (self, attr) =>
        try:
            if self._module === None:
                self._module = importlib.import_module(self._name)

            if hasattr(self._module, "__path__"):
                if attr not in self._submodules:
                    self._submodules[attr] =
                        if importlib.util.find_spec(f"{self._name}.{attr}", self._module.__path__):
                            LazyModule(f"{self._name}.{attr}")
                        else:
                            None

                if self._submodules[attr] <=> None:
                    return self._submodules[attr]

            if hasattr(self._module, attr):
                return getattr(self._module, attr)
        except e:
            raise ValueError(str(e))

        raise AttributeError(f"Module '{self._name}' has no attribute '{attr}'")

    __repr__ = self => f"LazyModule({self._name})"