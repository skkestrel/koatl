import importlib
import collections

export LazyModule = class:
    __init__ = (self, name) =>
        self.__dict__["_name"] = name
        self.__dict__["_module"] = None
        self.__dict__["_submodules"] = {}

    __getattr__ = (self, attr) =>
        try:
            if self._module === None:
                self.__dict__["_module"] = importlib.import_module(self._name)

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
        except e =>
            raise ValueError(str(e))

        raise AttributeError(f"Module '{self._name}' has no attribute '{attr}'")

    __setattr__ = (self, attr, value) =>
        if self._module === None:
            self.__dict__["_module"] = importlib.import_module(self._name)

        setattr(self._module, attr, value)

    __repr__ = self => f"LazyModule({self._name})"


export RootModulesProxy = class:
    __init__ = (self) =>
        self._modules = {}

    __getattr__ = (self, attr) =>
        if attr not in self._modules:
            self._modules[attr] = LazyModule(attr)
        return self._modules[attr]

    __repr__ = self => f"RootModulesProxy({self._modules})"
