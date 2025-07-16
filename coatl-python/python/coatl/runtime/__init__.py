from . import meta_finder
meta_finder.install_hook()
del meta_finder

def clean_up_exports(globals_dict, exports, module_star_exports):
    import importlib

    exports = set(exports)

    for module in module_star_exports:
        if module in globals_dict:
            mod = importlib.import_module(module)

            if hasattr(mod, "__all__"):
                for name in mod.__all__:
                    exports.add(name)
            else:
                for name in dir(mod):
                    if name.startswith("_"):
                        continue

                    exports.add(name)

    for key in list(globals_dict.keys()):
        if key not in exports:
            del globals_dict[key]