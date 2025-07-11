import forbiddenfruit

class Iter:
    def __init__(self, obj):
        self.obj = obj

    def for_each(self, func):
        print("Iterating over:", self.obj)

forbiddenfruit.curse(object, "it", property(lambda self: Iter(self)))

range(10).it.for_each(lambda x: print(x))