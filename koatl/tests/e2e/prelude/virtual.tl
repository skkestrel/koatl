import util.assert_eq

# this should get the special-cased iter
assert_eq([1, 2, 3].iter | list, [1, 2, 3])

# iter should grab items whenever possible
assert_eq({1: 2, 3: 4}.iter | list, [(1, 2), (3, 4)])

# iter should work on slices
assert_eq((..5).iter | list, [0, 1, 2, 3, 4])

# should grab attrs if they exist
x = {a: 1}
assert_eq(x.a, 1)


SomeTrait = Extension.trait! class(Trait):
    required_method = Trait.abstract! self => ()

    derived_method = self => self.required_method()
    derived_property = Trait.property! self => self.required_method()

Extension.method(object, "some_global_attr")! self => ()
Extension.property(object, "some_global_prop")! self => ()

assert_eq(None.some_global_attr(), ())
assert_eq(None.some_global_prop, ())
assert_eq({required_method: Record.method! self => 42}.derived_method(), 42)
assert_eq({required_method: Record.method! self => 42}.derived_property, 42)

assert_eq((check "asdf".derived_method).ok, False)
assert_eq((check "asdf".derived_prop).ok, False)