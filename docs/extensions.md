# Extension attributes (experimental)

Extension attributes allow us to non-destructively bestow properties and methods on any type (including builtin types).

Extensions are registered globally using the `Extension.method(type, name)`, `Extension.property(type, name)`, and `Extension.trait` decorators.
`Extension.method` and `Extension.property` enable lookup on the specified `type` (or any subtype).
`Extension.trait` enables lookup of a set of methods, predicated on the existence of all required abstract attributes/methods.

A trait is like a Python `abc.ABC`, and can be created by deriving from the global `Trait` base class.
Objects satisfying the trait requirements (marked with `Abstract`) get access to all of the trait's properties.

Example:

```koatl
export SomeTrait = Extension.trait! class(Trait):
    required_method = Trait.abstract! self => ()

    derived_method = self => self.required_method()
    derived_property = Trait.property! self => self.required_method()

Extension.method(object, "some_global_attr")! self => ()
Extension.property(object, "some_global_prop")! self => ()

None.some_global_attr() == ()
None.some_global_prop == ()
{required_method: Record.method& self => 42}.derived_method() == 42
{required_method: Record.method& self => 42}.derived_property == 42
```

Virtual resolution order is as follows:

1. Attempt `object.__getattr__(obj, "attr")`.
2. Attempt to look up the extension attribute in the type table, using referential equality to check each type in the object's `mro()`.
3. Attempt to look up the extension attribute in the trait table.
4. Raise AttributeError.

## The builtin `iter` extension property

The `iter` extension attribute is built in to the Koatl runtime and is used in `for .. in` loops as well as `yield from`;
it always delegates to `items()` if possible (to make dict iteration more sane) and also provides an implementation for slices.

`koatl.prelude.iterable.Iterable` is automatically registered as an extension trait for all types with an `iter`, and provides common methods for working with iterators.
