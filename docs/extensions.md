# Extension Methods (experimental)

Extension attributes add methods and properties to any type — including builtins — without monkey-patching.

They are registered globally using `Extension.method(type, name)`, `Extension.property(type, name)`, and `Extension.trait` decorators. `Extension.method` and `Extension.property` enable lookup on the specified `type` (or any subtype), while `Extension.trait` enables lookup of a set of methods predicated on the existence of all required abstract attributes.

A trait is analogous to a Python `abc.ABC` and can be created by deriving from the global `Trait` base class. Objects satisfying the trait requirements (marked with `Trait.abstract!`) get access to all of the trait's properties.

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
{required_method: Record.method! self => 42}.derived_method() == 42
{required_method: Record.method! self => 42}.derived_property == 42
```

Virtual resolution order is as follows:

1. Attempt `object.__getattr__(obj, "attr")`.
2. Attempt to look up the extension attribute in the type table, using referential equality to check each type in the object's `mro()`.
3. Attempt to look up the extension attribute in the trait table.
4. Raise AttributeError.

## The built-in `iter` extension property

The `iter` extension attribute is built in to the Koatl runtime and is used in `for .. in` loops as well as `yield from`. It delegates to `items()` when available (making dict iteration more predictable) and also provides an implementation for slices.

The `Iterable` trait from `koatl.prelude.iterable` (see [Prelude](prelude)) marks any type with an `iter` property. The `Iterator` trait is automatically registered as an extension for all types with `__next__`, providing common methods for working with iterators.
