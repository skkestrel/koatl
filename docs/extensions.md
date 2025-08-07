# Extension attributes (experimental)

Extension attributes allow us to bestow properties and methods on other types (including builtin types) without having to edit their type definition or use a library like `forbiddenfruit`.

Extensions are registered globally using the `ExtensionMethod(type, name)`, `ExtensionProperty(type, name)`, and `ExtensionTrait` decorators.
`ExtensionMethod` and `ExtensionProperty` enable lookup on the specified `type` (or any subtype).
`ExtensionTrait` enables lookup of a set of methods, predicated on the existence of all required abstract attributes/methods.

A trait is like a Python `abc.ABC`, and can be created by deriving from the global `Trait` base class.
Objects satisfying the trait requirements (marked with `Abstract`) get access to all of the trait's properties.

Example:

```koatl
export SomeTrait = ExtensionTrait& class(Trait):
    required_method = Abstract
    # or...
    # required_method = Abstract& self => ()

    derived_method = self => self.required_method()
    derived_property = TraitProperty& self => self.required_method()

ExtensionMethod(object, "some_global_attr")& self => ()
ExtensionProperty(object, "some_global_prop")& self => ()

None.some_global_attr() == ()
None.some_global_prop == ()
{required_method: () => 42}.derived_method() == 42
{required_method: () => 42}.derived_property == 42
#                 ^
# note: unlike JS, methods of anonymous objects should NOT take `self` as a parameter
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

## The builtin `ok`, `result`, `bind` extensions

`bind` provides a default Monad implementation in the Result monad to _all_ objects (see [Monads](monads)).

`.ok` is an extension property that is False for Exception types and None, and True for everything else.

`.result` wraps any object in a `Result`.
