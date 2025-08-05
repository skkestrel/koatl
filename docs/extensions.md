# Extension attributes (experimental)

Extension attributes allow us to bestow properties and methods on other types (including builtin types) without having to edit their type definition or use a library like `forbiddenfruit`.

In Koatl, `value.attr` transpiles to:

```python
__tl__.vget(value, "attr")
```

where `attr` is dynamically looked up using two global tables: one for types, and one for "traits".
A "trait" is a type of abstract base class that doesn't need to be inherited from; membership is dynamically determined based on the object
having all of the required attributes (which can also be extension attributes).

Extension attributes are registered globally using `koatl.runtime.virtual.register_global_attr(concrete_type, attr_name, method)`
or `koatl.runtime.virtual.register_global_trait(trait, attr_name, method)`.
`trait` should be `koatl.runtime.virtual.Trait(module_name, trait_name, trait_methods, *, requires)`.

Virtual resolution order is as follows:

1. Attempt `object.__getattr__(obj, "attr")`.
2. Attempt to look up the extension attribute in the type table using referential equality, for each type in the object's `mro()`.
3. Attempt to look up the extension attribute in the trait table.
4. Raise AttributeError.

One-argument `method`s can be decorated with the `__tl__.ExtensionProperty` decorator to have it behave as a property.

## The builtin `iter` extension property

The `iter` extension attribute is built in to the Koatl runtime and is used in `for .. in` loops as well as `yield from`;
it always delegates to `items()` if possible (to make dict iteration more sane) and also provides an implementation for slices.

`koatl.prelude.iterable.Iterable` is automatically registered as an extension trait for all types with an `iter`, and provides common methods for working with iterators.

## The builtin `ok`, `result`, `bind_once` extensions

`bind_once` provides a default Monad implementation in the Result monad to _all_ objects (see [Monads](monads)).

`.ok` is an extension property that is False for Exception types and None, and True for everything else.

`.result` wraps any object in a `Result`.
