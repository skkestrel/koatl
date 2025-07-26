# Extension attributes (experimental)

Extension attributes allow us to bestow properties and methods on other types (including builtin types) without having to edit their type definition or use a library like `forbiddenfruit`.

A virtual-get takes the syntax `value!attr`, and should be thought of as an alternative to the Python syntax of `value.attr`.

Specifically, `value!attr` transpiles to:

```python
__tl__.vget(value, "attr")
```

where `attr` is dynamically looked up using two global dictionaries, `koatl.runtime.traits.types_vtbl` and `koatl.runtime.traits.traits_vtbl`.

The structure of both dictionaries look like:

```python
types_vtbl = {
    "attr": {
        MyType: method_for_type
        MyOtherType: other_method
    }
}
```

Virtual resolution order is as follows:

1. Attempt `object.__getattr__(obj, "attr")`.
2. Attempt `types_vtbl["attr"][t]` for each `t` in `type(obj).mro()`.
3. Attempt `traits_vtbl["attr"][t]` for each `t`, but only if `isinstance(obj, t)`.
4. Raise AttributeError.

Virtual tables should be interfaced with using `koatl.runtime.traits.register_global_attr(concrete_type, attr_name, method)`
or `koatl.runtime.traits.register_global_trait(abstract_type, attr_name, method)`.
One-argument `method`s can be decorated with the `__tl__.ExtensionProperty` decorator to have it behave as a property.

## The builtin `iter` extension attribute

The `!iter` extension attribute is built in to the Koatl runtime and is used in `for .. in` loops as well as `yield from`;
it always delegates to `items()` if possible (to make dict iteration more sane) and also provides an implementation for slices.

`koatl.prelude.iterable.Iterable` is automatically registered as an extension trait for all types with an `!iter`, and provides common methods for working with iterators.

## The builtin `pure` and `bind_once` extension attributes

These extension attributes provide default Monad implementations (in the Ok monad) to all types (see [monads](Monads)).
