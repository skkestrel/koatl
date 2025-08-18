import koatl._rs.(fast_vset, fast_vset_trait)

ext_prop = (type, name) => value =>
    value::_property = True
    fast_vset(type, name, value)
    value


ext_method = (type, name) => value =>
    fast_vset(type, name, value)
    value


ext_trait = type =>
    for name, value in type::_own_methods:
        fast_vset_trait(type::__name__, type::_trait_reqs, name, value)
    type


export Extension = {
    property: ext_prop,
    method: ext_method,
    trait: ext_trait,
}