import util.assert_eq

# Test __locals__ in function scope with let declarations
test_function_locals = () =>
    let y = 1
    let z = 2
    assert_eq(__locals__, {y: 1, z: 2})

test_function_locals()

# Test __locals__ in nested function
test_nested = () =>
    let outer = 1
    let inner = () =>
        let inner_var = 2
        assert_eq(__locals__, {inner_var: 2})
    inner()

test_nested()

# Test __locals__ with function arguments (not mangled)
test_with_args = (arg1, arg2) =>
    let local = 100
    assert_eq(__locals__, {local: 100, arg1: arg1, arg2: arg2})

test_with_args(10, 20)