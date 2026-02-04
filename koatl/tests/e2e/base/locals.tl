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

# Test __captures__ in nested function
test_nested_captures = () =>
    let outer_var = 1
    let another_outer = 2
    let inner = () =>
        let inner_var = 3
        outer_var, another_outer

        # Should capture outer_var and another_outer but not inner_var
        assert_eq(__captures__, {assert_eq, inner, outer_var, another_outer})
    inner()

test_nested_captures()

# Test __captures__ with multiple nesting levels
test_deep_nesting = () =>
    let level1 = 1
    let middle = () =>
        let level2 = 2
        let inner = () =>
            let level3 = 3
            # Should capture level1 and level2 but not level3
            assert_eq(__captures__, {assert_eq, inner, middle, level1, level2})
        inner()
    middle()

test_deep_nesting()

test_no_captures = () =>
    assert_eq(__captures__, {test_no_captures, assert_eq})

test_no_captures()

# Test __captures__ with function arguments in outer scope
test_args_captured = (arg1, arg2) =>
    let local_outer = 100
    let inner = () =>
        assert_eq(__captures__, {assert_eq, inner, arg1, arg2, local_outer})
    inner()

test_args_captured(10, 20)