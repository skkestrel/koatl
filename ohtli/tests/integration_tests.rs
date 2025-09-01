use koatl_parser::{parse_tokens, tokenize};
use ohtli::{
    formatter::{format_lines, stmt_to_lines, LayoutCalculator, LayoutWriter},
    Config,
};

fn assert_compare_formatting(input: &str, expected: &str) {
    let config = Config::default();
    let (tokens, lex_errors) = tokenize(input, true);

    let Some(tokens) = tokens else {
        panic!("Lexing errors: {:?}", lex_errors)
    };

    let (cst, parse_errors) = parse_tokens(input, &tokens);

    println!("{:#?}", tokens);

    let Some(cst) = cst else {
        panic!("Parsing errors: {:?}", parse_errors);
    };

    let mut lines = Vec::new();

    for stmt in cst.value.iter() {
        lines.extend(stmt_to_lines(stmt));
    }

    let processed_layout = LayoutCalculator::new(&config).do_layout(lines.clone());

    let mut output_generator = LayoutWriter::new(&config);
    let result = output_generator.write(&processed_layout);

    if result.trim() != expected.trim() {
        println!("Input:\n{}", format_lines(&lines));
        println!("Output:\n{}", format_lines(&processed_layout));
        assert_eq!(result.trim(), expected.trim());
    }
}

#[test]
fn test_basic_assignment() {
    let input = "x = 42";
    let expected = "x = 42";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_line_comment_preservation() {
    let input = "# comment\nx = 42";
    let expected = "# comment\nx = 42";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_inline_comment_preservation() {
    let input = "x = 42 # inline comment";
    let expected = "x = 42 # inline comment";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_block() {
    let input = "if True:\n False";
    let expected = "if True:\n    False";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_multiple_assignments() {
    let input = "x = 42\ny = 1.5";
    let expected = "x = 42\ny = 1.5";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_simple_lambda() {
    let input = "f = x => x + 1";
    let expected = "f = x => x + 1";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_let_binding() {
    let input = "let x = 42";
    let expected = "let x = 42";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_method_call() {
    let input = "result = obj.method ( arg1, arg2)";
    let expected = "result = obj.method(arg1, arg2)";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_attribute_access() {
    let input = "value = obj.property";
    let expected = "value = obj.property";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_subscript_access() {
    let input = "item = array[index]";
    let expected = "item = array[index]";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_list_expression() {
    let input = "numbers = [1, 2, 3, 4, 5]";
    let expected = "numbers = [1, 2, 3, 4, 5]";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_mapping_expression() {
    let input = "config = {debug: true, timeout: 30}";
    let expected = "config = {debug: true, timeout: 30}";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_match_expression() {
    let input = "result = x match:\n    0 => \"zero\"\n    _ => \"other\"";
    let expected = "result = x match:\n    0 => \"zero\"\n    _ => \"other\"";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_complex_expression() {
    let input = "func(a, b)\n .method";
    let expected = "func(a, b)\n    .method";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_range_expression() {
    let input = "range = 1..10";
    let expected = "range = 1..10";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_await_expression() {
    let input = "result = @ async_func()";
    let expected = "result = @async_func()";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_trivia_spacing() {
    let input = "x=1+2*3  # Math expression";
    let expected = "x = 1 + 2 * 3 # Math expression"; // Normalize spacing and comments
    assert_compare_formatting(input, expected);
}

#[test]
fn test_chained_operations() {
    let input = "(..).map(i => i * 2).filter($ > 10).sum()";
    let expected = "(..).map(i => i * 2).filter($ > 10).sum()";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_record_literal() {
    let input = "person = {name: \"Alice\", age: 30}";
    let expected = "person = {name: \"Alice\", age: 30}";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_if_then_else() {
    let input = "result = condition then value1 else value2";
    let expected = "result = condition then value1 else value2";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_numeric_underscore() {
    let input = "big_number = 4_000_000";
    let expected = "big_number = 4_000_000";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_string_literal() {
    let input = "message = #- test -# \"Hello, world!\"";
    let expected = "message = #- test -# \"Hello, world!\"";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_floating_point() {
    let input = "pi = 3.14159";
    let expected = "pi = 3.14159";
    assert_compare_formatting(input, expected);
}

#[test]
fn test_memo_expression() {
    let input = "result = memo   @ fib(n - 1)";
    let expected = "result = memo @fib(n - 1)";
    assert_compare_formatting(input, expected);
}

// Block and indentation tests
#[test]
fn test_simple_if_block() {
    let input = r#"if condition:
    do_something()"#;
    let expected = r#"if condition:
    do_something()"#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_nested_blocks() {
    let input = r#"if outer:
    if inner:
        nested_action()"#;
    let expected = r#"if outer:
    if inner:
        nested_action()"#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_match_expression_with_blocks() {
    let input = r#"result = value match:
    0 =>
        "zero"
    1 =>
        "one"
    _ => "other""#;
    let expected = r#"result = value match:
    0 =>
        "zero"
    1 =>
        "one"
    _ => "other""#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_function_definition_with_block() {
    let input = r#"let func = x =>
    let y = x + 1
    y * 2"#;
    let expected = r#"let func = x =>
    let y = x + 1
    y * 2"#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_while_loop_block() {
    let input = r#"while condition:
    do_work()
    update_condition()"#;
    let expected = r#"while condition:
    do_work()
    update_condition()"#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_mixed_indentation_levels() {
    let input = r#"if condition1:
    if condition2:
        action1()
    else:
        action2()
    final_action()"#;
    let expected = r#"if condition1:
    if condition2:
        action1()
    else:
        action2()
    final_action()"#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_record_with_method_blocks() {
    let input = r#"let obj = {
    value: 42
    method: Record.method! self =>
        self.value + 1
}"#;
    let expected = r#"let obj = {
    value: 42
    method: Record.method! self =>
        self.value + 1
}"#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_multiline_list_literal() {
    let input = r#"let items = [
    first_item
    second_item
    third_item
]"#;
    let expected = r#"let items = [
    first_item
    second_item
    third_item
]"#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_mixed_list_literal() {
    let input = r#"let items = [
    1, 2, 3
    4, 5, 6
    7,  8, 9
]"#;
    let expected = r#"let items = [
    1, 2, 3
    4, 5, 6
    7, 8, 9
]"#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_multiline_mapping_literal() {
    let input = r#"let config = {
    debug: true
    timeout: 30
    retries: 3
}"#;
    let expected = r#"let config = {
    debug: true
    timeout: 30
    retries: 3
}"#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_complex_nested_structure() {
    let input = r#"let result = items
    .filter(item =>
        item.valid and
            item.active
    )
    .map(process)"#;
    let expected = r#"let result = items
    .filter(item =>
        item.valid and
            item.active
    )
    .map(process)"#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_memo_block() {
    let input = r#"memo:
    expensive_computation()
    with_caching()"#;
    let expected = r#"memo:
    expensive_computation()
    with_caching()"#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_try_except_block() {
    let input = r#"try:
    risky_operation()
except Error as e =>
    handle_error(e)
finally:
    cleanup()"#;
    let expected = r#"try:
    risky_operation()
except Error as e =>
    handle_error(e)
finally:
    cleanup()"#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_class_definition_block() {
    let input = r#"class:
    init = self =>
        self.value = 0
    method = self =>
        self.value + 1"#;
    let expected = r#"class:
    init = self =>
        self.value = 0
    method = self =>
        self.value + 1"#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_pattern_matching_with_guards() {
    let input = r#"result = value match:
    x if x > 0 =>
        "positive"
    x if x < 0 =>
        "negative"
    _ =>
        "zero""#;
    let expected = r#"result = value match:
    x if x > 0 =>
        "positive"
    x if x < 0 =>
        "negative"
    _ =>
        "zero""#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_fold_with_multiline_lambda() {
    let input = r#"result = items.fold(0, (acc, item) =>
    let processed = process(item)
    acc + processed
)"#;
    let expected = r#"result = items.fold(0, (acc, item) =>
    let processed = process(item)
    acc + processed
)"#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_deeply_nested_blocks() {
    let input = r#"if level1:
    if level2:
        if level3:
            if level4:
                deep_action()
            level3_action()
        level2_action()
    level1_action()"#;
    let expected = r#"if level1:
    if level2:
        if level3:
            if level4:
                deep_action()
            level3_action()
        level2_action()
    level1_action()"#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_inline_vs_block_conditional() {
    let input = r#"result = condition then short_value else:
    complex_computation()
    with_multiple_steps()"#;
    let expected = r#"result = condition then short_value else:
    complex_computation()
    with_multiple_steps()"#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_method_chaining_with_blocks() {
    let input = r#"result = data
    .filter(item =>
        item.is_valid()
    )
    .map(item =>
        item.transform()
    )
    .reduce((a, b) =>
        a.combine(b)
    )"#;
    let expected = r#"result = data
    .filter(item =>
        item.is_valid()
    )
    .map(item =>
        item.transform()
    )
    .reduce((a, b) =>
        a.combine(b)
    )"#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_comment_in_block() {
    let input = r#"if condition:
    # This is a comment
    action()
    # Another comment
    another_action()"#;
    let expected = r#"if condition:
    # This is a comment
    action()
    # Another comment
    another_action()"#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_mixed_inline_and_block() {
    let input = r#"result = condition1 then value1 else condition2 then:
    complex_value()
else:
    default_value()"#;
    let expected = r#"result = condition1 then value1 else condition2 then:
    complex_value()
else:
    default_value()"#;
    assert_compare_formatting(input, expected);
}

#[test]
fn test_parenthesized_block() {
    let input = r#"let a = (
    let b =
        3+4
    b
)"#;
    let expected = r#"let a = (
    let b =
        3 + 4
    b
)"#;

    assert_compare_formatting(input, expected);
}
