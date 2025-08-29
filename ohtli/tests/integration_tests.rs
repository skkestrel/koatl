use ohtli::{Config, Formatter};

fn assert_compare_formatting(input: &str, expected: &str) {
    let config = Config::default();
    let formatter = Formatter::new(config);
    let result = formatter.format(input);
    assert!(result.is_ok(), "Formatting should not error: {:?}", result);
    assert_eq!(result.unwrap(), expected);
}

#[test]
fn test_basic_formatting() {
    assert_compare_formatting("1e9", "1e9");
}
