#![allow(unused_variables)]

#[cfg(test)]
mod tests {
    use parser::{Token, tokenize};

    #[test]
    fn test_tokenize_simple_identifier() {
        let input = "hello";
        let (result, errors) = tokenize(input);

        assert!(errors.is_empty(), "Expected no errors, got: {:?}", errors);
        assert!(result.is_some(), "Expected tokenization result");

        let tokens = result.unwrap();
        assert_eq!(tokens.0.len(), 2); // identifier + eol

        match &tokens.0[0].0 {
            Token::Ident(name) => assert_eq!(*name, "hello"),
            _ => panic!("Expected identifier token, got: {:?}", tokens.0[0].0),
        }

        match &tokens.0[1].0 {
            Token::Eol => {}
            _ => panic!("Expected EOL token, got: {:?}", tokens.0[1].0),
        }
    }

    #[test]
    fn test_tokenize_keyword() {
        let input = "if";
        let (result, errors) = tokenize(input);

        assert!(errors.is_empty(), "Expected no errors, got: {:?}", errors);
        assert!(result.is_some(), "Expected tokenization result");

        let tokens = result.unwrap();
        assert_eq!(tokens.0.len(), 2); // keyword + eol

        match &tokens.0[0].0 {
            Token::Kw(kw) => assert_eq!(*kw, "if"),
            _ => panic!("Expected keyword token, got: {:?}", tokens.0[0].0),
        }
    }

    #[test]
    fn test_tokenize_number() {
        let input = "123";
        let (result, errors) = tokenize(input);

        assert!(errors.is_empty(), "Expected no errors, got: {:?}", errors);
        assert!(result.is_some(), "Expected tokenization result");

        let tokens = result.unwrap();
        assert_eq!(tokens.0.len(), 2); // number + eol

        match &tokens.0[0].0 {
            Token::Num(num) => assert_eq!(*num, "123"),
            _ => panic!("Expected number token, got: {:?}", tokens.0[0].0),
        }
    }

    #[test]
    fn test_tokenize_float() {
        let input = "123.45";
        let (result, errors) = tokenize(input);

        assert!(errors.is_empty(), "Expected no errors, got: {:?}", errors);
        assert!(result.is_some(), "Expected tokenization result");

        let tokens = result.unwrap();
        assert_eq!(tokens.0.len(), 2); // float + eol

        match &tokens.0[0].0 {
            Token::Num(num) => assert_eq!(*num, "123.45"),
            _ => panic!("Expected number token, got: {:?}", tokens.0[0].0),
        }
    }

    #[test]
    fn test_tokenize_string() {
        let input = r#""hello world""#;
        let (result, errors) = tokenize(input);

        assert!(errors.is_empty(), "Expected no errors, got: {:?}", errors);
        assert!(result.is_some(), "Expected tokenization result");

        let tokens = result.unwrap();
        assert_eq!(tokens.0.len(), 2); // string + eol

        match &tokens.0[0].0 {
            Token::Str(s) => assert_eq!(s, "hello world"),
            _ => panic!("Expected string token, got: {:?}", tokens.0[0].0),
        }
    }

    #[test]
    fn test_tokenize_symbols() {
        let input = "+ - * / == <> <= >=";
        let (result, errors) = tokenize(input);

        assert!(errors.is_empty(), "Expected no errors, got: {:?}", errors);
        assert!(result.is_some(), "Expected tokenization result");

        let tokens = result.unwrap();
        assert_eq!(tokens.0.len(), 9); // 8 symbols + eol

        let expected_symbols = ["+", "-", "*", "/", "==", "<>", "<=", ">="];
        for (i, expected) in expected_symbols.iter().enumerate() {
            match &tokens.0[i].0 {
                Token::Symbol(sym) => assert_eq!(sym, expected),
                _ => panic!("Expected symbol '{}', got: {:?}", expected, tokens.0[i].0),
            }
        }
    }

    #[test]
    fn test_tokenize_multiple_lines() {
        let input = "x = 1\ny = 2";
        let (result, errors) = tokenize(input);

        assert!(errors.is_empty(), "Expected no errors, got: {:?}", errors);
        assert!(result.is_some(), "Expected tokenization result");

        let tokens = result.unwrap();

        // Should have: x, =, 1, eol, y, =, 2, eol
        assert!(
            tokens.0.len() >= 8,
            "Expected at least 8 tokens, got: {}",
            tokens.0.len()
        );
    }

    #[test]
    fn test_tokenize_indentation() {
        let input = "if true:\n    x = 1";
        let (result, errors) = tokenize(input);

        assert!(errors.is_empty(), "Expected no errors, got: {:?}", errors);
        assert!(result.is_some(), "Expected tokenization result");

        let tokens = result.unwrap();

        // Should contain BEGIN_BLOCK and END_BLOCK tokens for indentation
        let has_begin_block = tokens
            .0
            .iter()
            .any(|(token, _)| matches!(token, Token::Symbol(s) if *s == "BEGIN_BLOCK"));
        let has_end_block = tokens
            .0
            .iter()
            .any(|(token, _)| matches!(token, Token::Symbol(s) if *s == "END_BLOCK"));

        assert!(has_begin_block, "Expected BEGIN_BLOCK token");
        assert!(has_end_block, "Expected END_BLOCK token");
    }

    #[test]
    fn test_tokenize_fstring() {
        let input = r#"f"hello {name}""#;
        let (result, errors) = tokenize(input);

        assert!(errors.is_empty(), "Expected no errors, got: {:?}", errors);
        assert!(result.is_some(), "Expected tokenization result");

        let tokens = result.unwrap();

        // Should contain f-string tokens
        let has_fstr_begin = tokens
            .0
            .iter()
            .any(|(token, _)| matches!(token, Token::FstrBegin(_)));

        assert!(has_fstr_begin, "Expected f-string begin token");
    }

    #[test]
    fn test_tokenize_block_comment() {
        let input = "#- this is a block comment -# x = 1";
        let (result, errors) = tokenize(input);

        assert!(errors.is_empty(), "Expected no errors, got: {:?}", errors);
        assert!(result.is_some(), "Expected tokenization result");

        let tokens = result.unwrap();

        // Should contain the identifier and assignment after the comment
        let has_identifier = tokens
            .0
            .iter()
            .any(|(token, _)| matches!(token, Token::Ident(name) if *name == "x"));

        assert!(
            has_identifier,
            "Expected identifier 'x' after block comment"
        );
    }

    #[test]
    fn test_empty_input() {
        let input = "";
        let (result, errors) = tokenize(input);

        // Empty input might be an error or might produce just an EOL token
        // This depends on your lexer's specific behavior
        if let Some(tokens) = result {
            // If successful, should at least have an EOL
            assert!(
                !tokens.0.is_empty(),
                "Expected at least one token for empty input"
            );
        }
    }

    #[test]
    fn test_invalid_indentation() {
        let input = "if true:\nx = 1"; // Missing indentation after colon
        let (result, errors) = tokenize(input);

        // This should produce an error due to invalid indentation
        assert!(!errors.is_empty(), "Expected indentation error");
    }
}
