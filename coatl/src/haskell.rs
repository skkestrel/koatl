use chumsky::prelude::*;
use chumsky::text::whitespace;
use std::fmt;

/// The tokens that the lexer will produce.
/// Note that we have special tokens for managing indentation blocks.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Token {
    // Keywords
    Let,
    In,
    If,
    Then,
    Else,

    // Delimiters and separators for layout blocks
    BlockBegin, // Like '{'
    BlockEnd,   // Like '}'
    Separator,  // Like ';'

    // Literals and identifiers
    Ident(String),
    Int(i64),

    // Operators
    Assign, // =
    Arrow,  // ->
    Op(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Let => write!(f, "let"),
            Token::In => write!(f, "in"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::BlockBegin => write!(f, "{{"),
            Token::BlockEnd => write!(f, "}}"),
            Token::Separator => write!(f, ";"),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Int(i) => write!(f, "{}", i),
            Token::Assign => write!(f, "="),
            Token::Arrow => write!(f, "->"),
            Token::Op(s) => write!(f, "{}", s),
        }
    }
}

/// The Abstract Syntax Tree (AST) for our simple language.
#[derive(Debug, PartialEq)]
pub enum Expr {
    Int(i64),
    Ident(String),
    Let {
        bindings: Vec<(String, Box<Expr>)>,
        body: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    BinaryOp(Box<Expr>, String, Box<Expr>),
    Lambda(String, Box<Expr>),
    Error,
}

/// The custom lexer that handles indentation.
fn lexer() -> impl Parser<char, Vec<(Token, SimpleSpan)>, Error = Simple<char>> {
    // A parser for basic tokens, ignoring comments.
    let token = {
        let int = text::int(10).map(|s: String| Token::Int(s.parse().unwrap()));

        let op = choice((
            just("=").to(Token::Assign),
            just("->").to(Token::Arrow),
            one_of("+-*/").map(|c| Token::Op(c.to_string())),
        ));

        let ident = text::ident().map(|ident: String| match ident.as_str() {
            "let" => Token::Let,
            "in" => Token::In,
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            _ => Token::Ident(ident),
        });

        int.or(op).or(ident)
    };

    // A parser for a single line of code. It produces the line's indentation
    // level and a vector of tokens found on that line.
    let line = text::indentation()
        .then(token.padded_by(whitespace()).repeated())
        .then_ignore(text::newline().or(end()));

    // The main lexer logic.
    line.repeated()
        .scan(
            (Vec::new(), Vec::new()),
            |(indent_stack, tokens), (col, line_tokens)| {
                // Check for layout changes at the start of the line
                if let Some(&last_indent) = indent_stack.last() {
                    if col < last_indent {
                        // Dedentation: end of a block
                        while let Some(&last_indent) = indent_stack.last() {
                            if col < last_indent {
                                indent_stack.pop();
                                tokens.push((Token::BlockEnd, SimpleSpan::new(0, 0))); // Span is dummy
                            } else {
                                break;
                            }
                        }
                        if indent_stack.last() != Some(&col) {
                            // This is a layout error
                            // For simplicity, we'll just stop lexing. A real implementation
                            // would emit an error token.
                            return None;
                        }
                        tokens.push((Token::Separator, SimpleSpan::new(0, 0)));
                    } else if col == last_indent {
                        // Same indentation: new item in the block
                        tokens.push((Token::Separator, SimpleSpan::new(0, 0)));
                    }
                }

                // Process tokens on the current line
                let mut last_was_layout_kw = false;
                for (tok, span) in line_tokens {
                    tokens.push((tok.clone(), span));
                    last_was_layout_kw = matches!(tok, Token::Let | Token::If);
                }

                // If a layout keyword was the last token, start a new block
                if last_was_layout_kw {
                    if let Some((next_col, _)) = tokens.last().and_then(|(tok, _)| {
                        // This is a simplification. A real lexer would need to peek ahead
                        // to find the indentation of the *next* line.
                        // For this example, we assume the block starts on the next line
                        // and has a greater indentation.
                        None // This part is tricky and requires a more stateful lexer.
                    }) {
                        // A more robust implementation would handle this.
                    } else {
                        // A simplified heuristic: if 'let' is seen, expect an indented block.
                        // We find the indentation of the *next* line to set the block level.
                        // This requires the lexer to be more stateful than `scan` allows easily.
                        // So we'll fake it a bit for this example.
                        if let Some(last_indent) = indent_stack.last() {
                            // A placeholder for the next line's indentation.
                            // A real implementation would need to buffer lines.
                            indent_stack.push(last_indent + 4);
                        } else {
                            indent_stack.push(col + 4); // Assume standard 4-space indent
                        }
                        tokens.push((Token::BlockBegin, SimpleSpan::new(0, 0)));
                    }
                }

                Some(std::mem::take(tokens))
            },
        )
        .flatten()
        .then_ignore(end())
        .map_with_span(|tokens, span| (tokens, span))
        .then_with(|((mut tokens, _), _)| {
            // Unwind any remaining indentation blocks at the end of the file.
            let end_span = SimpleSpan::new(0, 0); // Dummy span
            let indent_stack_len = tokens
                .iter()
                .filter(|(t, _)| *t == Token::BlockBegin)
                .count()
                - tokens.iter().filter(|(t, _)| *t == Token::BlockEnd).count();
            for _ in 0..indent_stack_len {
                tokens.push((Token::BlockEnd, end_span));
            }
            Ok(tokens)
        })
}

/// The parser for our language's expressions.
/// It operates on the stream of `Token`s from the lexer.
fn expr_parser() -> impl Parser<Token, Expr, Error = Simple<Token>> {
    let ident = select! { Token::Ident(ident) => ident }.labelled("identifier");

    recursive(|expr| {
        let val = select! {
            Token::Int(i) => Expr::Int(i),
            Token::Ident(s) => Expr::Ident(s),
        }
        .labelled("value");

        let r#let = just(Token::Let)
            .ignore_then(just(Token::BlockBegin))
            .ignore_then(
                ident
                    .then_ignore(just(Token::Assign))
                    .then(expr.clone())
                    .separated_by(just(Token::Separator))
                    .allow_trailing(),
            )
            .then_ignore(just(Token::BlockEnd))
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|(bindings, body)| Expr::Let {
                bindings: bindings
                    .into_iter()
                    .map(|(name, expr)| (name, Box::new(expr)))
                    .collect(),
                body: Box::new(body),
            });

        let r#if = just(Token::If)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Then))
            .then(expr.clone())
            .then_ignore(just(Token::Else))
            .then(expr.clone())
            .map(|((cond, then_b), else_b)| Expr::If {
                cond: Box::new(cond),
                then_branch: Box::new(then_b),
                else_branch: Box::new(else_b),
            });

        let lambda = ident
            .then_ignore(just(Token::Arrow))
            .then(expr.clone())
            .map(|(arg, body)| Expr::Lambda(arg, Box::new(body)));

        let atom = val.or(r#let).or(r#if).or(lambda).or(expr
            .clone()
            .delimited_by(just(Token::BlockBegin), just(Token::BlockEnd))); // For parenthesized exprs

        let op = select! { Token::Op(op) => op };
        atom.clone()
            .then(op.then(atom).repeated())
            .foldl(|lhs, (op, rhs)| Expr::BinaryOp(Box::new(lhs), op, Box::new(rhs)))
    })
}

fn main() {
    let src = r#"
let
    x = 10
    y = 20
in
    if x + y
    then x
    else y
"#;

    println!("Parsing source:\n{}", src);

    // Note: The lexer here is a simplified demonstration. A fully-featured
    // Haskell-style lexer requires more sophisticated state management to correctly
    // determine indentation levels for new blocks, especially when they aren't
    // on the immediately following line. This example uses a heuristic.
    let (tokens, lex_errs) = match lexer().parse_recovery(src) {
        (Some(tokens), errs) => (tokens, errs),
        (None, errs) => (vec![], errs),
    };

    println!("\nLexed tokens:");
    for (tok, _) in &tokens {
        print!("{} ", tok);
    }
    println!("\n");

    if !lex_errs.is_empty() {
        println!("Lexer errors: {:?}", lex_errs);
        return;
    }

    let (ast, parse_errs) =
        expr_parser().parse_recovery(tokens.as_slice().spanned(SimpleSpan::from(0..src.len())));

    match ast {
        Some(ast) => {
            println!("Parsed AST:\n{:#?}", ast);
        }
        None => {
            println!("Failed to parse.");
        }
    }

    if !parse_errs.is_empty() {
        println!("\nParser errors:");
        for e in parse_errs {
            println!("{:?}", e);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_let_in() {
        let src = "let\n  x = 5\nin\n  x";
        let (tokens, _) = lexer().parse(src).unwrap();
        let (ast, errs) = expr_parser()
            .parse_recovery(&tokens.as_slice().spanned(SimpleSpan::from(0..src.len())));

        assert!(errs.is_empty());
        assert_eq!(
            ast,
            Some(Expr::Let {
                bindings: vec![("x".to_string(), Box::new(Expr::Int(5)))],
                body: Box::new(Expr::Ident("x".to_string())),
            })
        );
    }

    #[test]
    fn test_nested_let() {
        let src = r#"
let
    a = let
            b = 1
        in
            b
in
    a
"#;
        // This test highlights the limitations of the simplified lexer.
        // A more robust implementation is needed for correct nested parsing.
        // The current lexer might not produce the correct token stream for this.
        let (tokens, lex_errs) = lexer().parse_recovery(src);
        println!("Tokens for nested let: {:?}", tokens);
        assert!(
            lex_errs.is_empty(),
            "Lexer should not produce errors for this valid input."
        );

        let (ast, parse_errs) = expr_parser().parse_recovery(
            &tokens
                .unwrap()
                .as_slice()
                .spanned(SimpleSpan::from(0..src.len())),
        );
        assert!(parse_errs.is_empty(), "Parser should not produce errors.");

        // The expected AST structure
        let expected_ast = Expr::Let {
            bindings: vec![(
                "a".to_string(),
                Box::new(Expr::Let {
                    bindings: vec![("b".to_string(), Box::new(Expr::Int(1)))],
                    body: Box::new(Expr::Ident("b".to_string())),
                }),
            )],
            body: Box::new(Expr::Ident("a".to_string())),
        };

        assert_eq!(ast, Some(expected_ast));
    }

    #[test]
    fn test_if_then_else() {
        let src = "if 1 then 2 else 3";
        let (tokens, _) = lexer().parse(src).unwrap();
        let (ast, errs) = expr_parser()
            .parse_recovery(&tokens.as_slice().spanned(SimpleSpan::from(0..src.len())));

        assert!(errs.is_empty());
        assert_eq!(
            ast,
            Some(Expr::If {
                cond: Box::new(Expr::Int(1)),
                then_branch: Box::new(Expr::Int(2)),
                else_branch: Box::new(Expr::Int(3)),
            })
        );
    }
}
