use anyhow::Result;
use koatl_parser::cst::*;
use koatl_parser::lexer::{tokenize, SToken, Trivium, TriviumType};
use koatl_parser::parse_tokens;

use crate::config::Config;

pub struct Formatter {
    config: Config,
}

impl Formatter {
    pub fn new(config: Config) -> Self {
        Self { config }
    }

    pub fn format(&self, source: &str) -> Result<String> {
        let (tokens, lex_errors) = tokenize(source, true);

        let Some(tokens) = tokens else {
            anyhow::bail!("Lexing errors: {:?}", lex_errors);
        };

        let (cst, parse_errors) = parse_tokens(source, &tokens);

        let Some(cst) = cst else {
            anyhow::bail!("Parsing errors: {:?}", parse_errors);
        };

        let mut layout_calculator = LayoutCalculator::new(&self.config);
        let layout = layout_calculator.calculate_layout(&cst, &tokens.0);

        let mut output_generator = LayoutWriter::new(&self.config);
        Ok(output_generator.generate_output(layout))
    }
}

/// Core layout structure that mirrors Koatl's syntax patterns
#[derive(Debug, Clone)]
pub struct Layout {
    pub elements: Vec<LayoutElement>,
}

#[derive(Debug, Clone)]
pub enum LayoutElement {
    /// A single line of tokens separated by spaces
    Line(Line),
    /// Block structure with colon
    ColonBlock {
        colon_line: Line,
        body: Vec<Line>,
        is_inline: bool,
    },
    /// Block structure with arrow
    ArrowBlock {
        arrow_line: Line,
        body: Vec<Line>,
        is_inline: bool,
    },
    /// Plain text content
    Text {
        content: String,
        preserve_whitespace: bool,
    },
    /// Newlines
    Newline { count: usize },
    /// Comments
    Comment {
        content: String,
        is_line_comment: bool,
    },
}

/// A line consists of tokens that should be separated by spaces
#[derive(Debug, Clone)]
pub struct Line {
    pub tokens: Vec<Token>,
    pub indent_level: usize,
}

/// Individual tokens in a line
#[derive(Debug, Clone)]
pub enum Token {
    /// Atomic token (literals, identifiers, keywords, operators) - always has space before/after
    Atomic(String),
    /// Token that attaches to the previous token without space (like punctuation)
    Attached(String),
    /// Line break with optional count
    LineBreak { count: usize },
    /// A sequence of lines that can be rendered inline or as a block
    Listing {
        lines: Vec<Line>,
        layout_mode: ListingMode,
    },
    /// Postfix operation (method call, attribute access, subscript) - attaches without space
    Postfix {
        base: Box<Token>,
        operation: PostfixOp,
    },
    /// Grouped tokens (parentheses, brackets, braces)
    Grouped {
        open: String,
        inner: Vec<Line>,
        close: String,
        separator: Option<String>, // e.g., "," for lists
    },
    /// Inline comment attached to a token
    WithComment { token: Box<Token>, comment: String },
}

/// Types of postfix operations
#[derive(Debug, Clone)]
pub enum PostfixOp {
    /// Method call: `.method(args)`
    MethodCall {
        dot: String,
        method: String,
        args: Option<Vec<Line>>,
    },
    /// Attribute access: `.attr`
    Attribute { dot: String, name: String },
    /// Subscript: `[index]`
    Subscript {
        open: String,
        index: Vec<Line>,
        close: String,
    },
    /// Function call: `(args)`
    Call {
        open: String,
        args: Vec<Line>,
        close: String,
    },
}

/// How a listing should be laid out
#[derive(Debug, Clone)]
pub enum ListingMode {
    Inline,
    Block,
    Auto,
}

struct LayoutCalculator<'a> {
    config: &'a Config,
}

impl<'a> LayoutCalculator<'a> {
    fn new(config: &'a Config) -> Self {
        Self { config }
    }

    fn calculate_layout(&mut self, stmts: &SStmts, tokens: &[SToken]) -> Layout {
        let mut elements = Vec::new();

        // Add leading trivia from the first token
        if let Some(first_token) = tokens.first() {
            for trivium in &first_token.leading_trivia {
                elements.extend(self.trivium_to_layout_elements(trivium));
            }
        }

        for (i, stmt) in stmts.value.iter().enumerate() {
            if i > 0 {
                elements.push(LayoutElement::Newline { count: 1 });
            }
            elements.extend(self.layout_statement(stmt, tokens));
        }

        Layout { elements }
    }

    fn layout_statement(&mut self, stmt: &SStmt, tokens: &[SToken]) -> Vec<LayoutElement> {
        let mut elements = Vec::new();

        // Use the new token-based approach
        let statement_tokens = self.statement_to_tokens(stmt);
        if !statement_tokens.is_empty() {
            elements.push(LayoutElement::Line(Line {
                tokens: statement_tokens,
                indent_level: 0,
            }));
        }

        // Look for trailing trivia in tokens that belong to this statement
        // For now, we'll use a simple heuristic to find relevant tokens
        self.add_trailing_trivia_for_statement(stmt, tokens, &mut elements);

        elements
    }

    fn add_trailing_trivia_for_statement(
        &self,
        _stmt: &SStmt,
        tokens: &[SToken],
        elements: &mut Vec<LayoutElement>,
    ) {
        // Find tokens that might be associated with this statement
        // This is a simplified approach - in a real formatter you'd want more precise mapping
        for token in tokens {
            if !token.trailing_trivia.is_empty() {
                for trivium in &token.trailing_trivia {
                    // Only add line comments, not automatic newlines
                    match trivium.typ {
                        TriviumType::LineComment => {
                            elements.extend(self.trivium_to_layout_elements(trivium));
                        }
                        TriviumType::BlockComment => {
                            elements.extend(self.trivium_to_layout_elements(trivium));
                        }
                        // Skip automatic newlines and whitespace - we'll handle spacing ourselves
                        TriviumType::Newline | TriviumType::Whitespace => {}
                    }
                }
            }
        }
    }

    fn trivium_to_layout_elements(&self, trivium: &Trivium) -> Vec<LayoutElement> {
        match trivium.typ {
            TriviumType::LineComment => {
                vec![LayoutElement::Comment {
                    content: trivium.value.to_string(),
                    is_line_comment: true,
                }]
            }
            TriviumType::BlockComment => {
                vec![LayoutElement::Comment {
                    content: trivium.value.to_string(),
                    is_line_comment: false,
                }]
            }
            TriviumType::Newline => {
                vec![LayoutElement::Newline { count: 1 }]
            }
            TriviumType::Whitespace => {
                // For now, we'll handle whitespace implicitly through our formatting
                vec![]
            }
        }
    }

    fn statement_to_text(&self, stmt: &SStmt) -> String {
        // Deprecated: Use statement_to_tokens instead
        let tokens = self.statement_to_tokens(stmt);
        tokens
            .into_iter()
            .map(|token| match token {
                Token::Atomic(text) => text,
                _ => "/* complex token */".to_string(),
            })
            .collect()
    }

    fn statement_to_tokens(&self, stmt: &SStmt) -> Vec<Token> {
        match &stmt.value {
            Stmt::Expr { expr } => self.expr_to_tokens(expr),
            Stmt::PatternAssign {
                modifier, lhs, rhs, ..
            } => {
                let mut tokens = Vec::new();
                if let Some(modifier) = modifier {
                    tokens.push(Token::Atomic(self.token_to_text(modifier)));
                    tokens.push(Token::Atomic(" ".to_string()));
                }
                tokens.extend(self.pattern_to_tokens(lhs));
                tokens.push(Token::Atomic(" = ".to_string()));
                tokens.extend(self.expr_to_tokens(rhs));
                tokens
            }
            Stmt::Assign { lhs, rhs, .. } => {
                let mut tokens = Vec::new();
                tokens.extend(self.expr_to_tokens(lhs));
                tokens.push(Token::Atomic(" = ".to_string()));
                tokens.extend(self.expr_to_tokens(rhs));
                tokens
            }
            _ => vec![Token::Atomic("/* statement */".to_string())],
        }
    }

    fn expr_to_text(&self, expr: &SExpr) -> String {
        match &expr.value {
            Expr::Literal { token } => self.token_to_text(token),
            Expr::Ident { token } => self.token_to_text(token),
            Expr::Binary { lhs, rhs, op, .. } => {
                format!(
                    "{} {} {}",
                    self.expr_to_text(lhs),
                    op.token,
                    self.expr_to_text(rhs)
                )
            }
            Expr::List { listing } => {
                let item_strings: Vec<String> = match listing {
                    Listing::Inline { items, .. }
                    | Listing::Block { items, .. }
                    | Listing::Open { items } => items
                        .iter()
                        .map(|item| match &item.item {
                            ListItem::Item { expr } => self.expr_to_text(expr),
                            ListItem::Spread { expr, .. } => {
                                format!("...{}", self.expr_to_text(expr))
                            }
                        })
                        .collect(),
                };
                format!("[{}]", item_strings.join(", "))
            }
            Expr::Mapping { listing } => {
                let item_strings: Vec<String> = match listing {
                    Listing::Inline { items, .. }
                    | Listing::Block { items, .. }
                    | Listing::Open { items } => items
                        .iter()
                        .map(|item| match &item.item {
                            MappingItem::Item { key, value, .. } => {
                                let key_text = match &key.value {
                                    MappingKey::Ident { token } => self.token_to_text(token),
                                    MappingKey::Literal { token } => self.token_to_text(token),
                                    MappingKey::Expr { key, .. } => self.expr_to_text(key),
                                    _ => "/* key */".to_string(),
                                };
                                format!("{}: {}", key_text, self.expr_to_text(value))
                            }
                            MappingItem::Ident { ident } => self.token_to_text(ident),
                            MappingItem::Spread { expr, .. } => {
                                format!("...{}", self.expr_to_text(expr))
                            }
                        })
                        .collect(),
                };
                format!("{{{}}}", item_strings.join(", "))
            }
            Expr::MethodCall {
                expr, method, args, ..
            } => {
                let base_text = self.expr_to_text(expr);
                let method_name = self.token_to_text(method);
                let arg_strings: Vec<String> = match args {
                    Listing::Inline { items, .. }
                    | Listing::Block { items, .. }
                    | Listing::Open { items } => items
                        .iter()
                        .map(|item| match &item.item {
                            CallItem::Arg { expr } => self.expr_to_text(expr),
                            CallItem::ArgSpread { expr, .. } => {
                                format!("*{}", self.expr_to_text(expr))
                            }
                            CallItem::Kwarg { name, expr, .. } => {
                                format!("{}={}", self.token_to_text(name), self.expr_to_text(expr))
                            }
                            CallItem::KwargSpread { expr, .. } => {
                                format!("**{}", self.expr_to_text(expr))
                            }
                        })
                        .collect(),
                };
                format!("{}.{}({})", base_text, method_name, arg_strings.join(", "))
            }
            Expr::Attribute { expr, attr, .. } => {
                format!("{}.{}", self.expr_to_text(expr), self.token_to_text(attr))
            }
            Expr::Subscript { expr, indices, .. } => {
                let base_text = self.expr_to_text(expr);
                let index_strings: Vec<String> = match indices {
                    Listing::Inline { items, .. }
                    | Listing::Block { items, .. }
                    | Listing::Open { items } => items
                        .iter()
                        .map(|item| match &item.item {
                            ListItem::Item { expr } => self.expr_to_text(expr),
                            ListItem::Spread { expr, .. } => {
                                format!("...{}", self.expr_to_text(expr))
                            }
                        })
                        .collect(),
                };
                format!("{}[{}]", base_text, index_strings.join(", "))
            }
            Expr::Call { expr, args, .. } => {
                let base_text = self.expr_to_text(expr);
                let arg_strings: Vec<String> = match args {
                    Listing::Inline { items, .. }
                    | Listing::Block { items, .. }
                    | Listing::Open { items } => items
                        .iter()
                        .map(|item| match &item.item {
                            CallItem::Arg { expr } => self.expr_to_text(expr),
                            CallItem::ArgSpread { expr, .. } => {
                                format!("*{}", self.expr_to_text(expr))
                            }
                            CallItem::Kwarg { name, expr, .. } => {
                                format!("{}={}", self.token_to_text(name), self.expr_to_text(expr))
                            }
                            CallItem::KwargSpread { expr, .. } => {
                                format!("**{}", self.expr_to_text(expr))
                            }
                        })
                        .collect(),
                };
                format!("{}({})", base_text, arg_strings.join(", "))
            }
            Expr::If {
                cond,
                body,
                else_clause,
                ..
            } => {
                let condition_text = self.expr_to_text(cond);
                let then_text = self.colon_block_to_text(body);
                let else_text = if let Some((_, else_body)) = else_clause {
                    format!(" else {}", self.colon_block_to_text(else_body))
                } else {
                    String::new()
                };
                format!("{} then {}{}", condition_text, then_text, else_text)
            }
            Expr::Fn { arg, body } => {
                let param_text = self.pattern_to_text(arg);
                let body_text = self.arrow_block_to_text(body);
                format!("{} => {}", param_text, body_text)
            }
            Expr::ParenthesizedFn { args, body } => {
                let param_strings: Vec<String> = match args {
                    Listing::Inline { items, .. }
                    | Listing::Block { items, .. }
                    | Listing::Open { items } => items
                        .iter()
                        .map(|item| match &item.item {
                            ArgDefItem::Arg { pattern, .. } => self.pattern_to_text(pattern),
                            _ => "/* arg */".to_string(),
                        })
                        .collect(),
                };
                let param_text = if param_strings.len() == 1 {
                    param_strings[0].clone()
                } else {
                    format!("({})", param_strings.join(", "))
                };
                let body_text = self.arrow_block_to_text(body);
                format!("{} => {}", param_text, body_text)
            }
            Expr::Slice { start, stop, .. } => {
                let start_text = if let Some(start) = start {
                    self.expr_to_text(start)
                } else {
                    String::new()
                };
                let stop_text = if let Some(stop) = stop {
                    self.expr_to_text(stop)
                } else {
                    String::new()
                };
                format!("{}..{}", start_text, stop_text)
            }
            Expr::Unary { op, expr, .. } => {
                format!("{}{}", op.token, self.expr_to_text(expr))
            }
            Expr::Await { expr, .. } => {
                format!("@{}", self.expr_to_text(expr))
            }
            Expr::Memo { body, .. } => {
                format!("memo {}", self.colon_block_to_text(body))
            }
            Expr::Placeholder { token } => self.token_to_text(token),
            Expr::Parenthesized { expr, .. } => {
                format!("({})", self.expr_to_text(expr))
            }
            Expr::Match {
                scrutinee, cases, ..
            } => {
                let scrutinee_text = self.expr_to_text(scrutinee);
                let cases_text: Vec<String> = cases
                    .iter()
                    .map(|case| {
                        let pattern_text = self.pattern_to_text(&case.pattern);
                        let body_text = self.arrow_block_to_text(&case.body);
                        format!("    {} => {}", pattern_text, body_text)
                    })
                    .collect();
                format!("{} match:\n{}", scrutinee_text, cases_text.join("\n"))
            }
            _ => format!("/* expr */"),
        }
    }

    fn expr_to_tokens(&self, expr: &SExpr) -> Vec<Token> {
        match &expr.value {
            Expr::Literal { token } => vec![Token::Atomic(self.token_to_text(token))],
            Expr::Ident { token } => vec![Token::Atomic(self.token_to_text(token))],
            Expr::Binary { lhs, rhs, op, .. } => {
                let mut tokens = Vec::new();
                tokens.extend(self.expr_to_tokens(lhs));
                tokens.push(Token::Atomic(format!(" {} ", op.token)));
                tokens.extend(self.expr_to_tokens(rhs));
                tokens
            }
            Expr::Unary { op, expr, .. } => {
                let mut tokens = vec![Token::Atomic(op.token.to_string())];
                tokens.extend(self.expr_to_tokens(expr));
                tokens
            }
            Expr::List { listing } => {
                let mut tokens = vec![Token::Atomic("[".to_string())];
                let item_tokens: Vec<Token> = match listing {
                    Listing::Inline { items, .. }
                    | Listing::Block { items, .. }
                    | Listing::Open { items } => items
                        .iter()
                        .enumerate()
                        .flat_map(|(i, item)| {
                            let mut item_tokens = match &item.item {
                                ListItem::Item { expr } => self.expr_to_tokens(expr),
                                ListItem::Spread { expr, .. } => {
                                    let mut spread_tokens = vec![Token::Atomic("...".to_string())];
                                    spread_tokens.extend(self.expr_to_tokens(expr));
                                    spread_tokens
                                }
                            };
                            if i > 0 {
                                item_tokens.insert(0, Token::Atomic(", ".to_string()));
                            }
                            item_tokens
                        })
                        .collect(),
                };
                tokens.extend(item_tokens);
                tokens.push(Token::Atomic("]".to_string()));
                tokens
            }
            Expr::Mapping { listing } => {
                let mut tokens = vec![Token::Atomic("{".to_string())];
                let item_tokens: Vec<Token> = match listing {
                    Listing::Inline { items, .. }
                    | Listing::Block { items, .. }
                    | Listing::Open { items } => items
                        .iter()
                        .enumerate()
                        .flat_map(|(i, item)| {
                            let mut item_tokens = match &item.item {
                                MappingItem::Item { key, value, .. } => {
                                    let mut key_tokens = match &key.value {
                                        MappingKey::Ident { token } => {
                                            vec![Token::Atomic(self.token_to_text(token))]
                                        }
                                        MappingKey::Literal { token } => {
                                            vec![Token::Atomic(self.token_to_text(token))]
                                        }
                                        MappingKey::Expr { key, .. } => self.expr_to_tokens(key),
                                        _ => vec![Token::Atomic("/* key */".to_string())],
                                    };
                                    key_tokens.push(Token::Atomic(": ".to_string()));
                                    key_tokens.extend(self.expr_to_tokens(value));
                                    key_tokens
                                }
                                MappingItem::Ident { ident } => {
                                    vec![Token::Atomic(self.token_to_text(ident))]
                                }
                                MappingItem::Spread { expr, .. } => {
                                    let mut spread_tokens = vec![Token::Atomic("...".to_string())];
                                    spread_tokens.extend(self.expr_to_tokens(expr));
                                    spread_tokens
                                }
                            };
                            if i > 0 {
                                item_tokens.insert(0, Token::Atomic(", ".to_string()));
                            }
                            item_tokens
                        })
                        .collect(),
                };
                tokens.extend(item_tokens);
                tokens.push(Token::Atomic("}".to_string()));
                tokens
            }
            Expr::MethodCall {
                expr, method, args, ..
            } => {
                let mut tokens = Vec::new();
                tokens.extend(self.expr_to_tokens(expr));
                tokens.push(Token::Atomic(".".to_string()));
                tokens.push(Token::Atomic(self.token_to_text(method)));
                tokens.push(Token::Atomic("(".to_string()));

                let arg_tokens: Vec<Token> = match args {
                    Listing::Inline { items, .. }
                    | Listing::Block { items, .. }
                    | Listing::Open { items } => items
                        .iter()
                        .enumerate()
                        .flat_map(|(i, item)| {
                            let mut item_tokens = match &item.item {
                                CallItem::Arg { expr } => self.expr_to_tokens(expr),
                                CallItem::ArgSpread { expr, .. } => {
                                    let mut spread_tokens = vec![Token::Atomic("*".to_string())];
                                    spread_tokens.extend(self.expr_to_tokens(expr));
                                    spread_tokens
                                }
                                CallItem::Kwarg { name, expr, .. } => {
                                    let mut kwarg_tokens =
                                        vec![Token::Atomic(self.token_to_text(name))];
                                    kwarg_tokens.push(Token::Atomic("=".to_string()));
                                    kwarg_tokens.extend(self.expr_to_tokens(expr));
                                    kwarg_tokens
                                }
                                CallItem::KwargSpread { expr, .. } => {
                                    let mut spread_tokens = vec![Token::Atomic("**".to_string())];
                                    spread_tokens.extend(self.expr_to_tokens(expr));
                                    spread_tokens
                                }
                            };
                            if i > 0 {
                                item_tokens.insert(0, Token::Atomic(", ".to_string()));
                            }
                            item_tokens
                        })
                        .collect(),
                };
                tokens.extend(arg_tokens);
                tokens.push(Token::Atomic(")".to_string()));
                tokens
            }
            Expr::Attribute { expr, attr, .. } => {
                let mut tokens = Vec::new();
                tokens.extend(self.expr_to_tokens(expr));
                tokens.push(Token::Atomic(".".to_string()));
                tokens.push(Token::Atomic(self.token_to_text(attr)));
                tokens
            }
            Expr::Subscript { expr, indices, .. } => {
                let mut tokens = Vec::new();
                tokens.extend(self.expr_to_tokens(expr));
                tokens.push(Token::Atomic("[".to_string()));

                let index_tokens: Vec<Token> = match indices {
                    Listing::Inline { items, .. }
                    | Listing::Block { items, .. }
                    | Listing::Open { items } => items
                        .iter()
                        .enumerate()
                        .flat_map(|(i, item)| {
                            let mut item_tokens = match &item.item {
                                ListItem::Item { expr } => self.expr_to_tokens(expr),
                                ListItem::Spread { expr, .. } => {
                                    let mut spread_tokens = vec![Token::Atomic("...".to_string())];
                                    spread_tokens.extend(self.expr_to_tokens(expr));
                                    spread_tokens
                                }
                            };
                            if i > 0 {
                                item_tokens.insert(0, Token::Atomic(", ".to_string()));
                            }
                            item_tokens
                        })
                        .collect(),
                };
                tokens.extend(index_tokens);
                tokens.push(Token::Atomic("]".to_string()));
                tokens
            }
            Expr::Call { expr, args, .. } => {
                let mut tokens = Vec::new();
                tokens.extend(self.expr_to_tokens(expr));
                tokens.push(Token::Atomic("(".to_string()));

                let arg_tokens: Vec<Token> = match args {
                    Listing::Inline { items, .. }
                    | Listing::Block { items, .. }
                    | Listing::Open { items } => items
                        .iter()
                        .enumerate()
                        .flat_map(|(i, item)| {
                            let mut item_tokens = match &item.item {
                                CallItem::Arg { expr } => self.expr_to_tokens(expr),
                                CallItem::ArgSpread { expr, .. } => {
                                    let mut spread_tokens = vec![Token::Atomic("*".to_string())];
                                    spread_tokens.extend(self.expr_to_tokens(expr));
                                    spread_tokens
                                }
                                CallItem::Kwarg { name, expr, .. } => {
                                    let mut kwarg_tokens =
                                        vec![Token::Atomic(self.token_to_text(name))];
                                    kwarg_tokens.push(Token::Atomic("=".to_string()));
                                    kwarg_tokens.extend(self.expr_to_tokens(expr));
                                    kwarg_tokens
                                }
                                CallItem::KwargSpread { expr, .. } => {
                                    let mut spread_tokens = vec![Token::Atomic("**".to_string())];
                                    spread_tokens.extend(self.expr_to_tokens(expr));
                                    spread_tokens
                                }
                            };
                            if i > 0 {
                                item_tokens.insert(0, Token::Atomic(", ".to_string()));
                            }
                            item_tokens
                        })
                        .collect(),
                };
                tokens.extend(arg_tokens);
                tokens.push(Token::Atomic(")".to_string()));
                tokens
            }
            Expr::If {
                cond,
                body,
                else_clause,
                ..
            } => {
                let mut tokens = Vec::new();
                tokens.extend(self.expr_to_tokens(cond));
                tokens.push(Token::Atomic(" then ".to_string()));
                tokens.push(Token::Atomic(self.colon_block_to_text(body)));
                if let Some((_, else_body)) = else_clause {
                    tokens.push(Token::Atomic(" else ".to_string()));
                    tokens.push(Token::Atomic(self.colon_block_to_text(else_body)));
                }
                tokens
            }
            Expr::Fn { arg, body } => {
                let mut tokens = Vec::new();
                tokens.extend(self.pattern_to_tokens(arg));
                tokens.push(Token::Atomic(" => ".to_string()));
                tokens.push(Token::Atomic(self.arrow_block_to_text(body)));
                tokens
            }
            Expr::ParenthesizedFn { args, body } => {
                let mut tokens = Vec::new();
                let param_tokens: Vec<Token> = match args {
                    Listing::Inline { items, .. }
                    | Listing::Block { items, .. }
                    | Listing::Open { items } => items
                        .iter()
                        .enumerate()
                        .flat_map(|(i, item)| {
                            let mut item_tokens = match &item.item {
                                ArgDefItem::Arg { pattern, .. } => self.pattern_to_tokens(pattern),
                                _ => vec![Token::Atomic("/* arg */".to_string())],
                            };
                            if i > 0 {
                                item_tokens.insert(0, Token::Atomic(", ".to_string()));
                            }
                            item_tokens
                        })
                        .collect(),
                };
                if param_tokens.len() <= 1 {
                    tokens.extend(param_tokens);
                } else {
                    tokens.push(Token::Atomic("(".to_string()));
                    tokens.extend(param_tokens);
                    tokens.push(Token::Atomic(")".to_string()));
                }
                tokens.push(Token::Atomic(" => ".to_string()));
                tokens.push(Token::Atomic(self.arrow_block_to_text(body)));
                tokens
            }
            Expr::Slice { start, stop, .. } => {
                let mut tokens = Vec::new();
                if let Some(start) = start {
                    tokens.extend(self.expr_to_tokens(start));
                }
                tokens.push(Token::Atomic("..".to_string()));
                if let Some(stop) = stop {
                    tokens.extend(self.expr_to_tokens(stop));
                }
                tokens
            }
            Expr::Await { expr, .. } => {
                let mut tokens = vec![Token::Atomic("@".to_string())];
                tokens.extend(self.expr_to_tokens(expr));
                tokens
            }
            Expr::Memo { body, .. } => {
                let mut tokens = vec![Token::Atomic("memo ".to_string())];
                tokens.push(Token::Atomic(self.colon_block_to_text(body)));
                tokens
            }
            Expr::Placeholder { token } => vec![Token::Atomic(self.token_to_text(token))],
            Expr::Parenthesized { expr, .. } => {
                let mut tokens = vec![Token::Atomic("(".to_string())];
                tokens.extend(self.expr_to_tokens(expr));
                tokens.push(Token::Atomic(")".to_string()));
                tokens
            }
            Expr::Match {
                scrutinee, cases, ..
            } => {
                let mut tokens = Vec::new();
                tokens.extend(self.expr_to_tokens(scrutinee));
                tokens.push(Token::Atomic(" match:".to_string()));
                tokens.push(Token::LineBreak { count: 1 });
                for case in cases {
                    tokens.push(Token::Atomic("    ".to_string()));
                    tokens.extend(self.pattern_to_tokens(&case.pattern));
                    tokens.push(Token::Atomic(" => ".to_string()));
                    tokens.push(Token::Atomic(self.arrow_block_to_text(&case.body)));
                    tokens.push(Token::LineBreak { count: 1 });
                }
                tokens
            }
            _ => vec![Token::Atomic("/* expr */".to_string())],
        }
    }

    fn pattern_to_tokens(&self, pattern: &SPattern) -> Vec<Token> {
        match &pattern.value {
            Pattern::Capture { name } => vec![Token::Atomic(self.token_to_text(name))],
            Pattern::Literal { token } => vec![Token::Atomic(self.token_to_text(token))],
            _ => vec![Token::Atomic("/* pattern */".to_string())],
        }
    }

    fn pattern_to_text(&self, pattern: &SPattern) -> String {
        // Deprecated: Use pattern_to_tokens instead
        let tokens = self.pattern_to_tokens(pattern);
        tokens
            .into_iter()
            .map(|token| match token {
                Token::Atomic(text) => text,
                _ => "/* complex token */".to_string(),
            })
            .collect()
    }

    fn token_to_text(&self, token: &koatl_parser::lexer::SToken) -> String {
        use koatl_parser::lexer::Token;

        match &token.token {
            Token::Ident(s) => s.to_string(),
            Token::None => "None".to_string(),
            Token::Bool(b) => b.to_string(),
            Token::Int(s) => s.to_string(),
            Token::IntBin(s) => s.to_string(),
            Token::IntOct(s) => s.to_string(),
            Token::IntHex(s) => s.to_string(),
            Token::Float(s) => s.to_string(),
            Token::Kw(s) => s.to_string(),
            Token::Symbol(s) => s.to_string(),
            Token::Str(s, _) => s.to_string(),
            Token::FstrInner(s, _) => s.to_string(),
            Token::FstrBegin(s) => s.to_string(),
            Token::FstrEnd(s) => s.to_string(),
            Token::VerbatimFstrBegin(s) => s.to_string(),
            Token::VerbatimFstrEnd(s) => s.to_string(),
            Token::Indent | Token::Dedent | Token::Eol => panic!(),
        }
    }

    fn colon_block_to_text(&self, block: &ColonBlock<STree>) -> String {
        match block {
            ColonBlock::Inline { stmt, .. } => {
                // For inline blocks, extract the statement
                match &stmt.value {
                    Stmt::Expr { expr } => self.expr_to_text(expr),
                    _ => self.statement_to_text(stmt),
                }
            }
            ColonBlock::Block { body, .. } => {
                // For block statements, if it's a single expression, inline it
                if body.value.len() == 1 {
                    if let Stmt::Expr { expr } = &body.value[0].value {
                        return self.expr_to_text(expr);
                    }
                }
                format!("/* block */")
            }
        }
    }

    fn arrow_block_to_text(&self, block: &ArrowBlock<STree>) -> String {
        match block {
            ArrowBlock::Inline { stmt, .. } => {
                // For inline blocks, extract the statement
                match &stmt.value {
                    Stmt::Expr { expr } => self.expr_to_text(expr),
                    _ => self.statement_to_text(stmt),
                }
            }
            ArrowBlock::Block { body, .. } => {
                // For block statements, if it's a single expression, inline it
                if body.value.len() == 1 {
                    if let Stmt::Expr { expr } = &body.value[0].value {
                        return self.expr_to_text(expr);
                    }
                }
                format!("/* block */")
            }
        }
    }
}

/// Phase 2: Generate output from layout decisions
/// The LayoutWriter doesn't concern itself with line length decisions -
/// that's already been handled by the layout calculation phase.
struct LayoutWriter<'a> {
    config: &'a Config,
    output: String,
}

impl<'a> LayoutWriter<'a> {
    fn new(config: &'a Config) -> Self {
        Self {
            config,
            output: String::new(),
        }
    }

    fn generate_output(&mut self, layout: Layout) -> String {
        for element in layout.elements {
            self.render_element(&element);
        }

        // Clean up trailing whitespace and normalize newlines
        self.output.trim_end().to_string()
    }

    fn render_element(&mut self, element: &LayoutElement) {
        match element {
            LayoutElement::Text { content, .. } => {
                self.output.push_str(content);
            }
            LayoutElement::Newline { count } => {
                for _ in 0..*count {
                    self.output.push('\n');
                }
            }
            LayoutElement::Comment {
                content,
                is_line_comment,
            } => {
                // Add space before comment if needed
                if !self.output.ends_with(' ') && !self.output.ends_with('\n') {
                    self.output.push(' ');
                }

                self.output.push_str(content);

                // Add newline after line comment if needed
                if *is_line_comment && !content.ends_with('\n') {
                    self.output.push('\n');
                }
            }
            LayoutElement::Line(line) => {
                self.render_line(line);
            }
            LayoutElement::ColonBlock {
                colon_line,
                body,
                is_inline,
            } => {
                self.render_line(colon_line);
                if *is_inline {
                    if !body.is_empty() {
                        self.output.push(' ');
                        self.render_line(&body[0]);
                    }
                } else {
                    self.render_block_body(body);
                }
            }
            LayoutElement::ArrowBlock {
                arrow_line,
                body,
                is_inline,
            } => {
                self.render_line(arrow_line);
                if *is_inline {
                    if !body.is_empty() {
                        self.output.push(' ');
                        self.render_line(&body[0]);
                    }
                } else {
                    self.render_block_body(body);
                }
            }
        }
    }

    fn render_line(&mut self, line: &Line) {
        // Add indentation based on line's indent level
        self.add_indentation(line.indent_level);

        // Render tokens with intelligent spacing
        for (i, token) in line.tokens.iter().enumerate() {
            if i > 0 {
                let prev_token = &line.tokens[i - 1];
                if self.needs_space_between(prev_token, token) {
                    self.output.push(' ');
                }
            }
            self.render_token(token);
        }
    }

    /// Determine if a space is needed between two consecutive tokens
    fn needs_space_between(&self, prev: &Token, current: &Token) -> bool {
        match (prev, current) {
            // No space if current token is attached
            (_, Token::Attached(_)) => false,
            // No space for postfix operations
            (_, Token::Postfix { .. }) => false,
            // Line breaks don't need spaces
            (Token::LineBreak { .. }, _) | (_, Token::LineBreak { .. }) => false,
            // Otherwise, atomic tokens and other types need spaces
            _ => true,
        }
    }
    fn add_indentation(&mut self, level: usize) {
        // Only add indentation if we're at the start of a line
        if self.output.ends_with('\n') || self.output.is_empty() {
            for _ in 0..(level * self.config.indent_width) {
                self.output.push(' ');
            }
        }
    }

    fn render_token(&mut self, token: &Token) {
        match token {
            Token::Atomic(content) => {
                self.output.push_str(content);
            }
            Token::Attached(content) => {
                self.output.push_str(content);
            }
            Token::LineBreak { count } => {
                for _ in 0..*count {
                    self.output.push('\n');
                }
            }
            Token::Listing { lines, layout_mode } => {
                self.render_listing(lines, layout_mode);
            }
            Token::Postfix { base, operation } => {
                self.render_token(base);
                self.render_postfix_op(operation);
            }
            Token::Grouped {
                open,
                inner,
                close,
                separator,
            } => {
                self.output.push_str(open);

                for (i, line) in inner.iter().enumerate() {
                    if i > 0 {
                        if let Some(sep) = separator {
                            self.output.push_str(sep);
                        }
                        self.output.push(' ');
                    }
                    self.render_line(line);
                }

                self.output.push_str(close);
            }
            Token::WithComment { token, comment } => {
                self.render_token(token);
                self.output.push(' ');
                self.output.push_str(comment);
            }
        }
    }

    fn render_postfix_op(&mut self, op: &PostfixOp) {
        match op {
            PostfixOp::MethodCall { dot, method, args } => {
                self.output.push_str(dot);
                self.output.push_str(method);

                if let Some(args) = args {
                    self.output.push('(');

                    for (i, arg_line) in args.iter().enumerate() {
                        if i > 0 {
                            self.output.push_str(", ");
                        }
                        self.render_line(arg_line);
                    }

                    self.output.push(')');
                }
            }
            PostfixOp::Attribute { dot, name } => {
                self.output.push_str(dot);
                self.output.push_str(name);
            }
            PostfixOp::Subscript { open, index, close } => {
                self.output.push_str(open);

                for (i, index_line) in index.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.render_line(index_line);
                }

                self.output.push_str(close);
            }
            PostfixOp::Call { open, args, close } => {
                self.output.push_str(open);

                for (i, arg_line) in args.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.render_line(arg_line);
                }

                self.output.push_str(close);
            }
        }
    }

    fn render_listing(&mut self, lines: &[Line], layout_mode: &ListingMode) {
        match layout_mode {
            ListingMode::Inline => {
                for (i, line) in lines.iter().enumerate() {
                    if i > 0 {
                        self.output.push(' ');
                    }
                    self.render_line(line);
                }
            }
            ListingMode::Block => {
                for line in lines {
                    self.output.push('\n');
                    self.render_line(line);
                }
            }
            ListingMode::Auto => {
                // This should have been resolved during layout calculation
                // For safety, fall back to inline for single lines, block for multiple
                if lines.len() <= 1 {
                    self.render_listing(lines, &ListingMode::Inline);
                } else {
                    self.render_listing(lines, &ListingMode::Block);
                }
            }
        }
    }

    fn render_block_body(&mut self, body: &[Line]) {
        for line in body {
            self.output.push('\n');
            self.render_line(line);
        }
    }
}
