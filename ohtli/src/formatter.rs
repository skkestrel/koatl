use anyhow::Result;
use koatl_parser::cst::*;
use koatl_parser::{lexer::tokenize, parse_tokens};

use crate::config::Config;

pub struct Formatter {
    config: Config,
}

impl Formatter {
    pub fn new(config: Config) -> Self {
        Self { config }
    }

    pub fn format(&self, source: &str) -> Result<String> {
        // Tokenize the source code
        let (tokens, lex_errors) = tokenize(source, true);

        if !lex_errors.is_empty() {
            anyhow::bail!("Lexing errors: {:?}", lex_errors);
        }

        let tokens = tokens.ok_or_else(|| anyhow::anyhow!("Failed to tokenize source"))?;

        // Parse the tokens into a CST
        let (cst, parse_errors) = parse_tokens(source, &tokens);

        if !parse_errors.is_empty() {
            anyhow::bail!("Parsing errors: {:?}", parse_errors);
        }

        let cst = cst.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))?;

        // Format the CST back to source code
        let mut formatter = CstFormatter::new(&self.config);
        Ok(formatter.format_statements(&cst))
    }
}

struct CstFormatter<'a> {
    config: &'a Config,
    output: String,
    current_indent: usize,
}

impl<'a> CstFormatter<'a> {
    fn new(config: &'a Config) -> Self {
        Self {
            config,
            output: String::new(),
            current_indent: 0,
        }
    }

    fn format_statements(&mut self, stmts: &SStmts) -> String {
        for stmt in &stmts.value {
            self.format_statement(stmt);
            self.output.push('\n');
        }

        // Remove trailing newline if present
        if self.output.ends_with('\n') {
            self.output.pop();
        }

        self.output.clone()
    }

    fn format_statement(&mut self, stmt: &SStmt) {
        self.write_indent();

        match &stmt.value {
            Stmt::Expr { expr } => {
                self.format_expression(expr);
            }
            Stmt::PatternAssign {
                modifier,
                lhs,
                eq: _,
                rhs,
            } => {
                if let Some(modifier) = modifier {
                    self.write_token_content(modifier);
                    self.output.push(' ');
                }
                self.format_pattern(lhs);
                self.output.push_str(" = ");
                self.format_expression(rhs);
            }
            Stmt::Assign {
                lhs,
                op: _,
                eq: _,
                rhs,
            } => {
                self.format_expression(lhs);
                self.output.push_str(" = ");
                self.format_expression(rhs);
            }
            Stmt::Return { return_kw: _, expr } => {
                self.output.push_str("return ");
                self.format_expression(expr);
            }
            Stmt::Break { break_kw: _ } => {
                self.output.push_str("break");
            }
            Stmt::Continue { continue_kw: _ } => {
                self.output.push_str("continue");
            }
            // TODO: Add support for more statement types:
            // - Stmt::Decl
            // - Stmt::While
            // - Stmt::For
            // - Stmt::Import
            // - Stmt::Raise
            _ => {
                // Fallback: preserve original formatting for unhandled cases
                self.output.push_str("/* unhandled statement */");
            }
        }
    }

    fn format_expression(&mut self, expr: &SExpr) {
        match &expr.value {
            Expr::Literal { token } => {
                self.write_token_content(token);
            }
            Expr::Ident { token } => {
                self.write_token_content(token);
            }
            Expr::Binary {
                lhs,
                not: _,
                op: _,
                op_kind,
                rhs,
            } => {
                self.format_expression(lhs);
                self.output.push(' ');
                self.write_binary_op(*op_kind);
                self.output.push(' ');
                self.format_expression(rhs);
            }
            Expr::Unary {
                op: _,
                op_kind,
                expr,
            } => {
                self.write_unary_op(*op_kind);
                self.format_expression(expr);
            }
            Expr::Call {
                expr,
                question: _,
                args,
            } => {
                self.format_expression(expr);
                self.format_call_args(args);
            }
            Expr::Parenthesized {
                lparen: _,
                expr,
                rparen: _,
            } => {
                self.output.push('(');
                self.format_expression(expr);
                self.output.push(')');
            }
            // TODO: Add support for more expression types:
            // - Expr::Fn (function literals)
            // - Expr::ParenthesizedFn
            // - Expr::Match and Expr::ClassicMatch
            // - Expr::If and Expr::ClassicIf
            // - Expr::List, Expr::Mapping, Expr::Tuple
            // - Expr::Slice
            // - Expr::Attribute, Expr::Subscript, Expr::MethodCall
            // - Expr::Class, Expr::With, Expr::Try
            // - Expr::Await, Expr::Yield, Expr::Memo
            // - Expr::Checked
            // - Expr::Fstr
            // - Expr::ParenthesizedBlock
            _ => {
                // Fallback: preserve original formatting for unhandled cases
                self.output.push_str("/* unhandled expression */");
            }
        }
    }

    fn format_pattern(&mut self, pattern: &SPattern) {
        match &pattern.value {
            Pattern::Capture { name } => {
                self.write_token_content(name);
            }
            Pattern::Literal { token } => {
                self.write_token_content(token);
            }
            // Add more pattern types as needed
            _ => {
                self.output.push_str("/* unhandled pattern */");
            }
        }
    }

    fn format_call_args(&mut self, args: &SListing<SCallItem>) {
        self.output.push('(');

        match args {
            Listing::Inline {
                begin: _,
                items,
                newline: _,
                end: _,
            } => {
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.format_call_item(&item.item);
                }
            }
            Listing::Block {
                begin: _,
                indent: _,
                items,
                dedent: _,
                newline: _,
                end: _,
            } => {
                if !items.is_empty() {
                    self.output.push('\n');
                    self.current_indent += 1;

                    for (i, item) in items.iter().enumerate() {
                        if i > 0 {
                            self.output.push_str(",\n");
                        }
                        self.write_indent();
                        self.format_call_item(&item.item);
                    }

                    self.current_indent -= 1;
                    self.output.push('\n');
                    self.write_indent();
                }
            }
            Listing::Open { items } => {
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.format_call_item(&item.item);
                }
            }
        }

        self.output.push(')');
    }

    fn format_call_item(&mut self, item: &SCallItem) {
        match item {
            CallItem::Arg { expr } => {
                self.format_expression(expr);
            }
            CallItem::Kwarg { name, eq: _, expr } => {
                self.write_token_content(name);
                self.output.push('=');
                self.format_expression(expr);
            }
            // Add more call item types as needed
            _ => {
                self.output.push_str("/* unhandled call item */");
            }
        }
    }

    fn write_binary_op(&mut self, op: BinaryOp) {
        let op_str = match op {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Eq => "==",
            BinaryOp::Neq => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Gt => ">",
            BinaryOp::Leq => "<=",
            BinaryOp::Geq => ">=",
            BinaryOp::And => "and",
            BinaryOp::Or => "or",
            // Add more operators as needed
            _ => "/* unknown op */",
        };
        self.output.push_str(op_str);
    }

    fn write_unary_op(&mut self, op: UnaryOp) {
        let op_str = match op {
            UnaryOp::Neg => "-",
            UnaryOp::Pos => "+",
            UnaryOp::Not => "not ",
            UnaryOp::Inv => "~",
            UnaryOp::Bind => "&",
        };
        self.output.push_str(op_str);
    }

    fn write_token_content(&mut self, token: &koatl_parser::lexer::SToken) {
        use koatl_parser::lexer::Token;

        // Extract the actual text content from the token
        match &token.token {
            Token::Ident(s) => self.output.push_str(s),
            Token::None => self.output.push_str("None"),
            Token::Bool(b) => self.output.push_str(&b.to_string()),
            Token::Str(s) => {
                self.output.push('"');
                self.output.push_str(s);
                self.output.push('"');
            }
            Token::Int(s) => self.output.push_str(s),
            Token::IntBin(s) => {
                self.output.push_str("0b");
                self.output.push_str(s);
            }
            Token::IntOct(s) => {
                self.output.push_str("0o");
                self.output.push_str(s);
            }
            Token::IntHex(s) => {
                self.output.push_str("0x");
                self.output.push_str(s);
            }
            Token::Float(s) => self.output.push_str(s),
            Token::Kw(s) => self.output.push_str(s),
            Token::Symbol(s) => self.output.push_str(s),
            Token::FstrBegin(s) => self.output.push_str(s),
            Token::FstrContinue(s) => self.output.push_str(s),
            Token::Indent | Token::Dedent | Token::Eol => {
                // These are handled by the formatter structure, not printed directly
            }
        }
    }

    fn write_indent(&mut self) {
        for _ in 0..self.current_indent {
            for _ in 0..self.config.indent_width {
                self.output.push(' ');
            }
        }
    }
}
