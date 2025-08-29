use anyhow::Result;
use koatl_parser::cst::*;
use koatl_parser::lexer::{tokenize, SToken};
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
        let layout = layout_calculator.calculate_layout(&cst);

        let mut output_generator = OutputGenerator::new(&self.config);
        Ok(output_generator.generate_output(layout))
    }
}

/// Represents the layout decisions for formatting
#[derive(Debug, Clone)]
pub struct Layout {
    pub elements: Vec<LayoutElement>,
}

#[derive(Debug, Clone)]
pub enum LayoutElement {
    Text {
        content: String,
        preserve_trivia: bool,
    },
    Newline {
        count: usize,
    },
    Indent {
        level: usize,
    },
    Space {
        count: usize,
    },
    Comment {
        content: String,
        is_line_comment: bool,
    },
    Group {
        elements: Vec<LayoutElement>,
        break_mode: BreakMode,
    },
}

#[derive(Debug, Clone)]
pub enum BreakMode {
    /// Never break, keep everything on one line
    Never,
    /// Break if it doesn't fit within max line length
    IfNeeded,
    /// Always break into multiple lines
    Always,
}

/// Phase 1: Calculate layout decisions
struct LayoutCalculator<'a> {
    config: &'a Config,
    current_indent: usize,
}

impl<'a> LayoutCalculator<'a> {
    fn new(config: &'a Config) -> Self {
        Self {
            config,
            current_indent: 0,
        }
    }

    fn layout_token(&self, token: &SToken) -> Vec<LayoutElement> {
        let mut elements = Vec::new();

        // Add leading trivia
        for trivium in &token.leading_trivia {
            elements.extend(self.layout_trivium(trivium));
        }

        // Add the token content
        elements.push(LayoutElement::Text {
            content: token.token.to_string(),
            preserve_trivia: false,
        });

        // Add trailing trivia
        for trivium in &token.trailing_trivia {
            elements.extend(self.layout_trivium(trivium));
        }

        elements
    }

    fn layout_trivium(&self, trivium: &koatl_parser::lexer::Trivium) -> Vec<LayoutElement> {
        use koatl_parser::lexer::TriviumType;

        match trivium.typ {
            TriviumType::Newline => vec![LayoutElement::Newline { count: 1 }],
            TriviumType::Whitespace => {
                if trivium.value.chars().all(|c| c == ' ') {
                    vec![LayoutElement::Space {
                        count: trivium.value.len(),
                    }]
                } else {
                    vec![LayoutElement::Text {
                        content: trivium.value.to_string(),
                        preserve_trivia: true,
                    }]
                }
            }
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
        }
    }

    fn calculate_layout(&mut self, stmts: &SStmts) -> Layout {
        let mut elements = Vec::new();

        for (i, stmt) in stmts.value.iter().enumerate() {
            if i > 0 {
                elements.push(LayoutElement::Newline { count: 1 });
            }
            elements.extend(self.layout_statement(stmt));
        }

        Layout { elements }
    }

    fn layout_statement(&mut self, stmt: &SStmt) -> Vec<LayoutElement> {
        let mut elements = vec![LayoutElement::Indent {
            level: self.current_indent,
        }];

        match &stmt.value {
            Stmt::Expr { expr } => {
                elements.extend(self.layout_expression(expr));
            }
            Stmt::PatternAssign {
                modifier,
                lhs,
                eq: _,
                rhs,
            } => {
                if let Some(modifier) = modifier {
                    elements.extend(self.layout_token_with_trivia(modifier));
                    elements.push(LayoutElement::Space { count: 1 });
                }
                elements.extend(self.layout_pattern(lhs));
                elements.push(LayoutElement::Space { count: 1 });
                elements.push(LayoutElement::Text {
                    content: "=".to_string(),
                    preserve_trivia: false,
                });
                elements.push(LayoutElement::Space { count: 1 });
                elements.extend(self.layout_expression(rhs));
            }
            Stmt::Assign {
                lhs,
                op: _,
                eq: _,
                rhs,
            } => {
                elements.extend(self.layout_expression(lhs));
                elements.push(LayoutElement::Space { count: 1 });
                elements.push(LayoutElement::Text {
                    content: "=".to_string(),
                    preserve_trivia: false,
                });
                elements.push(LayoutElement::Space { count: 1 });
                elements.extend(self.layout_expression(rhs));
            }
            Stmt::Return { return_kw: _, expr } => {
                elements.push(LayoutElement::Text {
                    content: "return".to_string(),
                    preserve_trivia: false,
                });
                elements.push(LayoutElement::Space { count: 1 });
                elements.extend(self.layout_expression(expr));
            }
            Stmt::Break { break_kw: _ } => {
                elements.push(LayoutElement::Text {
                    content: "break".to_string(),
                    preserve_trivia: false,
                });
            }
            Stmt::Continue { continue_kw: _ } => {
                elements.push(LayoutElement::Text {
                    content: "continue".to_string(),
                    preserve_trivia: false,
                });
            }
            Stmt::Decl { modifier, names } => {
                elements.extend(self.layout_token_with_trivia(modifier));
                elements.push(LayoutElement::Space { count: 1 });
                for (i, (name, _comma)) in names.iter().enumerate() {
                    if i > 0 {
                        elements.push(LayoutElement::Text {
                            content: ",".to_string(),
                            preserve_trivia: false,
                        });
                        elements.push(LayoutElement::Space { count: 1 });
                    }
                    elements.extend(self.layout_token_with_trivia(name));
                }
            }
            Stmt::While {
                while_kw,
                cond,
                body,
            } => {
                elements.extend(self.layout_token_with_trivia(while_kw));
                elements.push(LayoutElement::Space { count: 1 });
                elements.extend(self.layout_expression(cond));
                elements.extend(self.layout_colon_block(body));
            }
            Stmt::For {
                for_kw,
                pattern,
                in_kw,
                iter,
                body,
            } => {
                elements.extend(self.layout_token_with_trivia(for_kw));
                elements.push(LayoutElement::Space { count: 1 });
                elements.extend(self.layout_pattern(pattern));
                elements.push(LayoutElement::Space { count: 1 });
                elements.extend(self.layout_token_with_trivia(in_kw));
                elements.push(LayoutElement::Space { count: 1 });
                elements.extend(self.layout_expression(iter));
                elements.extend(self.layout_colon_block(body));
            }
            Stmt::Import {
                export,
                import,
                tree,
            } => {
                if let Some(export_token) = export {
                    elements.extend(self.layout_token_with_trivia(export_token));
                    elements.push(LayoutElement::Space { count: 1 });
                }
                elements.extend(self.layout_token_with_trivia(import));
                elements.push(LayoutElement::Space { count: 1 });
                elements.extend(self.layout_import_tree(tree));
            }
            Stmt::Raise { raise_kw, expr } => {
                elements.extend(self.layout_token_with_trivia(raise_kw));
                if let Some(expr) = expr {
                    elements.push(LayoutElement::Space { count: 1 });
                    elements.extend(self.layout_expression(expr));
                }
            }
            _ => {
                elements.push(LayoutElement::Text {
                    content: "/* unhandled statement */".to_string(),
                    preserve_trivia: false,
                });
            }
        }

        elements
    }

    fn layout_expression(&mut self, expr: &SExpr) -> Vec<LayoutElement> {
        match &expr.value {
            Expr::Literal { token } => self.layout_token_with_trivia(token),
            Expr::Ident { token } => self.layout_token_with_trivia(token),
            Expr::Binary {
                lhs,
                not: _,
                op: _,
                op_kind,
                rhs,
            } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_expression(lhs));

                // TODO: Add spacing config back when available
                elements.push(LayoutElement::Space { count: 1 });

                elements.push(LayoutElement::Text {
                    content: self.binary_op_to_string(*op_kind),
                    preserve_trivia: false,
                });

                elements.push(LayoutElement::Space { count: 1 });

                elements.extend(self.layout_expression(rhs));
                elements
            }
            Expr::Unary {
                op: _,
                op_kind,
                expr,
            } => {
                let mut elements = Vec::new();
                elements.push(LayoutElement::Text {
                    content: self.unary_op_to_string(*op_kind),
                    preserve_trivia: false,
                });
                elements.extend(self.layout_expression(expr));
                elements
            }
            Expr::Call {
                expr,
                question: _,
                args,
            } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_expression(expr));
                elements.extend(self.layout_call_args(args));
                elements
            }
            Expr::Parenthesized {
                lparen: _,
                expr,
                rparen: _,
            } => {
                let mut elements = Vec::new();
                elements.push(LayoutElement::Text {
                    content: "(".to_string(),
                    preserve_trivia: false,
                });
                elements.extend(self.layout_expression(expr));
                elements.push(LayoutElement::Text {
                    content: ")".to_string(),
                    preserve_trivia: false,
                });
                elements
            }
            Expr::Fn { arg, body } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_pattern(arg));
                elements.push(LayoutElement::Space { count: 1 });
                elements.extend(self.layout_arrow_block(body));
                elements
            }
            Expr::ParenthesizedFn { args, body } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_arg_def_listing(args));
                elements.push(LayoutElement::Space { count: 1 });
                elements.extend(self.layout_arrow_block(body));
                elements
            }
            Expr::If {
                cond,
                then_kw,
                body,
                else_clause,
            } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_expression(cond));
                elements.push(LayoutElement::Space { count: 1 });
                elements.extend(self.layout_token_with_trivia(then_kw));
                elements.push(LayoutElement::Space { count: 1 });
                elements.extend(self.layout_colon_block(body));
                if let Some((else_kw, else_body)) = else_clause {
                    elements.push(LayoutElement::Space { count: 1 });
                    elements.extend(self.layout_token_with_trivia(else_kw));
                    elements.push(LayoutElement::Space { count: 1 });
                    elements.extend(self.layout_colon_block(else_body));
                }
                elements
            }
            Expr::ClassicIf {
                if_kw,
                cond,
                body,
                else_clause,
            } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_token_with_trivia(if_kw));
                elements.push(LayoutElement::Space { count: 1 });
                elements.extend(self.layout_expression(cond));
                elements.extend(self.layout_colon_block(body));
                if let Some((else_kw, else_body)) = else_clause {
                    elements.push(LayoutElement::Space { count: 1 });
                    elements.extend(self.layout_token_with_trivia(else_kw));
                    elements.extend(self.layout_colon_block(else_body));
                }
                elements
            }
            Expr::List { listing } => self.layout_list_listing(listing),
            Expr::Tuple { kind } => self.layout_tuple_kind(kind),
            Expr::Mapping { listing } => self.layout_mapping_listing(listing),
            Expr::Slice {
                start,
                dots,
                stop,
                step_dots,
                step,
            } => {
                let mut elements = Vec::new();
                if let Some(start_expr) = start {
                    elements.extend(self.layout_expression(start_expr));
                }
                elements.extend(self.layout_token_with_trivia(dots));
                if let Some(stop_expr) = stop {
                    elements.extend(self.layout_expression(stop_expr));
                }
                if let Some(step_dots_token) = step_dots {
                    elements.extend(self.layout_token_with_trivia(step_dots_token));
                    if let Some(step_expr) = step {
                        elements.extend(self.layout_expression(step_expr));
                    }
                }
                elements
            }
            Expr::Attribute {
                expr,
                question,
                dot,
                attr,
            } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_expression(expr));
                if let Some(question_token) = question {
                    elements.extend(self.layout_token_with_trivia(question_token));
                }
                elements.extend(self.layout_token_with_trivia(dot));
                elements.extend(self.layout_token_with_trivia(attr));
                elements
            }
            Expr::MethodCall {
                expr,
                question,
                dot,
                method,
                args,
            } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_expression(expr));
                if let Some(question_token) = question {
                    elements.extend(self.layout_token_with_trivia(question_token));
                }
                elements.extend(self.layout_token_with_trivia(dot));
                elements.extend(self.layout_token_with_trivia(method));
                elements.extend(self.layout_call_args(args));
                elements
            }
            Expr::Subscript {
                expr,
                question,
                indices,
            } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_expression(expr));
                if let Some(question_token) = question {
                    elements.extend(self.layout_token_with_trivia(question_token));
                }
                elements.push(LayoutElement::Text {
                    content: "[".to_string(),
                    preserve_trivia: false,
                });
                elements.extend(self.layout_subscript_listing(indices));
                elements.push(LayoutElement::Text {
                    content: "]".to_string(),
                    preserve_trivia: false,
                });
                elements
            }
            Expr::Await { await_kw, expr } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_token_with_trivia(await_kw));
                elements.push(LayoutElement::Space { count: 1 });
                elements.extend(self.layout_expression(expr));
                elements
            }
            Expr::Yield {
                yield_kw,
                from_kw,
                expr,
            } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_token_with_trivia(yield_kw));
                if let Some(from_token) = from_kw {
                    elements.push(LayoutElement::Space { count: 1 });
                    elements.extend(self.layout_token_with_trivia(from_token));
                }
                elements.push(LayoutElement::Space { count: 1 });
                elements.extend(self.layout_expression(expr));
                elements
            }
            Expr::Memo {
                async_kw,
                memo_kw,
                body,
            } => {
                let mut elements = Vec::new();
                if let Some(async_token) = async_kw {
                    elements.extend(self.layout_token_with_trivia(async_token));
                    elements.push(LayoutElement::Space { count: 1 });
                }
                elements.extend(self.layout_token_with_trivia(memo_kw));
                elements.push(LayoutElement::Space { count: 1 });
                elements.extend(self.layout_colon_block(body));
                elements
            }
            Expr::Match {
                scrutinee,
                match_kw,
                colon: _,
                indent: _,
                cases,
                dedent: _,
            } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_expression(scrutinee));
                elements.push(LayoutElement::Space { count: 1 });
                elements.extend(self.layout_token_with_trivia(match_kw));
                elements.push(LayoutElement::Text {
                    content: ":".to_string(),
                    preserve_trivia: false,
                });
                elements.push(LayoutElement::Newline { count: 1 });

                for case in cases {
                    elements.push(LayoutElement::Space { count: 4 }); // TODO: Use config
                    elements.extend(self.layout_pattern(&case.pattern));
                    elements.push(LayoutElement::Space { count: 1 });
                    elements.extend(self.layout_arrow_block(&case.body));
                    elements.push(LayoutElement::Newline { count: 1 });
                }
                elements
            }
            Expr::ClassicMatch {
                match_kw,
                scrutinee,
                colon: _,
                indent: _,
                cases,
                dedent: _,
            } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_token_with_trivia(match_kw));
                elements.push(LayoutElement::Space { count: 1 });
                elements.extend(self.layout_expression(scrutinee));
                elements.push(LayoutElement::Text {
                    content: ":".to_string(),
                    preserve_trivia: false,
                });
                elements.push(LayoutElement::Newline { count: 1 });

                for case in cases {
                    elements.push(LayoutElement::Space { count: 4 }); // TODO: Use config
                    elements.extend(self.layout_pattern(&case.pattern));
                    elements.push(LayoutElement::Space { count: 1 });
                    elements.extend(self.layout_arrow_block(&case.body));
                    elements.push(LayoutElement::Newline { count: 1 });
                }
                elements
            }
            Expr::Placeholder { token } => self.layout_token_with_trivia(token),
            _ => vec![LayoutElement::Text {
                content: "/* unhandled expression */".to_string(),
                preserve_trivia: false,
            }],
        }
    }

    fn layout_pattern(&mut self, pattern: &SPattern) -> Vec<LayoutElement> {
        match &pattern.value {
            Pattern::Capture { name } => self.layout_token_with_trivia(name),
            Pattern::Literal { token } => self.layout_token_with_trivia(token),
            _ => vec![LayoutElement::Text {
                content: "/* unhandled pattern */".to_string(),
                preserve_trivia: false,
            }],
        }
    }

    fn layout_call_args(&mut self, args: &SListing<SCallItem>) -> Vec<LayoutElement> {
        let mut elements = Vec::new();
        elements.push(LayoutElement::Text {
            content: "(".to_string(),
            preserve_trivia: false,
        });

        match args {
            Listing::Inline {
                begin: _,
                items,
                newline: _,
                end: _,
            } => {
                let mut arg_elements = Vec::new();
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        arg_elements.push(LayoutElement::Text {
                            content: ",".to_string(),
                            preserve_trivia: false,
                        });
                        arg_elements.push(LayoutElement::Space { count: 1 });
                    }
                    arg_elements.extend(self.layout_call_item(&item.item));
                }

                elements.push(LayoutElement::Group {
                    elements: arg_elements,
                    break_mode: BreakMode::IfNeeded,
                });
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
                    elements.push(LayoutElement::Newline { count: 1 });
                    self.current_indent += 1;

                    for (i, item) in items.iter().enumerate() {
                        if i > 0 {
                            elements.push(LayoutElement::Text {
                                content: ",".to_string(),
                                preserve_trivia: false,
                            });
                            elements.push(LayoutElement::Newline { count: 1 });
                        }
                        elements.push(LayoutElement::Indent {
                            level: self.current_indent,
                        });
                        elements.extend(self.layout_call_item(&item.item));
                    }

                    self.current_indent -= 1;
                    elements.push(LayoutElement::Newline { count: 1 });
                    elements.push(LayoutElement::Indent {
                        level: self.current_indent,
                    });
                }
            }
            Listing::Open { items } => {
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        elements.push(LayoutElement::Text {
                            content: ",".to_string(),
                            preserve_trivia: false,
                        });
                        elements.push(LayoutElement::Space { count: 1 });
                    }
                    elements.extend(self.layout_call_item(&item.item));
                }
            }
        }

        elements.push(LayoutElement::Text {
            content: ")".to_string(),
            preserve_trivia: false,
        });
        elements
    }

    fn layout_call_item(&mut self, item: &SCallItem) -> Vec<LayoutElement> {
        match item {
            CallItem::Arg { expr } => self.layout_expression(expr),
            CallItem::Kwarg { name, eq: _, expr } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_token_with_trivia(name));
                elements.push(LayoutElement::Text {
                    content: "=".to_string(),
                    preserve_trivia: false,
                });
                elements.extend(self.layout_expression(expr));
                elements
            }
            _ => vec![LayoutElement::Text {
                content: "/* unhandled call item */".to_string(),
                preserve_trivia: false,
            }],
        }
    }

    fn layout_token_with_trivia(&self, token: &koatl_parser::lexer::SToken) -> Vec<LayoutElement> {
        let mut elements = Vec::new();

        // Add leading trivia
        elements.extend(self.layout_trivia(&token.leading_trivia));

        // Add the token content
        elements.push(LayoutElement::Text {
            content: self.token_to_string(token),
            preserve_trivia: true,
        });

        // Add trailing trivia
        elements.extend(self.layout_trivia(&token.trailing_trivia));

        elements
    }

    fn layout_trivia(&self, trivia: &[koatl_parser::lexer::Trivium]) -> Vec<LayoutElement> {
        use koatl_parser::lexer::TriviumType;
        let mut elements = Vec::new();

        for t in trivia {
            match t.typ {
                TriviumType::LineComment => {
                    elements.push(LayoutElement::Comment {
                        content: t.value.to_string(),
                        is_line_comment: true,
                    });
                }
                TriviumType::BlockComment => {
                    elements.push(LayoutElement::Comment {
                        content: t.value.to_string(),
                        is_line_comment: false,
                    });
                }
                TriviumType::Whitespace => {
                    // Generally let the formatter handle whitespace,
                    // but preserve significant whitespace in some contexts
                    if self.should_preserve_trivia_whitespace(t) {
                        elements.push(LayoutElement::Text {
                            content: t.value.to_string(),
                            preserve_trivia: true,
                        });
                    }
                }
                TriviumType::Newline => {
                    // Preserve multiple consecutive newlines as blank lines
                    let newline_count = t.value.matches('\n').count();
                    if newline_count > 1 {
                        elements.push(LayoutElement::Newline {
                            count: newline_count - 1,
                        });
                    }
                }
            }
        }

        elements
    }

    fn should_preserve_trivia_whitespace(&self, _trivia: &koatl_parser::lexer::Trivium) -> bool {
        // For now, let the formatter handle whitespace
        false
    }

    fn token_to_string(&self, token: &koatl_parser::lexer::SToken) -> String {
        use koatl_parser::lexer::Token;

        match &token.token {
            Token::Ident(s) => s.to_string(),
            Token::None => "None".to_string(),
            Token::Bool(b) => b.to_string(),
            Token::Str(s) => format!("\"{}\"", s),
            Token::Int(s) => s.to_string(),
            Token::IntBin(s) => format!("0b{}", s),
            Token::IntOct(s) => format!("0o{}", s),
            Token::IntHex(s) => format!("0x{}", s),
            Token::Float(s) => s.to_string(),
            Token::Kw(s) => s.to_string(),
            Token::Symbol(s) => s.to_string(),
            Token::FstrBegin(s) => s.to_string(),
            Token::FstrContinue(s) => s.to_string(),
            Token::Indent | Token::Dedent | Token::Eol => String::new(),
        }
    }

    fn binary_op_to_string(&self, op: BinaryOp) -> String {
        match op {
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
            BinaryOp::MatMul => "@",
            BinaryOp::Exp => "**",
            BinaryOp::FloorDiv => "//",
            BinaryOp::Mod => "%",
            BinaryOp::In => "in",
            BinaryOp::Nin => "not in",
            BinaryOp::Is => "is",
            BinaryOp::Nis => "is not",
            BinaryOp::BitAnd => "&",
            BinaryOp::BitOr => "|",
            BinaryOp::BitXor => "^",
            BinaryOp::LShift => "<<",
            BinaryOp::RShift => ">>",
            BinaryOp::Coalesce => "??",
            BinaryOp::Pipe => "|",
        }
        .to_string()
    }

    fn unary_op_to_string(&self, op: UnaryOp) -> String {
        match op {
            UnaryOp::Neg => "-",
            UnaryOp::Pos => "+",
            UnaryOp::Not => "not ",
            UnaryOp::Inv => "~",
            UnaryOp::Bind => "@",
        }
        .to_string()
    }

    // Additional layout methods for complex constructs

    fn layout_colon_block(&mut self, block: &SColonBlock) -> Vec<LayoutElement> {
        let mut elements = Vec::new();

        match block {
            ColonBlock::Block {
                colon,
                indent: _,
                body,
                dedent: _,
            } => {
                elements.extend(self.layout_token_with_trivia(colon));
                elements.push(LayoutElement::Newline { count: 1 });
                self.current_indent += 1;

                for (i, stmt) in body.value.iter().enumerate() {
                    if i > 0 {
                        elements.push(LayoutElement::Newline { count: 1 });
                    }
                    elements.extend(self.layout_statement(stmt));
                }

                self.current_indent -= 1;
            }
            ColonBlock::Inline { colon, stmt } => {
                if let Some(colon_token) = colon {
                    elements.extend(self.layout_token_with_trivia(colon_token));
                    elements.push(LayoutElement::Space { count: 1 });
                }
                elements.extend(self.layout_statement(stmt));
            }
        }

        elements
    }

    fn layout_arrow_block(&mut self, block: &ArrowBlock<STree>) -> Vec<LayoutElement> {
        let mut elements = Vec::new();

        match block {
            ArrowBlock::Block {
                arrow,
                indent: _,
                body,
                dedent: _,
            } => {
                elements.extend(self.layout_token_with_trivia(arrow));
                elements.push(LayoutElement::Newline { count: 1 });
                self.current_indent += 1;

                for (i, stmt) in body.value.iter().enumerate() {
                    if i > 0 {
                        elements.push(LayoutElement::Newline { count: 1 });
                    }
                    elements.extend(self.layout_statement(stmt));
                }

                self.current_indent -= 1;
            }
            ArrowBlock::Inline { arrow, stmt } => {
                elements.extend(self.layout_token_with_trivia(arrow));
                elements.push(LayoutElement::Space { count: 1 });
                elements.extend(self.layout_statement(stmt));
            }
        }

        elements
    }

    fn layout_import_tree(&self, _tree: &ImportTree<STree>) -> Vec<LayoutElement> {
        // Simplified import handling for now
        vec![LayoutElement::Text {
            content: "/* import tree */".to_string(),
            preserve_trivia: false,
        }]
    }

    fn layout_arg_def_listing(&mut self, args: &SListing<SArgDefItem>) -> Vec<LayoutElement> {
        let mut elements = Vec::new();
        elements.push(LayoutElement::Text {
            content: "(".to_string(),
            preserve_trivia: false,
        });

        match args {
            Listing::Inline {
                begin: _,
                items,
                newline: _,
                end: _,
            } => {
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        elements.push(LayoutElement::Text {
                            content: ",".to_string(),
                            preserve_trivia: false,
                        });
                        elements.push(LayoutElement::Space { count: 1 });
                    }
                    elements.extend(self.layout_arg_def_item(&item.item));
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
                    elements.push(LayoutElement::Newline { count: 1 });
                    self.current_indent += 1;

                    for (i, item) in items.iter().enumerate() {
                        if i > 0 {
                            elements.push(LayoutElement::Text {
                                content: ",".to_string(),
                                preserve_trivia: false,
                            });
                            elements.push(LayoutElement::Newline { count: 1 });
                        }
                        elements.push(LayoutElement::Indent {
                            level: self.current_indent,
                        });
                        elements.extend(self.layout_arg_def_item(&item.item));
                    }

                    self.current_indent -= 1;
                    elements.push(LayoutElement::Newline { count: 1 });
                    elements.push(LayoutElement::Indent {
                        level: self.current_indent,
                    });
                }
            }
            Listing::Open { items } => {
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        elements.push(LayoutElement::Text {
                            content: ",".to_string(),
                            preserve_trivia: false,
                        });
                        elements.push(LayoutElement::Space { count: 1 });
                    }
                    elements.extend(self.layout_arg_def_item(&item.item));
                }
            }
        }

        elements.push(LayoutElement::Text {
            content: ")".to_string(),
            preserve_trivia: false,
        });
        elements
    }

    fn layout_arg_def_item(&mut self, item: &SArgDefItem) -> Vec<LayoutElement> {
        match item {
            ArgDefItem::Arg { pattern, default } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_pattern(pattern));
                if let Some((eq_token, default_expr)) = default {
                    elements.extend(self.layout_token_with_trivia(eq_token));
                    elements.extend(self.layout_expression(default_expr));
                }
                elements
            }
            ArgDefItem::ArgSpread { star, name } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_token_with_trivia(star));
                elements.extend(self.layout_token_with_trivia(name));
                elements
            }
            ArgDefItem::KwargSpread { stars, name } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_token_with_trivia(stars));
                elements.extend(self.layout_token_with_trivia(name));
                elements
            }
        }
    }

    fn layout_list_listing(&mut self, listing: &SListing<SListItem>) -> Vec<LayoutElement> {
        let mut elements = Vec::new();
        elements.push(LayoutElement::Text {
            content: "[".to_string(),
            preserve_trivia: false,
        });

        match listing {
            Listing::Inline {
                begin: _,
                items,
                newline: _,
                end: _,
            } => {
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        elements.push(LayoutElement::Text {
                            content: ",".to_string(),
                            preserve_trivia: false,
                        });
                        elements.push(LayoutElement::Space { count: 1 });
                    }
                    elements.extend(self.layout_list_item(&item.item));
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
                    elements.push(LayoutElement::Newline { count: 1 });
                    self.current_indent += 1;

                    for (i, item) in items.iter().enumerate() {
                        if i > 0 {
                            elements.push(LayoutElement::Text {
                                content: ",".to_string(),
                                preserve_trivia: false,
                            });
                            elements.push(LayoutElement::Newline { count: 1 });
                        }
                        elements.push(LayoutElement::Indent {
                            level: self.current_indent,
                        });
                        elements.extend(self.layout_list_item(&item.item));
                    }

                    self.current_indent -= 1;
                    elements.push(LayoutElement::Newline { count: 1 });
                    elements.push(LayoutElement::Indent {
                        level: self.current_indent,
                    });
                }
            }
            Listing::Open { items } => {
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        elements.push(LayoutElement::Text {
                            content: ",".to_string(),
                            preserve_trivia: false,
                        });
                        elements.push(LayoutElement::Space { count: 1 });
                    }
                    elements.extend(self.layout_list_item(&item.item));
                }
            }
        }

        elements.push(LayoutElement::Text {
            content: "]".to_string(),
            preserve_trivia: false,
        });
        elements
    }

    fn layout_list_item(&mut self, item: &SListItem) -> Vec<LayoutElement> {
        match item {
            ListItem::Item { expr } => self.layout_expression(expr),
            ListItem::Spread { star, expr } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_token_with_trivia(star));
                elements.extend(self.layout_expression(expr));
                elements
            }
        }
    }

    fn layout_tuple_kind(&mut self, kind: &TupleKind<STree>) -> Vec<LayoutElement> {
        match kind {
            TupleKind::Unit(lparen, rparen) => {
                let mut elements = Vec::new();
                elements.extend(self.layout_token_with_trivia(lparen));
                elements.extend(self.layout_token_with_trivia(rparen));
                elements
            }
            TupleKind::Listing(items) => {
                let mut elements = Vec::new();
                elements.push(LayoutElement::Text {
                    content: "(".to_string(),
                    preserve_trivia: false,
                });

                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        elements.push(LayoutElement::Text {
                            content: ",".to_string(),
                            preserve_trivia: false,
                        });
                        elements.push(LayoutElement::Space { count: 1 });
                    }
                    elements.extend(self.layout_list_item(&item.item));
                }

                elements.push(LayoutElement::Text {
                    content: ")".to_string(),
                    preserve_trivia: false,
                });
                elements
            }
        }
    }

    fn layout_mapping_listing(&mut self, listing: &SListing<SMappingItem>) -> Vec<LayoutElement> {
        let mut elements = Vec::new();
        elements.push(LayoutElement::Text {
            content: "{".to_string(),
            preserve_trivia: false,
        });

        match listing {
            Listing::Inline {
                begin: _,
                items,
                newline: _,
                end: _,
            } => {
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        elements.push(LayoutElement::Text {
                            content: ",".to_string(),
                            preserve_trivia: false,
                        });
                        elements.push(LayoutElement::Space { count: 1 });
                    }
                    elements.extend(self.layout_mapping_item(&item.item));
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
                    elements.push(LayoutElement::Newline { count: 1 });
                    self.current_indent += 1;

                    for (i, item) in items.iter().enumerate() {
                        if i > 0 {
                            elements.push(LayoutElement::Text {
                                content: ",".to_string(),
                                preserve_trivia: false,
                            });
                            elements.push(LayoutElement::Newline { count: 1 });
                        }
                        elements.push(LayoutElement::Indent {
                            level: self.current_indent,
                        });
                        elements.extend(self.layout_mapping_item(&item.item));
                    }

                    self.current_indent -= 1;
                    elements.push(LayoutElement::Newline { count: 1 });
                    elements.push(LayoutElement::Indent {
                        level: self.current_indent,
                    });
                }
            }
            Listing::Open { items } => {
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        elements.push(LayoutElement::Text {
                            content: ",".to_string(),
                            preserve_trivia: false,
                        });
                        elements.push(LayoutElement::Space { count: 1 });
                    }
                    elements.extend(self.layout_mapping_item(&item.item));
                }
            }
        }

        elements.push(LayoutElement::Text {
            content: "}".to_string(),
            preserve_trivia: false,
        });
        elements
    }

    fn layout_mapping_item(&mut self, item: &SMappingItem) -> Vec<LayoutElement> {
        match item {
            MappingItem::Ident { ident } => self.layout_token_with_trivia(ident),
            MappingItem::Item { key, colon, value } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_mapping_key(key));
                elements.extend(self.layout_token_with_trivia(colon));
                elements.push(LayoutElement::Space { count: 1 });
                elements.extend(self.layout_expression(value));
                elements
            }
            MappingItem::Spread { stars, expr } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_token_with_trivia(stars));
                elements.extend(self.layout_expression(expr));
                elements
            }
        }
    }

    fn layout_mapping_key(&mut self, key: &MappingKey<STree>) -> Vec<LayoutElement> {
        match key {
            MappingKey::Ident { token } => self.layout_token_with_trivia(token),
            MappingKey::Unit { lparen, rparen } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_token_with_trivia(lparen));
                elements.extend(self.layout_token_with_trivia(rparen));
                elements
            }
            MappingKey::Literal { token } => self.layout_token_with_trivia(token),
            MappingKey::Expr {
                lparen,
                key,
                rparen,
            } => {
                let mut elements = Vec::new();
                elements.extend(self.layout_token_with_trivia(lparen));
                elements.extend(self.layout_expression(key));
                elements.extend(self.layout_token_with_trivia(rparen));
                elements
            }
            _ => vec![LayoutElement::Text {
                content: "/* unhandled mapping key */".to_string(),
                preserve_trivia: false,
            }],
        }
    }

    fn layout_subscript_listing(&mut self, listing: &SListing<SListItem>) -> Vec<LayoutElement> {
        // Subscripts use the same structure as lists but without brackets
        match listing {
            Listing::Inline {
                begin: _,
                items,
                newline: _,
                end: _,
            } => {
                let mut elements = Vec::new();
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        elements.push(LayoutElement::Text {
                            content: ",".to_string(),
                            preserve_trivia: false,
                        });
                        elements.push(LayoutElement::Space { count: 1 });
                    }
                    elements.extend(self.layout_list_item(&item.item));
                }
                elements
            }
            Listing::Block {
                begin: _,
                indent: _,
                items,
                dedent: _,
                newline: _,
                end: _,
            } => {
                let mut elements = Vec::new();
                if !items.is_empty() {
                    elements.push(LayoutElement::Newline { count: 1 });
                    self.current_indent += 1;

                    for (i, item) in items.iter().enumerate() {
                        if i > 0 {
                            elements.push(LayoutElement::Text {
                                content: ",".to_string(),
                                preserve_trivia: false,
                            });
                            elements.push(LayoutElement::Newline { count: 1 });
                        }
                        elements.push(LayoutElement::Indent {
                            level: self.current_indent,
                        });
                        elements.extend(self.layout_list_item(&item.item));
                    }

                    self.current_indent -= 1;
                    elements.push(LayoutElement::Newline { count: 1 });
                    elements.push(LayoutElement::Indent {
                        level: self.current_indent,
                    });
                }
                elements
            }
            Listing::Open { items } => {
                let mut elements = Vec::new();
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        elements.push(LayoutElement::Text {
                            content: ",".to_string(),
                            preserve_trivia: false,
                        });
                        elements.push(LayoutElement::Space { count: 1 });
                    }
                    elements.extend(self.layout_list_item(&item.item));
                }
                elements
            }
        }
    }
}

/// Phase 2: Generate output from layout decisions
struct OutputGenerator<'a> {
    config: &'a Config,
    output: String,
    current_line_length: usize,
}

impl<'a> OutputGenerator<'a> {
    fn new(config: &'a Config) -> Self {
        Self {
            config,
            output: String::new(),
            current_line_length: 0,
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
            LayoutElement::Text {
                content,
                preserve_trivia: _,
            } => {
                self.output.push_str(content);
                self.current_line_length += content.len();
            }
            LayoutElement::Newline { count } => {
                for _ in 0..*count {
                    self.output.push('\n');
                }
                self.current_line_length = 0;
            }
            LayoutElement::Indent { level } => {
                let indent_size = level * self.config.indent_width;
                for _ in 0..indent_size {
                    self.output.push(' ');
                }
                self.current_line_length += indent_size;
            }
            LayoutElement::Space { count } => {
                for _ in 0..*count {
                    self.output.push(' ');
                }
                self.current_line_length += count;
            }
            LayoutElement::Comment {
                content,
                is_line_comment,
            } => {
                // Add appropriate spacing before comments
                if !self.output.ends_with(' ') && !self.output.ends_with('\n') {
                    self.output.push(' ');
                    self.current_line_length += 1;
                }

                self.output.push_str(content);
                self.current_line_length += content.len();

                // Line comments typically end with newline
                if *is_line_comment && !content.ends_with('\n') {
                    self.output.push('\n');
                    self.current_line_length = 0;
                }
            }
            LayoutElement::Group {
                elements,
                break_mode,
            } => {
                // For now, render groups simply based on break mode
                // Later we can implement smart line breaking based on length
                match break_mode {
                    BreakMode::Never => {
                        for el in elements {
                            self.render_element(el);
                        }
                    }
                    BreakMode::IfNeeded => {
                        // Try to fit on one line first, break if needed
                        // For now, just render normally
                        for el in elements {
                            self.render_element(el);
                        }
                    }
                    BreakMode::Always => {
                        // Force line breaks between elements
                        for (i, el) in elements.iter().enumerate() {
                            if i > 0 {
                                self.output.push('\n');
                                self.current_line_length = 0;
                            }
                            self.render_element(el);
                        }
                    }
                }
            }
        }
    }
}
