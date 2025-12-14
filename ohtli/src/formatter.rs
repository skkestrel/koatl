use std::fmt::Display;

use anyhow::Result;
use koatl_parser::lexer::{tokenize, SToken, TrivialToken, TrivialTokenType};
use koatl_parser::parse_tokens;
use koatl_parser::{cst::*, Token};

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

        // println!("{:#?} {:#?}", tokens, lex_errors);

        let (cst, parse_errors) = parse_tokens(source, &tokens);

        let Some(cst) = cst else {
            anyhow::bail!("Parsing errors: {:?}", parse_errors);
        };

        // println!("{:#?} {:#?}", cst, parse_errors);

        let mut lines = Vec::new();

        for stmt in cst.value.iter() {
            lines.extend(stmt_to_lines(stmt));
        }

        // println!("Input Layout:\n{}", format_lines(&lines));

        let processed_layout = LayoutCalculator::new(&self.config).do_layout(lines);

        // println!("Layout:\n{}", format_lines(&processed_layout));

        let mut output_generator = LayoutWriter::new(&self.config);
        Ok(output_generator.write(&processed_layout))
    }
}

pub type Elements = Vec<Element>;
pub type Line = Elements;

#[derive(Debug, Clone)]
pub struct Element {
    pub attach_before: bool,
    pub attach_after: bool,
    pub data: ElementData,
}

#[derive(Debug, Clone)]
pub enum ElementData {
    Atom {
        content: String,
        is_comment: bool,
    },
    LineBreak,

    Listing {
        lines: Vec<Line>,
        inline: bool,
    },

    Parens {
        begin: Elements,
        elements: Elements,
        end: Elements,
    },

    Block {
        lines: Vec<Line>,
        inline: bool,
    },
}

impl Display for ElementData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ElementData::Atom {
                content,
                is_comment,
            } => {
                if *is_comment {
                    write!(f, "Comment({})", content)
                } else {
                    write!(f, "{:?}", content)
                }
            }
            ElementData::LineBreak => write!(f, "LineBreak"),
            ElementData::Listing { lines, inline } => {
                write!(
                    f,
                    "Listing({}, inline={})",
                    lines
                        .iter()
                        .map(|x| format_elements(x))
                        .collect::<Vec<_>>()
                        .join(", "),
                    inline
                )
            }
            ElementData::Parens {
                begin,
                elements,
                end,
            } => {
                write!(
                    f,
                    "Parens({}, {}, {})",
                    format_elements(begin),
                    format_elements(elements),
                    format_elements(end)
                )
            }
            ElementData::Block { lines, inline } => {
                write!(
                    f,
                    "Block({}, inline={})",
                    lines
                        .iter()
                        .map(|x| format_elements(x))
                        .collect::<Vec<_>>()
                        .join(", "),
                    inline
                )
            }
        }
    }
}

pub fn format_elements(elements: &[Element]) -> String {
    let element_strs: Vec<String> = elements
        .iter()
        .map(|e| {
            let content = format!("{}", e.data);
            if e.attach_before && e.attach_after {
                format!("+{}+", content)
            } else if e.attach_before {
                format!("+{}", content)
            } else if e.attach_after {
                format!("{}+", content)
            } else {
                content
            }
        })
        .collect();
    format!("[{}]", element_strs.join(" "))
}

#[allow(dead_code)]
pub fn format_lines(lines: &[Line]) -> String {
    let line_strs: Vec<String> = lines.iter().map(|line| format_elements(line)).collect();
    format!("{}", line_strs.join("\n"))
}

macro_rules! line {
    ($($v:expr),+) => {
        {
            let mut v: Vec<Element> = vec![];

            $(
                v.extend($v.to_elements());
            )+

            v
        }
    };
}

trait ToElements {
    fn to_elements(&self) -> Vec<Element>;
}

trait IntoElements {
    fn to_elements(self) -> Vec<Element>;
}

pub fn split_blocks_into_separate_lines(tokens: Elements) -> Vec<Line> {
    let mut cur_line = vec![];
    let mut lines = vec![];

    for t in tokens {
        match &t.data {
            ElementData::Block { inline: false, .. } => {
                cur_line.push(t);
                lines.push(std::mem::take(&mut cur_line));
                continue;
            }
            _ => {}
        }

        cur_line.push(t)
    }

    if !cur_line.is_empty() {
        lines.push(cur_line);
    }

    lines
}

pub fn stmt_to_elements(stmt: &SStmt) -> Elements {
    let tokens = match &stmt.value {
        Stmt::Expr { expr } => line!(expr),
        Stmt::PatternAssign {
            modifier,
            lhs,
            eq,
            rhs,
        } => {
            line!(modifier, lhs, eq, rhs)
        }
        Stmt::Assign {
            lhs,
            eq,
            rhs,
            op: _,
        } => {
            line!(lhs, eq, rhs)
        }
        Stmt::Decl { modifier, names } => {
            line!(
                modifier,
                names
                    .iter()
                    .flat_map(|(x, comma)| line!(x, comma.map(attached_token)))
                    .collect::<Vec<_>>()
            )
        }
        Stmt::While {
            while_kw,
            cond,
            body,
        } => {
            line!(while_kw, cond, body)
        }
        Stmt::For {
            for_kw,
            pattern,
            in_kw,
            iter,
            body,
        } => {
            line!(for_kw, pattern, in_kw, iter, body)
        }
        Stmt::Import {
            export,
            import,
            tree,
        } => {
            line!(export, import, tree)
        }
        Stmt::Break { break_kw } => {
            line!(break_kw)
        }
        Stmt::Continue { continue_kw } => {
            line!(continue_kw)
        }
        Stmt::Return { return_kw, expr } => {
            line!(return_kw, expr)
        }
        Stmt::Raise { raise_kw, expr } => {
            line!(raise_kw, expr)
        }
        Stmt::Error { raw } => {
            // Tokenize the raw error text to preserve trivia (comments, newlines, whitespace)
            let (tokens_opt, _) = tokenize(raw, true);

            if let Some(tokens) = tokens_opt {
                let mut elements = Vec::new();

                // Extract trivia and tokens from the error, preserving all formatting
                for token in &tokens.0 {
                    // Add leading trivia (comments, newlines, whitespace)
                    for trivium in &token.leading_trivia {
                        match trivium.typ {
                            TrivialTokenType::LineComment | TrivialTokenType::BlockComment => {
                                elements.push(Element::comment(trivium.value.to_string()));
                            }
                            TrivialTokenType::Newline => {
                                elements.push(Element::line_break());
                            }
                            TrivialTokenType::Whitespace => {
                                // Preserve whitespace for error statements
                                elements.push(Element::attached_both(trivium.value.to_string()));
                            }
                        }
                    }

                    // Add the token itself if it's not whitespace/structural
                    if !matches!(token.token, Token::Indent | Token::Dedent | Token::Eol) {
                        let token_text = token_to_text(token);
                        elements.push(Element::attached_both(token_text));
                    }

                    // Add trailing trivia (whitespace, comments)
                    for trivium in &token.trailing_trivia {
                        match trivium.typ {
                            TrivialTokenType::LineComment | TrivialTokenType::BlockComment => {
                                elements.push(Element::comment(trivium.value.to_string()));
                            }
                            TrivialTokenType::Newline => {
                                elements.push(Element::line_break());
                            }
                            TrivialTokenType::Whitespace => {
                                // Preserve whitespace for error statements
                                elements.push(Element::attached_both(trivium.value.to_string()));
                            }
                        }
                    }
                }

                // Add a line break at the end
                elements.push(Element::line_break());
                elements
            } else {
                // Fallback if tokenization fails
                let clean_raw = raw.trim_end().to_string();
                vec![Element::attached_both(clean_raw), Element::line_break()]
            }
        }
    };

    tokens
}

/**
 * Some statements become multiple lines:
 * if True:
 *  1
 * else:
 *  2
 *
 * is a single statement but is best represented as two independent lines,
 * one for each branch.
 */
pub fn stmt_to_lines(stmt: &SStmt) -> Vec<Line> {
    let elements = stmt_to_elements(stmt);
    split_blocks_into_separate_lines(elements)
}

impl<T> IntoElements for Option<T>
where
    T: IntoElements,
{
    fn to_elements(self) -> Vec<Element> {
        if let Some(value) = self {
            value.to_elements()
        } else {
            Vec::new()
        }
    }
}

impl<T> ToElements for Option<T>
where
    T: ToElements,
{
    fn to_elements(&self) -> Vec<Element> {
        if let Some(value) = self {
            value.to_elements()
        } else {
            Vec::new()
        }
    }
}

impl<T> ToElements for Box<T>
where
    T: ToElements,
{
    fn to_elements(&self) -> Vec<Element> {
        self.as_ref().to_elements()
    }
}

impl<T> ToElements for Vec<T>
where
    T: ToElements,
{
    fn to_elements(&self) -> Vec<Element> {
        let mut elements = Vec::new();
        for item in self {
            elements.extend(item.to_elements());
        }
        elements
    }
}

impl<T> IntoElements for Vec<T>
where
    T: IntoElements,
{
    fn to_elements(self) -> Vec<Element> {
        let mut elements = Vec::new();
        for item in self {
            elements.extend(item.to_elements());
        }
        elements
    }
}

impl IntoElements for Element {
    fn to_elements(self) -> Vec<Element> {
        vec![self]
    }
}

impl<T> ToElements for Spanned<T>
where
    T: ToElements,
{
    fn to_elements(&self) -> Vec<Element> {
        self.value.to_elements()
    }
}

impl Element {
    pub fn atom(content: String) -> Self {
        Element {
            attach_before: false,
            attach_after: false,
            data: ElementData::Atom {
                content,
                is_comment: false,
            },
        }
    }

    pub fn line_break() -> Self {
        Element {
            attach_before: false,
            attach_after: false,
            data: ElementData::LineBreak,
        }
    }

    pub fn attached_after(content: String) -> Self {
        let mut a = Self::atom(content);
        a.attach_after = true;
        a
    }

    pub fn attached_before(content: String) -> Self {
        let mut a = Self::atom(content);
        a.attach_before = true;
        a
    }

    pub fn attached_both(content: String) -> Self {
        let mut a = Self::atom(content);
        a.attach_before = true;
        a.attach_after = true;
        a
    }

    pub fn comment(content: String) -> Self {
        Element {
            attach_before: false,
            attach_after: false,
            data: ElementData::Atom {
                content,
                is_comment: true,
            },
        }
    }

    pub fn listing(begin: Line, lines: Vec<Line>, end: Line, inline: bool, attached: bool) -> Self {
        Element {
            attach_before: attached,
            attach_after: false,
            data: ElementData::Parens {
                begin,
                elements: vec![Element {
                    attach_before: false,
                    attach_after: false,
                    data: ElementData::Listing { lines, inline },
                }],
                end,
            },
        }
    }

    pub fn parens(begin: Elements, elements: Vec<Element>, end: Elements) -> Self {
        Element {
            attach_before: false,
            attach_after: false,
            data: ElementData::Parens {
                begin,
                elements,
                end,
            },
        }
    }

    pub fn block(lines: Vec<Line>, inline: bool) -> Self {
        Element {
            attach_before: false,
            attach_after: false,
            data: ElementData::Block { lines, inline },
        }
    }
}

impl ToElements for ImportTree<STree<'_, '_>> {
    fn to_elements(&self) -> Vec<Element> {
        line!(
            self.dots
                .value
                .iter()
                .flat_map(|(dot, _count)| attached_next_token(dot))
                .collect::<Vec<_>>(),
            self.trunk
                .iter()
                .flat_map(|(ident, dot)| {
                    line!(
                        ident,
                        special_token_to_elements(dot, TokenContext::DoublyAttached)
                    )
                })
                .collect::<Vec<_>>(),
            self.leaf
        )
    }
}

impl ToElements for ImportLeaf<STree<'_, '_>> {
    fn to_elements(&self) -> Vec<Element> {
        match self {
            ImportLeaf::Multi(listing) => {
                line!(listing)
            }
            ImportLeaf::Single { name, alias } => {
                line!(name, alias.map(|(x, y)| line!(x, y)))
            }
            ImportLeaf::This { dot, alias } => {
                line!(dot, alias.map(|(x, y)| line!(x, y)))
            }
            ImportLeaf::Star { star } => line!(star),
        }
    }
}

impl ToElements for SExpr<'_, '_> {
    fn to_elements(&self) -> Vec<Element> {
        match &self.value {
            Expr::Literal { token } => token.to_elements(),
            Expr::Ident { token } => token.to_elements(),
            Expr::Binary {
                lhs,
                not,
                op,
                rhs,
                op_kind: _,
            } => {
                line!(lhs, not, op, rhs)
            }
            Expr::Unary {
                op,
                expr,
                op_kind: _,
            } => {
                line!(
                    special_token_to_elements(op, TokenContext::UnaryOperator),
                    expr
                )
            }
            Expr::List { listing } => line!(listing),
            Expr::Mapping { listing } => line!(listing),
            Expr::MethodCall {
                expr,
                question,
                dot,
                method,
                args,
            } => {
                line!(
                    expr,
                    question.map(attached_token),
                    attached_token(dot),
                    attached_token(method),
                    attached_listing(args)
                )
            }
            Expr::RawAttribute {
                expr,
                question,
                double_colon,
                attr,
            } => {
                line!(
                    expr,
                    question.map(attached_token),
                    attached_token(double_colon),
                    attached_token(attr)
                )
            }
            Expr::ScopedAttribute {
                expr,
                question,
                dot,
                lparen,
                rhs,
                rparen,
            } => {
                let mut parens = Element::parens(
                    lparen.to_elements(),
                    rhs.to_elements(),
                    rparen.to_elements(),
                );
                parens.attach_before = true;
                line!(
                    expr,
                    question.map(attached_token),
                    attached_token(dot),
                    parens
                )
            }
            Expr::Attribute {
                expr,
                dot,
                question,
                attr,
            } => {
                line!(
                    expr,
                    question.map(attached_token),
                    attached_token(dot),
                    attached_token(attr)
                )
            }
            Expr::MaybeAttribute {
                expr,
                question,
                dot,
                question2,
                attr,
            } => {
                line!(
                    expr,
                    question.map(attached_token),
                    attached_token(dot),
                    attached_token(question2),
                    attached_token(attr)
                )
            }
            Expr::Subscript {
                expr,
                question,
                indices,
            } => {
                line!(
                    expr,
                    question.map(attached_token),
                    attached_listing(indices)
                )
            }
            Expr::Call {
                expr,
                question,
                args,
            } => {
                line!(expr, question.map(attached_token), attached_listing(args))
            }
            Expr::If {
                cond,
                then_kw,
                body,
                else_clause,
            } => line!(
                cond,
                then_kw,
                body,
                else_clause.as_ref().map(|(x, y)| line!(x, y))
            ),
            Expr::Fn { arg, body } => line!(arg, body),
            Expr::ParenthesizedFn { args, body } => line!(args, body),
            Expr::Slice {
                start,
                dots,
                stop,
                step_dots,
                step,
            } => {
                // For the first dots: doubly-attached only if start is present; otherwise attach to next
                let first_ctx = if start.is_some() {
                    TokenContext::DoublyAttached
                } else {
                    TokenContext::AttachedNext
                };

                // For step dots: doubly-attached only if step is present; otherwise attach to previous
                let step_ctx = if step.is_some() {
                    TokenContext::DoublyAttached
                } else {
                    TokenContext::Attached
                };

                line!(
                    start,
                    special_token_to_elements(dots, first_ctx),
                    stop,
                    step_dots.map(|x| special_token_to_elements(x, step_ctx)),
                    step
                )
            }
            Expr::Await { await_kw, expr, .. } => line![await_kw, expr],
            Expr::Memo {
                async_kw,
                memo_kw,
                body,
            } => {
                line!(async_kw, memo_kw, body)
            }
            Expr::Placeholder { token } => line!(token),
            Expr::Parenthesized {
                lparen,
                expr,
                rparen,
            } => line!(lparen, expr, rparen),
            Expr::Match {
                scrutinee,
                match_kw,
                colon,
                indent,
                cases,
                dedent,
            } => {
                line!(
                    scrutinee,
                    match_kw,
                    attached_token(colon),
                    indent,
                    Element::block(cases.iter().map(|case| line!(case)).collect(), false),
                    dedent
                )
            }
            Expr::ClassicMatch {
                match_kw,
                scrutinee,
                colon,
                indent,
                cases,
                dedent,
            } => {
                line!(
                    match_kw,
                    scrutinee,
                    attached_token(colon),
                    indent,
                    Element::block(cases.iter().map(|case| line!(case)).collect(), false),
                    dedent
                )
            }
            Expr::ClassicIf {
                if_kw,
                cond,
                body,
                else_clause,
            } => {
                if let Some((else_kw, else_body)) = else_clause {
                    line![if_kw, cond, body.clone(), else_kw, else_body.clone()]
                } else {
                    line![if_kw, cond, body.clone()]
                }
            }
            Expr::Yield {
                yield_kw,
                from_kw,
                expr,
            } => {
                if let Some(from_kw) = from_kw {
                    line![yield_kw, from_kw, expr]
                } else {
                    line![yield_kw, expr]
                }
            }
            Expr::Matches {
                lhs,
                not_kw,
                matches_kw,
                pattern,
            } => {
                line!(lhs, not_kw, matches_kw, pattern)
            }
            Expr::Class {
                class_kw,
                args,
                body,
            } => {
                line!(class_kw, args.as_ref().map(|x| attached_listing(x)), body)
            }
            Expr::With {
                with_kw,
                pattern,
                eq,
                value,
                body,
            } => {
                line!(with_kw, pattern, eq, value, body)
            }
            Expr::Try {
                try_kw,
                body,
                cases,
                finally,
            } => {
                let cases = cases
                    .iter()
                    .map(|case| {
                        line!(
                            case.except,
                            case.pattern,
                            case.guard.as_ref().map(|(if_, expr)| line!(if_, expr)),
                            case.body
                        )
                    })
                    .collect::<Vec<_>>();

                let finally = finally.as_ref().map(|(kw, block)| line!(kw, block));

                line!(try_kw, body, cases, finally)
            }
            Expr::Checked {
                check_kw,
                expr,
                except_kw,
                pattern,
            } => {
                line!(check_kw, expr, except_kw, pattern)
            }
            Expr::Decorated {
                expr,
                op,
                decorator,
            } => {
                line!(expr, attached_token(op), decorator)
            }
            Expr::Fstr {
                begin,
                head,
                parts,
                end,
            } => line!(
                attached_next_token(begin),
                attached_next_token(head),
                parts
                    .iter()
                    .map(|(a, b)| line!(
                        a,
                        special_token_to_elements(b, TokenContext::DoublyAttached)
                    ))
                    .collect::<Vec<_>>(),
                attached_token(end)
            ),
            Expr::Tuple { kind } => match kind {
                TupleKind::Unit(lparen, rparen) => {
                    line!(lparen, attached_token(rparen))
                }
                TupleKind::Listing(items) => {
                    line!(items)
                }
            },
            Expr::ParenthesizedBlock {
                lparen,
                indent,
                body,
                dedent,
                rparen,
            } => {
                let body_lines = body
                    .value
                    .iter()
                    .flat_map(|item| stmt_to_lines(item))
                    .collect();
                let body = Element::block(body_lines, false);

                Element::parens(
                    lparen.to_elements(),
                    line!(indent, body, dedent),
                    rparen.to_elements(),
                )
                .to_elements()
            }
        }
    }
}

impl<T> ToElements for ListingItem<T, STree<'_, '_>>
where
    T: ToElements,
{
    fn to_elements(&self) -> Vec<Element> {
        line!(self.item, self.separator.map(attached_token), self.newline)
    }
}

impl ToElements for ArgDefItem<STree<'_, '_>> {
    fn to_elements(&self) -> Vec<Element> {
        match self {
            ArgDefItem::Arg { pattern, default } => {
                line!(
                    pattern,
                    default.as_ref().map(|(eq, expr)| line!(
                        special_token_to_elements(eq, TokenContext::DoublyAttached),
                        expr
                    ))
                )
            }
            ArgDefItem::ArgSpread { star, name } => line!(attached_next_token(star), name),
            ArgDefItem::KwargSpread { stars, name } => line!(attached_next_token(stars), name),
            ArgDefItem::PosOnlyMarker { slash } => line!(attached_next_token(slash)),
            ArgDefItem::KwOnlyMarker { star } => line!(attached_next_token(star)),
        }
    }
}

impl ToElements for SListItem<'_, '_> {
    fn to_elements(&self) -> Vec<Element> {
        match self {
            ListItem::Item { expr } => line!(expr),
            ListItem::Spread { star, expr } => line!(attached_next_token(star), expr),
        }
    }
}

impl ToElements for SCallItem<'_, '_> {
    fn to_elements(&self) -> Vec<Element> {
        match self {
            CallItem::Arg { expr } => line!(expr),
            CallItem::Kwarg { name, expr, eq } => line!(
                name,
                special_token_to_elements(eq, TokenContext::DoublyAttached),
                expr
            ),
            CallItem::ArgSpread { expr, star } => line!(attached_next_token(star), expr),
            CallItem::KwargSpread { expr, stars } => line!(attached_next_token(stars), expr),
        }
    }
}

impl ToElements for PatternSequenceItem<STree<'_, '_>> {
    fn to_elements(&self) -> Vec<Element> {
        match self {
            PatternSequenceItem::Item { pattern } => line!(pattern),
            PatternSequenceItem::Spread { star, name } => line!(attached_next_token(star), name),
        }
    }
}

impl ToElements for PatternMappingItem<STree<'_, '_>> {
    fn to_elements(&self) -> Vec<Element> {
        match self {
            PatternMappingItem::Item {
                key,
                colon,
                pattern,
            } => line!(key, colon, pattern),
            PatternMappingItem::Ident { name } => line!(name),
            PatternMappingItem::Spread { stars, name } => line!(attached_next_token(stars), name),
        }
    }
}

impl ToElements for PatternClassItem<STree<'_, '_>> {
    fn to_elements(&self) -> Vec<Element> {
        match self {
            PatternClassItem::Item { pattern } => line!(pattern),
            PatternClassItem::Kw { name, eq, pattern } => line!(
                name,
                special_token_to_elements(eq, TokenContext::DoublyAttached),
                pattern
            ),
        }
    }
}

impl ToElements for PatternMappingKey<STree<'_, '_>> {
    fn to_elements(&self) -> Vec<Element> {
        match self {
            PatternMappingKey::Ident { token } => line!(token),
            PatternMappingKey::Unit { lparen, rparen } => line!(lparen, attached_token(rparen)),
            PatternMappingKey::Literal { token } => line!(token),
            PatternMappingKey::Expr {
                lparen,
                key,
                rparen,
            } => line!(Element::parens(
                lparen.to_elements(),
                key.to_elements(),
                rparen.to_elements()
            )),
        }
    }
}

impl ToElements for SMappingItem<'_, '_> {
    fn to_elements(&self) -> Vec<Element> {
        match self {
            MappingItem::Item { key, colon, value } => line!(
                key,
                special_token_to_elements(colon, TokenContext::Attached),
                value
            ),
            MappingItem::Ident { ident } => line!(ident),
            MappingItem::Spread { stars, expr } => line!(attached_next_token(stars), expr),
        }
    }
}

impl ToElements for FmtExpr<STree<'_, '_>> {
    fn to_elements(&self) -> Vec<Element> {
        line!(self.expr, self.fmt)
    }
}

impl ToElements for FmtSpec<STree<'_, '_>> {
    fn to_elements(&self) -> Vec<Element> {
        line!(
            special_token_to_elements(self.sep, TokenContext::DoublyAttached),
            self.head,
            self.parts
                .iter()
                .map(|(expr, cont)| line!(expr, cont))
                .collect::<Vec<_>>()
        )
    }
}

impl ToElements for MappingKey<STree<'_, '_>> {
    fn to_elements(&self) -> Vec<Element> {
        match self {
            MappingKey::Ident { token } => line!(token),
            MappingKey::Literal { token } => line!(token),
            MappingKey::Expr {
                lparen,
                key,
                rparen,
            } => line!(Element::parens(
                lparen.to_elements(),
                key.to_elements(),
                rparen.to_elements()
            )),
            MappingKey::Fstr {
                begin,
                head,
                parts,
                end,
            } => line!(
                attached_next_token(begin),
                attached_next_token(head),
                parts
                    .iter()
                    .map(|(a, b)| line!(
                        a,
                        special_token_to_elements(b, TokenContext::DoublyAttached)
                    ))
                    .collect::<Vec<_>>(),
                attached_token(end)
            ),
            MappingKey::Unit { lparen, rparen } => line!(lparen, attached_token(rparen)),
            MappingKey::ParenthesizedBlock { .. } => {
                todo!()
            }
        }
    }
}

impl ToElements for SMatchCase<'_, '_> {
    fn to_elements(&self) -> Vec<Element> {
        line!(
            self.pattern,
            self.guard
                .as_ref()
                .map(|(if_kw, guard_expr)| line!(if_kw, guard_expr)),
            self.body
        )
    }
}

fn attached_listing<T>(listing: &SListing<'_, '_, T>) -> Vec<Element>
where
    T: ToElements,
{
    listing_to_elements(listing, true)
}

impl<T> ToElements for SListing<'_, '_, T>
where
    T: ToElements,
{
    fn to_elements(&self) -> Vec<Element> {
        listing_to_elements(self, false)
    }
}

fn listing_to_elements<T>(listing: &SListing<'_, '_, T>, attached: bool) -> Vec<Element>
where
    T: ToElements,
{
    match listing {
        Listing::Inline {
            begin,
            items,
            end,
            newline: _,
        } => {
            line!(Element::listing(
                begin.to_elements(),
                items.iter().map(|item| item.to_elements()).collect(),
                end.to_elements(),
                true,
                attached
            ))
        }
        Listing::Block {
            begin,
            indent,
            items,
            dedent,
            newline,
            end,
        } => {
            line!(Element::listing(
                line!(begin, indent),
                items
                    .iter()
                    .flat_map(|item| split_blocks_into_separate_lines(item.to_elements()))
                    .collect(),
                line!(dedent, newline, end),
                false,
                attached
            ))
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum TokenContext {
    Normal,
    UnaryOperator,
    AttachedNext,
    Attached,
    DoublyAttached,
}

impl IntoElements for &SToken<'_> {
    fn to_elements(self) -> Vec<Element> {
        special_token_to_elements(self, TokenContext::Normal)
    }
}

fn attached_token(token: &SToken) -> Vec<Element> {
    special_token_to_elements(token, TokenContext::Attached)
}

fn attached_next_token(token: &SToken) -> Vec<Element> {
    special_token_to_elements(token, TokenContext::AttachedNext)
}

impl ToElements for Vec<TrivialToken<'_>> {
    fn to_elements(&self) -> Vec<Element> {
        let mut elements = Vec::new();
        for trivium in self {
            match trivium.typ {
                TrivialTokenType::LineComment => {
                    elements.push(Element::comment(trivium.value.to_string()));
                }
                TrivialTokenType::BlockComment => {
                    elements.push(Element::comment(trivium.value.to_string()));
                }
                TrivialTokenType::Newline => {
                    elements.push(Element::line_break());
                }
                TrivialTokenType::Whitespace => {}
            }
        }
        elements
    }
}

fn special_token_to_elements(token: &SToken, context: TokenContext) -> Vec<Element> {
    let mut elements = Vec::new();

    elements.extend(token.leading_trivia.to_elements());

    if !matches!(token.token, Token::Indent | Token::Dedent | Token::Eol) {
        let token_text = token_to_text(token);

        let element = match context {
            TokenContext::AttachedNext => Element::attached_after(token_text),
            TokenContext::Attached => Element::attached_before(token_text),
            TokenContext::DoublyAttached => Element::attached_both(token_text),
            TokenContext::UnaryOperator => {
                if matches!(token.token, Token::Kw("not")) {
                    Element::atom(token_text)
                } else {
                    Element::attached_after(token_text)
                }
            }
            TokenContext::Normal => {
                if token
                    .trailing_trivia
                    .iter()
                    .all(|x| matches!(x.typ, TrivialTokenType::Whitespace))
                    && matches!(token.token, Token::Symbol(s) if s == "[" || s == "(" || s == "{")
                {
                    Element::attached_after(token_text)
                } else if token
                    .leading_trivia
                    .iter()
                    .all(|x| matches!(x.typ, TrivialTokenType::Whitespace))
                    && matches!(token.token, Token::Symbol(s) if s == "]" || s == ")" || s == "}")
                {
                    Element::attached_before(token_text)
                } else {
                    Element::atom(token_text)
                }
            }
        };

        elements.push(element);
    }

    elements.extend(token.trailing_trivia.to_elements());

    elements
}

fn token_to_text(token: &SToken) -> String {
    match &token.token {
        Token::Ident(s) => s.to_string(),
        Token::None => "None".to_string(),
        Token::Bool(b) => {
            if *b {
                "True".to_string()
            } else {
                "False".to_string()
            }
        }
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

impl ToElements for SPattern<'_, '_> {
    fn to_elements(&self) -> Vec<Element> {
        match &self.value {
            Pattern::Capture { name } => line!(name),
            Pattern::Literal { token } => line!(token),
            Pattern::Value { dot, expr } => line!(dot, expr),
            Pattern::As {
                pattern,
                as_kw,
                name,
            } => line!(pattern, as_kw, name),
            Pattern::Or { head, rest } => {
                line!(
                    head,
                    rest.iter()
                        .flat_map(|(pipe, pattern)| line!(pipe, pattern))
                        .collect::<Vec<_>>()
                )
            }
            Pattern::Sequence { listing } => line!(listing),
            Pattern::TupleSequence { kind } => match kind {
                PatternTupleSequenceKind::Unit(lparen, rparen) => {
                    line!(lparen, rparen)
                }
                PatternTupleSequenceKind::Listing(items) => {
                    line!(items)
                }
            },
            Pattern::Mapping { listing } => line!(listing),
            Pattern::Class { expr, items } => line!(expr, attached_listing(items)),
            Pattern::Parenthesized {
                lparen,
                pattern,
                rparen,
            } => line!(Element::parens(
                lparen.to_elements(),
                pattern.to_elements(),
                rparen.to_elements(),
            )),
        }
    }
}

impl ToElements for SInducedBlock<'_, '_> {
    fn to_elements(&self) -> Vec<Element> {
        match self {
            InducedBlock::Inline { inducer, stmt } => {
                let inducer = inducer.map(|inducer| {
                    if token_to_text(inducer) == ":" {
                        attached_token(inducer)
                    } else {
                        inducer.to_elements()
                    }
                });

                line!(inducer, Element::block(vec![stmt_to_elements(stmt)], true))
            }
            InducedBlock::Block {
                inducer,
                indent,
                body,
                dedent,
            } => {
                let inducer = if token_to_text(inducer) == ":" {
                    attached_token(inducer)
                } else {
                    inducer.to_elements()
                };

                let lines = body
                    .value
                    .iter()
                    .flat_map(|x| stmt_to_lines(x))
                    .collect::<Vec<_>>();

                let block = Element::block(lines, false);

                line!(inducer, indent, block, dedent)
            }
        }
    }
}

#[allow(dead_code)]
pub struct LayoutCalculator<'a> {
    config: &'a Config,
}

impl LayoutCalculator<'_> {
    pub fn new(config: &'_ Config) -> LayoutCalculator<'_> {
        LayoutCalculator { config }
    }

    pub fn do_layout(&self, elements: Vec<Line>) -> Vec<Line> {
        elements
            .into_iter()
            .map(|line| self.layout_line(line))
            .collect()
    }

    pub fn layout_line(&self, line: Line) -> Line {
        line.into_iter().map(|e| self.layout_element(e)).collect()
    }

    fn layout_element(&self, element: Element) -> Element {
        let new_data = match element.data {
            ElementData::Block { lines, inline } => {
                let new_lines = lines
                    .into_iter()
                    .map(|line| self.layout_line(line))
                    .collect();
                ElementData::Block {
                    lines: new_lines,
                    inline,
                }
            }
            ElementData::Listing { lines, inline } => {
                let new_lines = lines
                    .into_iter()
                    .map(|line| self.layout_line(line))
                    .collect();
                ElementData::Listing {
                    lines: new_lines,
                    inline,
                }
            }
            ElementData::Parens {
                begin,
                elements,
                end,
            } => {
                let new_begin = self.layout_line(begin);
                let new_elements = self.layout_line(elements);
                let new_end = self.layout_line(end);
                ElementData::Parens {
                    begin: new_begin,
                    elements: new_elements,
                    end: new_end,
                }
            }
            ElementData::Atom { .. } | ElementData::LineBreak => element.data,
        };

        Element {
            attach_before: element.attach_before,
            attach_after: element.attach_after,
            data: new_data,
        }
    }
}

#[derive(Debug)]
struct WriterLineInfo {
    just_emitted_line_break: bool,
    seen_atom: bool,
    continued: bool,
}

#[derive(Debug)]
pub struct LayoutWriter<'a> {
    config: &'a Config,

    output: String,
    indent_level: usize,
    attach_next: bool,

    line_stack: Vec<WriterLineInfo>,
}

impl<'a> LayoutWriter<'a> {
    pub fn new(config: &'a Config) -> Self {
        Self {
            config,
            output: String::new(),
            indent_level: 0,
            attach_next: true,
            line_stack: Vec::new(),
        }
    }

    fn line_break(&mut self) {
        self.output.push('\n');
        self.attach_next = true;
    }

    pub fn write(&mut self, layout: &Vec<Line>) -> String {
        for line in layout {
            self.write_line(line);
        }

        self.output.to_string()
    }

    fn write_line(&mut self, line: &Line) {
        self.line_stack.push(WriterLineInfo {
            just_emitted_line_break: false,
            seen_atom: false,
            continued: false,
        });

        self.write_elements(line);

        let popped = self.line_stack.pop().unwrap();

        if popped.continued {
            self.indent_level -= 1;
        }
    }

    fn write_elements(&mut self, elements: &Elements) {
        for element in elements {
            self.write_element(element);
        }
    }

    fn write_element(&mut self, element: &Element) {
        self.attach_next |= element.attach_before;

        match &element.data {
            ElementData::Atom {
                content,
                is_comment,
            } => {
                let top = self.line_stack.last_mut().unwrap();

                if top.just_emitted_line_break && top.seen_atom {
                    if !top.continued {
                        top.continued = true;
                        self.indent_level += 1;
                    }
                }

                if !*is_comment {
                    top.seen_atom = true;
                }

                top.just_emitted_line_break = false;

                if self.output.ends_with("\n") {
                    for _ in 0..(self.indent_level * self.config.indent_width) {
                        self.output.push(' ');
                    }
                }

                if !self.attach_next {
                    self.output.push(' ');
                }

                self.attach_next = element.attach_after;
                self.output.push_str(content);
            }
            ElementData::LineBreak => {
                let top = self.line_stack.last_mut().unwrap();
                top.just_emitted_line_break = true;
                self.line_break();
            }

            // Composite
            ElementData::Listing { lines, inline } => {
                if *inline {
                    for line in lines {
                        self.write_elements(line);
                    }
                } else {
                    self.indent_level += 1;
                    for line in lines {
                        self.write_line(line);
                    }
                    self.indent_level -= 1;

                    /*
                     * let x = [
                     *    2
                     * ]
                     * ^ we want this to appear on the base line, so reset just_emitted_line_break
                     */
                    let top = self.line_stack.last_mut().unwrap();
                    top.just_emitted_line_break = false;
                }
            }
            ElementData::Parens {
                begin,
                elements,
                end,
            } => {
                self.write_elements(begin);
                self.write_elements(elements);
                self.write_elements(end);
            }
            ElementData::Block { lines, inline } => {
                if *inline {
                    for (i, line) in lines.iter().enumerate() {
                        self.write_line(line);
                        if i < lines.len() - 1 {
                            self.output.push_str("; ");
                            self.attach_next = true;
                        }
                    }
                } else {
                    self.indent_level += 1;
                    for line in lines {
                        self.write_line(line);
                    }
                    self.indent_level -= 1;

                    let top = self.line_stack.last_mut().unwrap();
                    top.just_emitted_line_break = false;
                }
            }
        }
    }
}
