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

        let (cst, parse_errors) = parse_tokens(source, &tokens);

        println!("{:#?}", cst);

        let Some(cst) = cst else {
            anyhow::bail!("Parsing errors: {:?}", parse_errors);
        };

        let mut lines = Vec::new();

        for stmt in cst.value.iter() {
            lines.extend(stmt_to_lines(stmt));
        }

        println!("Input Layout:\n{}", format_lines(&lines));

        let processed_layout = LayoutCalculator::new(&self.config).do_layout(lines);

        println!("Layout:\n{}", format_lines(&processed_layout));

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

    Group {
        elements: Elements,
        power: u8,
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
                    write!(f, "{}", content)
                }
            }
            ElementData::LineBreak => write!(f, "LineBreak"),
            ElementData::Listing { lines, inline } => {
                write!(f, "Listing({}, inline={})", format_lines(lines), inline)
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
            ElementData::Group { elements, power } => {
                write!(f, "Group({}, power={})", format_elements(elements), power)
            }
            ElementData::Block { lines, inline } => {
                write!(f, "Block({}, inline={})", format_lines(lines), inline)
            }
        }
    }
}

fn format_elements(elements: &[Element]) -> String {
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

fn format_lines(lines: &[Line]) -> String {
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

pub fn cleanup_lines(tokens: Elements) -> Vec<Line> {
    let mut saw_semantic_token = false;
    let mut cur_line = vec![];

    let mut lines = vec![];

    for t in tokens {
        match &t.data {
            ElementData::Atom {
                is_comment: true, ..
            } => {}
            ElementData::LineBreak => {
                if !saw_semantic_token {
                    cur_line.push(t);
                    lines.push(std::mem::take(&mut cur_line));
                    continue;
                }
            }
            ElementData::Block { .. } => {
                cur_line.push(t);
                lines.push(std::mem::take(&mut cur_line));
                continue;
            }
            _ => {
                saw_semantic_token = true;
            }
        }

        cur_line.push(t)
    }

    if !cur_line.is_empty() {
        lines.push(cur_line);
    }

    lines
}

pub fn stmt_to_lines(stmt: &SStmt) -> Vec<Line> {
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
        Stmt::Error { raw } => vec![Element::atom(raw.to_string())],
    };

    cleanup_lines(tokens)
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
    pub fn is_multiline(&self) -> bool {
        match &self.data {
            ElementData::LineBreak => false,
            ElementData::Group { .. } => false,
            ElementData::Atom { .. } => true,
            ElementData::Listing { inline, .. } => !*inline,
            ElementData::Parens { .. } => todo!(),
            ElementData::Block { .. } => todo!(),
        }
    }

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
                .flat_map(|(dot, _count)| attached_token(dot))
                .collect::<Vec<_>>(),
            self.trunk
                .iter()
                .flat_map(|(ident, dot)| { line!(attached_token(ident), attached_token(dot)) })
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
                line!(unary_op_token(op), expr)
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
                line!(
                    expr,
                    question.map(attached_token),
                    attached_token(dot),
                    Element::parens(
                        lparen.to_elements(),
                        rhs.to_elements(),
                        rparen.to_elements()
                    )
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
                line!(
                    start,
                    special_token_to_elements(dots, TokenContext::DoublyAttached),
                    stop,
                    step_dots.map(|x| special_token_to_elements(x, TokenContext::DoublyAttached)),
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
                line!(class_kw, args, body)
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
                let mut tokens = Vec::new();
                tokens.extend(try_kw.to_elements());
                tokens.extend(body.clone().to_elements());
                for case in cases {
                    tokens.extend(case.except.to_elements());
                    tokens.extend(case.pattern.clone().to_elements());
                    if let Some((if_kw, guard_expr)) = &case.guard {
                        tokens.extend(if_kw.to_elements());
                        tokens.extend(guard_expr.to_elements());
                    }
                    tokens.extend(case.body.clone().to_elements());
                }
                if let Some((finally_kw, finally_body)) = finally {
                    tokens.extend(finally_kw.to_elements());
                    tokens.extend(finally_body.clone().to_elements());
                }
                tokens
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
                op: ampersand,
                decorator,
            } => {
                line!(expr, ampersand, decorator)
            }
            Expr::Fstr {
                begin,
                head,
                parts,
                end,
            } => {
                // TODO re-glue the parts
                line!(
                    unary_op_token(begin),
                    unary_op_token(head),
                    parts
                        .iter()
                        .map(|(a, b)| line!(
                            a,
                            special_token_to_elements(b, TokenContext::DoublyAttached)
                        ))
                        .collect::<Vec<_>>(),
                    attached_token(end)
                )
            }
            Expr::Tuple { kind } => match kind {
                TupleKind::Unit(lparen, rparen) => {
                    line!(lparen, rparen)
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
                todo!()
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
            ArgDefItem::ArgSpread { star, name } => line!(unary_op_token(star), name),
            ArgDefItem::KwargSpread { stars, name } => line!(unary_op_token(stars), name),
        }
    }
}

impl ToElements for SListItem<'_, '_> {
    fn to_elements(&self) -> Vec<Element> {
        match self {
            ListItem::Item { expr } => line!(expr),
            ListItem::Spread { star, expr } => line!(unary_op_token(star), expr),
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
            CallItem::ArgSpread { expr, star } => line!(unary_op_token(star), expr),
            CallItem::KwargSpread { expr, stars } => line!(unary_op_token(stars), expr),
        }
    }
}

impl ToElements for PatternSequenceItem<STree<'_, '_>> {
    fn to_elements(&self) -> Vec<Element> {
        match self {
            PatternSequenceItem::Item { pattern } => line!(pattern),
            PatternSequenceItem::Spread { star, name } => line!(unary_op_token(star), name),
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
            PatternMappingItem::Spread { stars, name } => line!(unary_op_token(stars), name),
        }
    }
}

impl ToElements for PatternClassItem<STree<'_, '_>> {
    fn to_elements(&self) -> Vec<Element> {
        match self {
            PatternClassItem::Item { pattern } => line!(pattern),
            PatternClassItem::Kw { name, eq, pattern } => line!(
                name,
                special_token_to_elements(eq, TokenContext::Attached),
                pattern
            ),
        }
    }
}

impl ToElements for PatternMappingKey<STree<'_, '_>> {
    fn to_elements(&self) -> Vec<Element> {
        match self {
            PatternMappingKey::Ident { token } => line!(token),
            PatternMappingKey::Unit { .. } => todo!(),
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
            MappingItem::Spread { stars, expr } => line!(unary_op_token(stars), expr),
        }
    }
}

impl ToElements for FmtExpr<STree<'_, '_>> {
    fn to_elements(&self) -> Vec<Element> {
        let block = Element::block(
            self.stmts
                .value
                .iter()
                .flat_map(|x| stmt_to_lines(x))
                .collect::<Vec<_>>(),
            true,
        );

        line!(self.indent, block, self.dedent, self.fmt)
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
            } => {
                line!(
                    unary_op_token(begin),
                    unary_op_token(head),
                    parts
                        .iter()
                        .map(|(a, b)| line!(
                            a,
                            special_token_to_elements(b, TokenContext::DoublyAttached)
                        ))
                        .collect::<Vec<_>>(),
                    attached_token(end)
                )
            }
            MappingKey::Unit { .. } => todo!(),
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
                    .flat_map(|item| cleanup_lines(item.to_elements()))
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

fn unary_op_token(token: &SToken) -> Vec<Element> {
    special_token_to_elements(token, TokenContext::UnaryOperator)
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
            TokenContext::UnaryOperator => Element::attached_after(token_text),
            TokenContext::Attached => Element::attached_before(token_text),
            TokenContext::DoublyAttached => Element::attached_both(token_text),
            TokenContext::Normal => {
                if token
                    .trailing_trivia
                    .iter()
                    .all(|x| !matches!(x.typ, TrivialTokenType::Whitespace))
                    && matches!(token.token, Token::Symbol(s) if s == "[" || s == "(" || s == "{")
                {
                    Element::attached_after(token_text)
                } else if token
                    .leading_trivia
                    .iter()
                    .all(|x| !matches!(x.typ, TrivialTokenType::Whitespace))
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

                line!(inducer, Element::block(stmt_to_lines(stmt), true))
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

pub struct LayoutCalculator<'a> {
    config: &'a Config,
}

impl LayoutCalculator<'_> {
    pub fn new(config: &Config) -> LayoutCalculator {
        LayoutCalculator { config }
    }

    pub fn do_layout(&self, elements: Vec<Line>) -> Vec<Line> {
        elements
    }
}

#[derive(Debug)]
pub struct LayoutWriter<'a> {
    config: &'a Config,

    output: String,
    indent_level: usize,
    attach_next: bool,
}

impl<'a> LayoutWriter<'a> {
    pub fn new(config: &'a Config) -> Self {
        Self {
            config,
            output: String::new(),
            indent_level: 0,
            attach_next: true,
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

        if !self.output.ends_with("\n") {
            self.output.push('\n');
        }

        self.output.to_string()
    }

    fn write_line(&mut self, line: &Line) {
        self.write_elements(line.iter().collect::<Vec<_>>().as_slice());
    }

    fn write_elements(&mut self, elements: &[&Element]) {
        let mut indented = false;
        let mut has_continuation = false;

        for e in elements {
            if indented && !has_continuation && matches!(e.data, ElementData::Block { .. }) {
                self.indent_level -= 1;
                indented = false;
                has_continuation = false;
            }

            self.write_element(e);

            if !indented && matches!(e.data, ElementData::LineBreak) {
                self.indent_level += 1;
                indented = true;
            }

            if indented && !matches!(e.data, ElementData::LineBreak | ElementData::Block { .. }) {
                has_continuation = true;
            }
        }

        if indented {
            self.indent_level -= 1;
        }
    }

    fn write_element(&mut self, element: &Element) {
        self.attach_next |= element.attach_before;

        match &element.data {
            ElementData::Atom { content, .. } => {
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
            ElementData::Group { elements, .. } => {
                self.write_line(elements);
            }
            ElementData::LineBreak => {
                self.line_break();
            }
            ElementData::Listing { lines, inline } => {
                if *inline {
                    for line in lines {
                        self.write_line(line);
                    }
                } else {
                    self.indent_level += 1;
                    for line in lines {
                        self.write_line(line);
                    }
                    self.indent_level -= 1;
                }
            }
            ElementData::Parens {
                begin,
                elements,
                end,
            } => {
                self.write_line(begin);
                self.write_line(elements);
                self.write_line(end);
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
                }
            }
        }
    }
}
