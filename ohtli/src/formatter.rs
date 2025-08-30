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

        let mut lines = Vec::new();

        for stmt in cst.value.iter() {
            lines.extend(stmt_to_lines(stmt));
        }

        let processed_layout = LayoutCalculator::new(&self.config).do_layout(lines);

        let mut output_generator = LayoutWriter::new(&self.config);
        Ok(output_generator.generate_output(processed_layout))
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
    Atom(String),
    Range(String),
    LineComment(String),

    Listing {
        lines: Vec<Line>,
        inline: bool,
    },

    Group {
        begin: Elements,
        elements: Line,
        end: Elements,
    },

    Block {
        lines: Vec<Line>,
        inline: bool,
    },
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

fn stmt_to_lines(stmt: &SStmt) -> Vec<Line> {
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

    vec![tokens]
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
            data: ElementData::Atom(content),
        }
    }

    pub fn attached_after(content: String) -> Self {
        Element {
            attach_before: false,
            attach_after: true,
            data: ElementData::Atom(content),
        }
    }

    pub fn attached_before(content: String) -> Self {
        Element {
            attach_before: false,
            attach_after: true,
            data: ElementData::Atom(content),
        }
    }

    pub fn range(content: String) -> Self {
        Element {
            attach_before: false,
            attach_after: false,
            data: ElementData::Range(content),
        }
    }

    pub fn line_comment(content: String) -> Self {
        Element {
            attach_before: false,
            attach_after: false,
            data: ElementData::LineComment(content),
        }
    }

    pub fn listing(begin: Line, lines: Vec<Line>, end: Line, inline: bool, attached: bool) -> Self {
        Element::group(
            begin,
            vec![Element {
                attach_before: attached,
                attach_after: false,
                data: ElementData::Listing { lines, inline },
            }],
            end,
        )
    }

    pub fn group(begin: Elements, elements: Vec<Element>, end: Elements) -> Self {
        Element {
            attach_before: false,
            attach_after: false,
            data: ElementData::Group {
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
            Expr::Attribute {
                expr,
                dot,
                attr,
                question,
            } => {
                line!(expr, question, dot, attr)
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
                    special_token_to_elements(dots, TokenContext::Range),
                    stop,
                    step_dots.map(|x| special_token_to_elements(x, TokenContext::Range)),
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
                line!(scrutinee, match_kw, colon, indent, cases, dedent)
            }
            Expr::ClassicMatch {
                match_kw,
                scrutinee,
                colon,
                indent,
                cases,
                dedent,
            } => {
                line!(match_kw, scrutinee, colon, indent, cases, dedent)
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
            Expr::RawAttribute {
                expr,
                question,
                double_colon,
                attr,
            } => {
                line!(expr, question, double_colon, attr)
            }
            Expr::ScopedAttribute {
                expr,
                question,
                dot,
                lparen,
                rhs,
                rparen,
            } => {
                line!(expr, question, dot, lparen, rhs, rparen)
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
                ampersand,
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
                todo!()
            }
            Expr::Tuple { kind } => match kind {
                TupleKind::Unit(lparen, rparen) => {
                    todo!()
                }
                TupleKind::Listing(items) => {
                    todo!()
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
        self.item.to_elements()
    }
}

impl ToElements for ArgDefItem<STree<'_, '_>> {
    fn to_elements(&self) -> Vec<Element> {
        match self {
            ArgDefItem::Arg { pattern, default } => {
                line!(
                    pattern,
                    default.as_ref().map(|(eq, expr)| line!(
                        special_token_to_elements(eq, TokenContext::Range),
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
                special_token_to_elements(eq, TokenContext::Range),
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
                special_token_to_elements(eq, TokenContext::Range),
                pattern
            ),
        }
    }
}

impl ToElements for PatternMappingKey<STree<'_, '_>> {
    fn to_elements(&self) -> Vec<Element> {
        match self {
            PatternMappingKey::Ident { token } => line!(token),
            PatternMappingKey::Unit { lparen, rparen } => todo!(),
            PatternMappingKey::Literal { token } => line!(token),
            PatternMappingKey::Expr {
                lparen,
                key,
                rparen,
            } => line!(lparen, key, rparen),
        }
    }
}

impl ToElements for SMappingItem<'_, '_> {
    fn to_elements(&self) -> Vec<Element> {
        match self {
            MappingItem::Item { key, colon, value } => line!(key, colon, value),
            MappingItem::Ident { ident } => line!(ident),
            MappingItem::Spread { stars, expr } => line!(stars, expr),
        }
    }
}

impl ToElements for MappingKey<STree<'_, '_>> {
    fn to_elements(&self) -> Vec<Element> {
        match self {
            MappingKey::Ident { token } => line!(token),
            MappingKey::Literal { token } => line!(token),
            MappingKey::Expr { key, .. } => todo!(),
            MappingKey::Fstr {
                begin,
                head,
                parts,
                end,
            } => {
                todo!()
            }
            MappingKey::Unit { lparen, rparen } => todo!(),
            MappingKey::ParenthesizedBlock { lparen, .. } => {
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
                items.iter().map(|item| item.to_elements()).collect(),
                line!(dedent, newline, end),
                true,
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
    Range,
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

impl ToElements for Vec<Trivium<'_>> {
    fn to_elements(&self) -> Vec<Element> {
        let mut elements = Vec::new();
        for trivium in self {
            match trivium.typ {
                TriviumType::LineComment => {
                    elements.push(Element::line_comment(trivium.value.to_string()));
                }
                TriviumType::BlockComment => {
                    elements.push(Element::atom(trivium.value.to_string()));
                }
                TriviumType::Newline => {
                    // elements.push(Element::line_break());
                }
                TriviumType::Whitespace => {}
            }
        }
        elements
    }
}

fn special_token_to_elements(token: &SToken, context: TokenContext) -> Vec<Element> {
    let mut elements = Vec::new();

    elements.extend(token.leading_trivia.to_elements());

    let token_text = token_to_text(token);

    let element = match context {
        TokenContext::UnaryOperator => Element::attached_after(token_text),
        TokenContext::Normal => Element::atom(token_text),
        TokenContext::Attached => Element::attached_before(token_text),
        TokenContext::Range => Element::range(token_text),
    };
    elements.push(element);

    elements.extend(token.trailing_trivia.to_elements());

    elements
}

fn token_to_text(token: &SToken) -> String {
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
                    todo!()
                }
                PatternTupleSequenceKind::Listing(items) => {
                    let mut tokens = Vec::new();
                    tokens.push(Element::atom("(".to_string()));
                    for item in items {
                        tokens.extend(item.to_elements());
                    }
                    tokens.push(Element::atom(")".to_string()));
                    tokens
                }
            },
            Pattern::Mapping { listing } => line!(listing),
            Pattern::Class { expr, items } => line!(expr, items),
            Pattern::Parenthesized {
                lparen,
                pattern,
                rparen,
            } => {
                line![lparen, &*pattern, rparen]
            }
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

struct LayoutCalculator<'a> {
    config: &'a Config,
}

impl LayoutCalculator<'_> {
    fn new(config: &Config) -> LayoutCalculator {
        LayoutCalculator { config }
    }

    fn do_layout(&self, elements: Vec<Line>) -> Vec<Line> {
        elements
    }
}

struct LayoutWriter<'a> {
    config: &'a Config,

    output: String,
    indent_level: usize,
}

impl<'a> LayoutWriter<'a> {
    fn new(config: &'a Config) -> Self {
        Self {
            config,
            output: String::new(),
            indent_level: 0,
        }
    }

    fn generate_output(&mut self, layout: Vec<Line>) -> String {
        for element in layout {
            self.render_line(&element);
        }
        self.output.to_string()
    }

    fn render_line(&mut self, elements: &Line) {
        let mut current_line = Vec::new();

        for element in elements {
            current_line.push(element.clone());
        }

        if !current_line.is_empty() {
            self.render_layout_elements(&current_line);
        }
    }

    fn render_layout_elements(&mut self, elements: &[Element]) {
        // Render tokens with intelligent spacing
        for (i, token) in elements.iter().enumerate() {
            if i > 0 {
                let prev_token = &elements[i - 1];
                if self.needs_space_between(prev_token, token) {
                    self.output.push(' ');
                }
            }
            self.render_token(token);
        }
    }

    /// Determine if a space is needed between two consecutive tokens
    fn needs_space_between(&self, prev: &Element, current: &Element) -> bool {
        match (prev, current) {
            // No space if current token is attached before
            (_, el) if el.attach_before => false,
            // No space if previous token is attached after
            (el, _) if el.attach_after => false,
            // No space around range operators
            (
                _,
                Element {
                    data: ElementData::Range(_),
                    ..
                },
            ) => false,
            (
                Element {
                    data: ElementData::Range(_),
                    ..
                },
                _,
            ) => false,
            // Otherwise, atomic tokens and other types need spaces
            _ => true,
        }
    }

    fn add_indentation(&mut self) {
        for _ in 0..(self.indent_level * self.config.indent_width) {
            self.output.push(' ');
        }
    }

    fn render_token(&mut self, token: &Element) {
        match &token.data {
            ElementData::Atom(content) => {
                self.output.push_str(content);
            }
            ElementData::Range(content) => {
                self.output.push_str(content);
            }
            ElementData::Group {
                begin,
                elements,
                end,
            } => {
                self.render_layout_elements(begin);
                self.render_layout_elements(elements);
                self.render_layout_elements(end);
            }
            ElementData::Block { lines, inline } => {
                if *inline {
                    for (i, line) in lines.iter().enumerate() {
                        if i > 0 {
                            self.output.push(' ');
                        }
                        self.render_line(line);
                    }
                } else {
                    for line in lines {
                        self.output.push('\n');
                        self.render_line(line);
                    }
                }
            }
            ElementData::LineComment(content) => {
                self.output.push_str(content);
            }
            ElementData::Listing { lines, inline } => todo!(),
        }
    }

    fn render_block_body(&mut self, body: &[Line]) {
        for line in body {
            self.output.push('\n');
            self.render_line(line);
        }
    }
}
