#![allow(dead_code)]

use std::borrow::Cow;

use crate::ast::*;
use crate::lexer::*;
use chumsky::{extra::ParserExtra, input::ValueInput, prelude::*};

pub trait PatternParserExt<'tokens, 'src: 'tokens, I, E>:
    Parser<'tokens, I, SPatternInner<'src>, E>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    E: ParserExtra<'tokens, I>,
    Self: Clone,
{
    fn spanned_pattern(self) -> impl Parser<'tokens, I, SPattern<'src>, E> + Clone + Sized {
        self.map_with(|x, e| x.spanned(e.span()))
    }
}

impl<'tokens, 'src: 'tokens, I, E, P> PatternParserExt<'tokens, 'src, I, E> for P
where
    P: Parser<'tokens, I, SPatternInner<'src>, E> + Clone,
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    E: ParserExtra<'tokens, I>,
{
}

pub trait ExprParserExt<'tokens, 'src: 'tokens, I, E>:
    Parser<'tokens, I, SExprInner<'src>, E> + Sized + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    E: ParserExtra<'tokens, I>,
{
    fn spanned_expr(self) -> impl Parser<'tokens, I, SExpr<'src>, E> + Clone + Sized {
        self.map_with(|x, e| x.spanned(e.span()))
    }
}

impl<'tokens, 'src: 'tokens, I, E, P> ExprParserExt<'tokens, 'src, I, E> for P
where
    P: Parser<'tokens, I, SExprInner<'src>, E> + Sized + Clone,
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    E: ParserExtra<'tokens, I>,
{
}

pub trait ParserExt<'tokens, 'src: 'tokens, I, O, E>: Parser<'tokens, I, O, E>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    E: ParserExtra<'tokens, I>,
{
    fn delimited_by_with_eol(
        self,
        start: impl Parser<'tokens, I, Token<'src>, E> + Clone,
        end: impl Parser<'tokens, I, Token<'src>, E> + Clone,
    ) -> impl Parser<'tokens, I, O, E> + Clone
    where
        Self: Sized + Clone,
    {
        self.delimited_by(start, just(Token::Eol).or_not().then(end))
    }
}

impl<'tokens, 'src: 'tokens, I, O, E, P> ParserExt<'tokens, 'src, I, O, E> for P
where
    P: Parser<'tokens, I, O, E> + Sized,
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    E: ParserExtra<'tokens, I>,
{
}

fn enumeration<'tokens, 'src: 'tokens, I, O: 'tokens, OS: 'tokens, E, ItemParser, SepParser>(
    item_parser: ItemParser,
    optional_separator: SepParser,
) -> impl Parser<'tokens, I, Vec<O>, E> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    E: ParserExtra<'tokens, I, Error = Rich<'tokens, Token<'src>, Span>>,
    ItemParser: Parser<'tokens, I, O, E> + Clone + 'tokens,
    SepParser: Parser<'tokens, I, OS, E> + Clone + 'tokens,
{
    choice((
        item_parser
            .clone()
            .separated_by(choice((
                optional_separator
                    .clone()
                    .then(just(Token::Eol).or_not())
                    .ignored(),
                optional_separator
                    .clone()
                    .or_not()
                    .then(just(Token::Eol))
                    .ignored(),
            )))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::Indent), just(Token::Dedent))
            .labelled("block enumeration"),
        item_parser
            .separated_by(optional_separator)
            .allow_trailing()
            .collect()
            .labelled("inline enumeration"),
    ))
    .labelled("enumeration")
    .boxed()
}

const START_BLOCK: Token = Token::Symbol(":");
type TExtra<'tokens, 'src> = extra::Err<Rich<'tokens, Token<'src>, Span>>;

pub fn symbol<'tokens, 'src: 'tokens, I>(
    symbol: &'static str,
) -> impl Parser<'tokens, I, Token<'src>, TExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    just(Token::Symbol(symbol))
}

pub fn match_pattern<'tokens, 'src: 'tokens, TInput, PIdent, PQualIdent, PLiteral>(
    ident: PIdent,
    qualified_ident: PQualIdent,
    literal: PLiteral,
) -> (
    impl Parser<'tokens, TInput, SPattern<'src>, TExtra<'tokens, 'src>> + Clone,
    impl Parser<'tokens, TInput, SPattern<'src>, TExtra<'tokens, 'src>> + Clone,
    impl Parser<'tokens, TInput, SPattern<'src>, TExtra<'tokens, 'src>> + Clone,
)
where
    TInput: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    PIdent: Parser<'tokens, TInput, SIdent<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PQualIdent: Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PLiteral: Parser<'tokens, TInput, SLiteral<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
{
    let mut pattern =
        Recursive::<chumsky::recursive::Indirect<TInput, SPattern, TExtra>>::declare();

    let literal_pattern = literal
        .clone()
        .map(|x| Pattern::Literal(x))
        .spanned_pattern()
        .boxed();

    fn to_wildcard<'src>(id: SIdent<'src>) -> Option<SIdent<'src>> {
        if id.value.0 == "_" { None } else { Some(id) }
    }

    let capture_pattern = ident
        .clone()
        .map(to_wildcard)
        .map(Pattern::Capture)
        .spanned_pattern()
        .boxed();

    let value_pattern = symbol(".")
        .ignore_then(qualified_ident.clone())
        .map(|value| Pattern::Value(value.indirect()))
        .spanned_pattern()
        .boxed();

    let group_pattern = pattern
        .clone()
        .delimited_by_with_eol(symbol("("), symbol(")"))
        .boxed();

    let sequence_item = choice((
        symbol("*")
            .ignore_then(ident.clone())
            .map(|x| PatternSequenceItem::Spread(to_wildcard(x))),
        pattern
            .clone()
            .map(|x| PatternSequenceItem::Item(x.indirect())),
    ))
    .boxed();

    let sequence_pattern = enumeration(sequence_item.clone(), symbol(","))
        .map(Pattern::Sequence)
        .delimited_by_with_eol(symbol("["), symbol("]"))
        .spanned_pattern()
        .boxed();

    let sequence_pattern2 = enumeration(sequence_item.clone(), symbol(","))
        .map(Pattern::Sequence)
        .delimited_by_with_eol(symbol("("), symbol(")"))
        .spanned_pattern()
        .boxed();

    let nary_sequence_pattern = sequence_item
        .separated_by(symbol(","))
        .at_least(1)
        .collect::<Vec<_>>()
        .then(symbol(",").to(0).or_not())
        .map_with(|(items, last_comma), e| {
            if items.len() == 1 && last_comma.is_none() {
                let item = items.into_iter().next().unwrap();
                if let PatternSequenceItem::Item(inner) = item {
                    inner.extract()
                } else {
                    Pattern::Sequence(vec![item]).spanned(e.span())
                }
            } else {
                Pattern::Sequence(items).spanned(e.span())
            }
        })
        .boxed();

    let mapping_item = choice((
        symbol("**")
            .ignore_then(ident.clone())
            .map(|x| PatternMappingItem::Spread(to_wildcard(x))),
        choice((
            ident
                .clone()
                .map(|id| Expr::Literal(Literal::Str(id.value.0).spanned(id.span)))
                .spanned_expr(),
            literal.clone().map(Expr::Literal).spanned_expr(),
            qualified_ident
                .clone()
                .delimited_by(symbol("("), symbol(")")),
        ))
        .then_ignore(symbol(":"))
        .then(pattern.clone())
        .map(|(key, value)| PatternMappingItem::Item(key.indirect(), value.indirect())),
        ident.clone().map(PatternMappingItem::Ident),
    ))
    .boxed();

    let mapping_pattern = enumeration(mapping_item, symbol(","))
        .map(SPatternInner::Mapping)
        .delimited_by_with_eol(symbol("{"), symbol("}"))
        .spanned_pattern()
        .boxed();

    let class_item = choice((
        ident
            .clone()
            .then_ignore(symbol("="))
            .then(pattern.clone())
            .map(|(key, value)| PatternClassItem::Kw(key, value.indirect())),
        pattern
            .clone()
            .map(|x| PatternClassItem::Item(x.indirect())),
    ))
    .boxed();

    let class_pattern = qualified_ident
        .clone()
        .then(
            class_item
                .separated_by(symbol(","))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by_with_eol(symbol("("), symbol(")")),
        )
        .map(|(a, b)| Pattern::Class(a.indirect(), b))
        .spanned_pattern()
        .boxed();

    let closed_pattern = choice((
        literal_pattern,
        class_pattern,
        capture_pattern,
        value_pattern.clone(),
        group_pattern,
        sequence_pattern,
        sequence_pattern2,
        mapping_pattern,
    ))
    .boxed();

    let or_pattern = closed_pattern
        .clone()
        .map(|x| x.indirect())
        .separated_by(symbol("|").then(just(Token::Eol).or_not()))
        .at_least(1)
        .allow_leading()
        .collect::<Vec<_>>()
        .map_with(|x, e| {
            if x.len() == 1 {
                x.into_iter().next().unwrap().extract()
            } else {
                Pattern::Or(x).spanned(e.span())
            }
        });

    let as_pattern = or_pattern
        .then(just(Token::Kw("as")).ignore_then(ident.clone()).or_not())
        .map_with(|(pattern, as_ident), e| {
            if let Some(as_ident) = as_ident {
                Pattern::As(pattern.indirect(), as_ident).spanned(e.span())
            } else {
                pattern
            }
        })
        .boxed();

    pattern.define(
        choice((
            as_pattern.clone(),
            value_pattern,
            closed_pattern.clone(),
            symbol("_")
                .map(|_| Pattern::Capture(None))
                .spanned_pattern(),
        ))
        .labelled("pattern"),
    );

    (
        closed_pattern.labelled("pattern").as_context(),
        as_pattern.labelled("pattern").as_context(),
        nary_sequence_pattern.labelled("pattern").as_context(),
    )
}

pub fn match_expr<'tokens, 'src: 'tokens, TInput, PLHS, PGuard, PPattern, PBody>(
    lhs: PLHS,
    case_guard: PGuard,
    nary_pattern: PPattern,
    expr_or_inline_stmt_or_block: PBody,
) -> (
    impl Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone,
    impl Parser<'tokens, TInput, Vec<SMatchCase<'src>>, TExtra<'tokens, 'src>> + Clone,
)
where
    TInput: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    PLHS: Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PGuard: Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PPattern: Parser<'tokens, TInput, SPattern<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PBody: Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
{
    let case_ = group((
        nary_pattern.clone(),
        just(Token::Kw("if")).ignore_then(case_guard).or_not(),
        symbol("=>").ignore_then(expr_or_inline_stmt_or_block.clone()),
    ))
    .map(|(pattern, guard, body)| SMatchCase {
        pattern: Some(pattern.indirect()),
        guard: guard.map(|x| x.indirect()),
        body: body.indirect(),
    })
    .labelled("match-case")
    .as_context()
    .boxed();

    let default_case = just(Token::Ident("default"))
        .then(just(START_BLOCK).or_not())
        .ignore_then(expr_or_inline_stmt_or_block.clone())
        .map(|x| SMatchCase {
            pattern: None,
            guard: None,
            body: x.indirect(),
        })
        .boxed();

    let cases = choice((
        just(Token::Kw("else"))
            .or_not()
            .ignore_then(case_.clone())
            .then_ignore(just(Token::Eol))
            .repeated()
            .collect::<Vec<_>>()
            .then(default_case.clone().then_ignore(just(Token::Eol)).or_not())
            .delimited_by(just(Token::Indent), just(Token::Dedent)),
        case_
            .separated_by(just(Token::Kw("else")))
            .allow_trailing()
            .collect::<Vec<_>>()
            .then(default_case.or_not()),
    ))
    .map(|(mut cases, default)| {
        cases.extend(default.into_iter());
        cases
    })
    .boxed();

    let match_ = lhs
        .then(
            just(Token::Ident("match"))
                .then(just(START_BLOCK).or_not())
                .ignore_then(cases.clone())
                .or_not(),
        )
        .map_with(|(scrutinee, cases), e| {
            if let Some(cases) = cases {
                Expr::Match(scrutinee.indirect(), cases).spanned(e.span())
            } else {
                scrutinee
            }
        })
        .labelled("match")
        .boxed();

    (match_.labelled("match-expression").boxed(), cases)
}

pub fn function<'tokens, 'src: 'tokens, TInput, PBody, PIdent, PExpr, PPattern>(
    block_or_inline_stmt: PBody,
    ident: PIdent,
    expr: PExpr,
    pattern: PPattern,
) -> impl Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone
where
    TInput: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    PBody: Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PIdent: Parser<'tokens, TInput, SIdent<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PExpr: Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PPattern: Parser<'tokens, TInput, SPattern<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
{
    recursive(|fn_| {
        let fn_body = symbol("=>")
            .ignore_then(choice((
                block_or_inline_stmt.clone(),
                // should these be switched?
                fn_.clone().boxed(),
            )))
            .boxed();

        let uni_fn = pattern
            .clone()
            .then(fn_body.clone())
            .map(|(x, body)| Expr::Fn(vec![ArgDefItem::Arg(x.indirect(), None)], body.indirect()))
            .spanned_expr()
            .labelled("uni-fn")
            .boxed();

        let arg_list = enumeration(
            choice((
                symbol("*")
                    .ignore_then(ident.clone())
                    .map(|x| ArgDefItem::ArgSpread(x)),
                symbol("**")
                    .ignore_then(ident.clone())
                    .map(ArgDefItem::KwargSpread),
                pattern
                    .clone()
                    .then(symbol("=").ignore_then(expr.clone()).or_not())
                    .map(|(key, value)| {
                        ArgDefItem::Arg(key.indirect(), value.map(|x| x.indirect()))
                    }),
            )),
            symbol(","),
        )
        .labelled("argument-def-list")
        .boxed();

        let multi_fn = arg_list
            .clone()
            .delimited_by_with_eol(symbol("("), symbol(")"))
            .then(fn_body)
            .map(|(args, body)| SExprInner::Fn(args, body.indirect()))
            .spanned_expr()
            .labelled("multi-fn")
            .as_context()
            .boxed();

        choice((multi_fn, uni_fn)).labelled("fn")
    })
}

pub fn statement<
    'tokens,
    'src: 'tokens,
    TInput,
    PBody,
    PTuple,
    PExpr,
    PIdent,
    PNaryPattern,
    PPattern,
>(
    expr_or_inline_stmt_or_block: PBody,
    nary_tuple: PTuple,
    expr: PExpr,
    ident: PIdent,
    nary_pattern: PNaryPattern,
    pattern: PPattern,
) -> (
    impl Parser<'tokens, TInput, SStmt<'src>, TExtra<'tokens, 'src>> + Clone,
    impl Parser<'tokens, TInput, SStmt<'src>, TExtra<'tokens, 'src>> + Clone,
)
where
    TInput: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    PBody: Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PTuple: Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PExpr: Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PIdent: Parser<'tokens, TInput, SIdent<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PNaryPattern: Parser<'tokens, TInput, SPattern<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
    PPattern: Parser<'tokens, TInput, SPattern<'src>, TExtra<'tokens, 'src>> + Clone + 'tokens,
{
    let decl_mod = choice((
        just(Token::Kw("export")).to(DeclType::Export),
        just(Token::Kw("global")).to(DeclType::Global),
        just(Token::Kw("let")).to(DeclType::Let),
        just(Token::Kw("const")).to(DeclType::Const),
    ));

    let decl_stmt = decl_mod
        .clone()
        .then(ident.clone().separated_by(symbol(",")).collect())
        .map(|(decl, idents)| SStmtInner::Decl(idents, decl))
        .boxed();

    let pattern_assign_stmt = group((
        decl_mod.clone().or_not(),
        nary_pattern.clone(),
        symbol("=").ignore_then(nary_tuple.clone()),
    ))
    .map(|(decl, pat, rhs)| SStmtInner::PatternAssign(pat.indirect(), rhs.indirect(), decl))
    .boxed();

    let inline_pattern_assign_stmt = group((
        decl_mod.clone().or_not(),
        pattern.clone(),
        symbol("=").ignore_then(expr.clone()),
    ))
    .map(|(decl, pat, rhs)| SStmtInner::PatternAssign(pat.indirect(), rhs.indirect(), decl))
    .boxed();

    let aug_op = choice((
        symbol("+=").to(BinaryOp::Add),
        symbol("-=").to(BinaryOp::Sub),
        symbol("*=").to(BinaryOp::Mul),
        symbol("/=").to(BinaryOp::Div),
        symbol("//=").to(BinaryOp::FloorDiv),
        symbol("%=").to(BinaryOp::Mod),
        symbol("|=").to(BinaryOp::Pipe),
        symbol("??=").to(BinaryOp::Coalesce),
    ))
    .map(Some);

    let assign_op = choice((aug_op, symbol("=").to(None)));

    let assign_stmt = group((
        nary_tuple.clone(),
        assign_op.clone().then(nary_tuple.clone()).or_not(),
    ))
    .map(|(lhs, rhs)| {
        if let Some((op, rhs)) = rhs {
            SStmtInner::Assign(lhs.indirect(), rhs.indirect(), op)
        } else {
            SStmtInner::Expr(lhs.indirect())
        }
    })
    .labelled("assignment")
    .boxed();

    let inline_assign_stmt = group((expr.clone(), assign_op.clone().then(expr.clone()).or_not()))
        .map(|(lhs, rhs)| {
            if let Some((op, rhs)) = rhs {
                SStmtInner::Assign(lhs.indirect(), rhs.indirect(), op)
            } else {
                SStmtInner::Expr(lhs.indirect())
            }
        })
        .labelled("assignment")
        .boxed();

    let while_stmt = just(Token::Kw("while"))
        .ignore_then(expr.clone())
        .then_ignore(just(START_BLOCK))
        .then(expr_or_inline_stmt_or_block.clone())
        .map(|(cond, body)| SStmtInner::While(cond.indirect(), body.indirect()))
        .labelled("while statement")
        .boxed();

    let except_block = just(Token::Eol)
        .then(just(Token::Kw("except")))
        .ignore_then(nary_pattern.clone().or_not())
        .boxed()
        .then(just(START_BLOCK).ignore_then(expr_or_inline_stmt_or_block.clone()))
        .map(|(pattern, body)| SMatchCase {
            pattern: pattern.map(|x| x.indirect()),
            guard: None,
            body: body.indirect(),
        })
        .labelled("except block")
        .boxed();

    let finally_block = one_of([Token::Eol])
        .then(just(Token::Kw("finally")))
        .then(just(START_BLOCK))
        .ignore_then(expr_or_inline_stmt_or_block.clone())
        .labelled("finally block")
        .boxed();

    let try_stmt = just(Token::Kw("try"))
        .then(just(START_BLOCK))
        .ignore_then(group((
            expr_or_inline_stmt_or_block.clone(),
            except_block.repeated().collect(),
            finally_block.or_not(),
        )))
        .map(|(body, excepts, finally)| {
            SStmtInner::Try(body.indirect(), excepts, finally.map(|x| x.indirect()))
        })
        .labelled("try statement")
        .boxed();

    let for_stmt = just(Token::Kw("for"))
        .ignore_then(group((
            nary_pattern.clone().then_ignore(just(Token::Kw("in"))),
            expr.clone().then_ignore(just(START_BLOCK)),
            expr_or_inline_stmt_or_block.clone(),
        )))
        .map(|(decl, iter, body)| {
            SStmtInner::For(decl.indirect(), iter.indirect(), body.indirect())
        })
        .labelled("for statement")
        .boxed();

    let return_stmt = just(Token::Kw("return"))
        .ignore_then(nary_tuple.clone())
        .map(|x| SStmtInner::Return(x.indirect()))
        .labelled("return statement")
        .boxed();

    let inline_return_stmt = just(Token::Kw("return"))
        .ignore_then(expr.clone())
        .map(|x| SStmtInner::Return(x.indirect()))
        .labelled("inline return statement")
        .boxed();

    let raise_stmt = just(Token::Kw("raise"))
        .ignore_then(nary_tuple.clone().or_not())
        .map(|x| SStmtInner::Raise(x.map(|x| x.indirect())))
        .labelled("raise statement")
        .boxed();

    let inline_raise_stmt = just(Token::Kw("raise"))
        .ignore_then(expr.clone().or_not())
        .map(|x| SStmtInner::Raise(x.map(|x| x.indirect())))
        .labelled("inline raise statement")
        .boxed();

    let break_stmt = just(Token::Kw("break"))
        .map(|_| SStmtInner::Break)
        .labelled("break statement")
        .boxed();

    let continue_stmt = just(Token::Kw("continue"))
        .map(|_| SStmtInner::Continue)
        .labelled("continue statement")
        .boxed();

    let import_item = recursive(|import_item| {
        // TODO this looks extremely cursed
        let level = symbol("..")
            .to(2)
            .repeated()
            .foldr(
                symbol(".")
                    .or_not()
                    .map(|x| if x.is_some() { 1 } else { 0 }),
                |acc, sum| acc + sum,
            )
            .boxed();

        let prefix = ident
            .clone()
            .then_ignore(symbol("."))
            .repeated()
            .collect()
            .boxed();

        choice((
            group((
                level,
                prefix,
                choice((
                    enumeration(import_item.clone(), symbol(","))
                        .delimited_by_with_eol(symbol("("), symbol(")"))
                        .map(ImportLeaf::Multi),
                    ident
                        .clone()
                        .then(just(Token::Kw("as")).ignore_then(ident.clone()).or_not())
                        .map(|(item, name)| ImportLeaf::Single(item, name)),
                    symbol("*").map(|_| ImportLeaf::Star),
                )),
            ))
            .map_with(|(level, prefix, choice), e| ImportTree {
                trunk: prefix,
                level: level,
                leaf: choice.spanned(e.span()),
            }),
            symbol(".")
                .ignore_then(just(Token::Kw("as")).ignore_then(ident.clone()).or_not())
                .map_with(|alias, e| ImportTree {
                    trunk: vec![],
                    level: 0,
                    leaf: ImportLeaf::This(alias).spanned(e.span()),
                }),
        ))
    });

    let import_stmt = just(Token::Kw("export"))
        .to(1)
        .or_not()
        .then_ignore(just(Token::Kw("import")))
        .then(import_item)
        .map(|(reexport, tree)| SStmtInner::Import(tree, reexport.is_some()))
        .labelled("import statement")
        .boxed();

    let stmt = choice((
        import_stmt,
        pattern_assign_stmt,
        assign_stmt,
        decl_stmt,
        while_stmt.clone(),
        for_stmt.clone(),
        return_stmt,
        raise_stmt,
        break_stmt.clone(),
        continue_stmt.clone(),
        try_stmt,
    ))
    .labelled("statement")
    .map_with(|x, e| x.spanned(e.span()))
    .boxed();

    let inline_stmt = choice((
        inline_pattern_assign_stmt,
        inline_assign_stmt,
        while_stmt,
        for_stmt,
        inline_return_stmt,
        inline_raise_stmt,
        break_stmt,
        continue_stmt,
    ))
    .labelled("inline-statement")
    .map_with(|x, e| x.spanned(e.span()))
    .boxed();

    (stmt, inline_stmt)
}

pub fn parser<'tokens, 'src: 'tokens, TInput>()
-> impl Parser<'tokens, TInput, SExpr<'src>, TExtra<'tokens, 'src>> + Clone
where
    TInput: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
{
    let mut stmt = Recursive::<chumsky::recursive::Indirect<TInput, SStmt, TExtra>>::declare();
    let mut inline_stmt =
        Recursive::<chumsky::recursive::Indirect<TInput, SStmt, TExtra>>::declare();

    let mut atom = Recursive::<chumsky::recursive::Indirect<TInput, SExpr, TExtra>>::declare();
    let mut expr = Recursive::<chumsky::recursive::Indirect<TInput, SExpr, TExtra>>::declare();
    let mut unary = Recursive::<chumsky::recursive::Indirect<TInput, SExpr, TExtra>>::declare();
    let mut cases = Recursive::<chumsky::recursive::Indirect<TInput, _, TExtra>>::declare();
    let mut below_fn = Recursive::<chumsky::recursive::Indirect<TInput, SExpr, TExtra>>::declare();

    let stmts = stmt
        .clone()
        .then_ignore(just(Token::Eol).or(symbol(";")))
        .map(|x| x.indirect())
        .repeated()
        .collect::<Vec<Indirect<SStmt>>>()
        .labelled("statement-list")
        .boxed();

    let block = stmts
        .clone()
        .delimited_by(just(Token::Indent), just(Token::Dedent))
        .map(SExprInner::Block)
        .spanned_expr()
        .boxed();

    let expr_or_block = choice((block.clone(), expr.clone())).boxed();

    let expr_or_inline_stmt_or_block = choice((
        block.clone(),
        inline_stmt
            .clone()
            .map_with(|x, e| match x.value {
                Stmt::Expr(x) => x.extract(),
                _ => Expr::Block(vec![x.indirect()]).spanned(e.span()),
            })
            .boxed(),
    ))
    .boxed();

    let other_literal = select! {
        Token::Num(s) => Literal::Num(Cow::Borrowed(s)),
        Token::Bool(s) => Literal::Bool(s),
        Token::None => Literal::None
    }
    .map_with(|x, e| x.spanned(e.span()))
    .labelled("literal")
    .boxed();

    let literal_str = select! {
        Token::Str(s) => s
    }
    .repeated()
    .at_least(1)
    .collect::<Vec<_>>()
    .map_with(|x, e| Literal::Str(Cow::Owned(x.join(""))).spanned(e.span()))
    .labelled("string-literal")
    .boxed();

    let literal = choice((literal_str, other_literal))
        .labelled("literal")
        .boxed();

    let literal_expr = literal
        .clone()
        .map(Expr::Literal)
        .spanned_expr()
        .labelled("literal-expr")
        .boxed();

    let ident = select! {
        Token::Ident(s) => Ident(Cow::Borrowed(s)),
    }
    .map_with(|x, e| x.spanned(e.span()))
    .labelled("identifier")
    .boxed();

    let ident_expr = ident
        .clone()
        .map(Expr::Ident)
        .spanned_expr()
        .labelled("identifier-expr")
        .boxed();

    let placeholder = select! {
        Token::Symbol("$") => Expr::Placeholder,
    }
    .spanned_expr()
    .labelled("placeholder")
    .boxed();

    let list_item = choice((
        symbol("*")
            .ignore_then(expr.clone())
            .map(|x| ListItem::Spread(x.indirect())),
        expr.clone().map(|x| ListItem::Item(x.indirect())),
    ))
    .boxed();

    let list = enumeration(list_item.clone(), symbol(","))
        .delimited_by_with_eol(symbol("["), symbol("]"))
        .map(Expr::List)
        .spanned_expr()
        .labelled("list")
        .as_context()
        .boxed();

    let nary_tuple = group((
        list_item.clone(),
        symbol(",")
            .ignore_then(list_item)
            .repeated()
            .collect::<Vec<_>>(),
        symbol(",").to(1).or_not(),
    ))
    .try_map_with(
        |(first, rest, last_comma), e| -> Result<SExpr, Rich<'tokens, Token<'src>, Span>> {
            let mut items = Vec::<ListItem<STree>>::new();

            match first {
                ListItem::Item(expr) if rest.is_empty() && last_comma.is_none() => {
                    return Ok(expr.extract());
                }
                // ListItem::Spread(expr) if rest.is_empty() && last_comma.is_none() => {
                //     // should this be an error?

                //     return Err(Rich::custom(
                //         first.1,
                //         "Spread operator must be in a list or tuple",
                //     ));
                // }
                _ => {
                    items.push(first);
                }
            }
            items.extend(rest);

            Ok(Expr::Tuple(items).spanned(e.span()))
        },
    )
    .labelled("nary-tuple")
    .boxed();
    // .memoized();

    let inline_stmts = stmt
        .clone()
        .map(|x| x.indirect())
        .separated_by(symbol(";"))
        .allow_trailing()
        .at_least(1)
        .collect::<Vec<_>>()
        .try_map_with(|x, e| {
            if x.is_empty() {
                panic!("Expected at least one statement, got none");
            } else if x.len() == 1 {
                match &x[0].value {
                    Stmt::Expr(_) => {
                        let Stmt::Expr(x) = x.into_iter().next().unwrap().value else {
                            panic!();
                        };
                        Ok(x.extract())
                    }
                    _ => Ok(SExprInner::Block(x).spanned(e.span())),
                }
            } else {
                Ok(SExprInner::Block(x).spanned(e.span()))
            }
        })
        .boxed();

    let round_brackets = choice((
        symbol("(")
            .then(symbol(")"))
            .map(|_| Expr::Tuple(vec![]))
            .spanned_expr(),
        inline_stmts
            .clone()
            .delimited_by_with_eol(symbol("("), symbol(")")),
        block
            .clone()
            .delimited_by_with_eol(symbol("("), symbol(")")),
    ))
    .boxed();

    let mapping = enumeration(
        choice((
            symbol("**")
                .ignore_then(expr.clone().map(|x| x.indirect()))
                .map(MappingItem::Spread),
            choice((
                ident
                    .clone()
                    .map(|id| Expr::Literal(Literal::Str(id.value.0).spanned(id.span)))
                    .spanned_expr(),
                literal_expr.clone(),
                round_brackets.clone(),
            ))
            .then_ignore(symbol(":"))
            .then(expr.clone())
            .map(|(key, value)| MappingItem::Item(key.indirect(), value.indirect())),
            ident_expr.clone().map(|x| MappingItem::Ident(x.indirect())),
        )),
        symbol(","),
    )
    .delimited_by_with_eol(just(Token::Symbol("{")), just(Token::Symbol("}")))
    .map(Expr::Mapping)
    .spanned_expr()
    .labelled("mapping")
    .as_context()
    .boxed();

    let fstr_begin = select! {
        Token::FstrBegin(s) => s,
    }
    .map_with(|x, e| x.spanned(e.span()));

    let fstr_continue = select! {
        Token::FstrContinue(s) => s,
    }
    .map_with(|x, e| x.spanned(e.span()));

    let mut fstr = Recursive::<chumsky::recursive::Indirect<TInput, SExpr, TExtra>>::declare();

    fstr.define(
        fstr_begin
            .then(
                expr_or_block
                    .clone()
                    .then(symbol("!").ignore_then(fstr.clone()).or_not())
                    .then(fstr_continue)
                    .map(|((block, fmt), cont)| {
                        (
                            FmtExpr {
                                expr: block.indirect(),
                                fmt: fmt.map(|x| x.indirect()),
                            },
                            cont,
                        )
                    })
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map(|(begin, parts)| SExprInner::Fstr(begin, parts))
            .spanned_expr()
            .labelled("f-string")
            .boxed(),
    );

    let class_ = just(Token::Kw("class"))
        .ignore_then(
            enumeration(
                choice((
                    ident
                        .clone()
                        .then_ignore(symbol("="))
                        .then(expr.clone())
                        .map(|(key, value)| CallItem::Kwarg(key, value.indirect())),
                    expr.clone().map(|x| CallItem::Arg(x.indirect())),
                ))
                .boxed(),
                symbol(","),
            )
            .delimited_by_with_eol(just(Token::Symbol("(")), just(Token::Symbol(")")))
            .or_not(),
        )
        .then_ignore(just(START_BLOCK))
        .then(expr_or_inline_stmt_or_block.clone())
        .map(|(arglist, block)| {
            SExprInner::Class(arglist.unwrap_or_else(|| Vec::new()), block.indirect())
        })
        .spanned_expr()
        .labelled("class")
        .boxed();

    let classic_if = just(Token::Kw("if"))
        .ignore_then(group((
            expr.clone().then_ignore(just(START_BLOCK)),
            expr_or_inline_stmt_or_block.clone(),
            group((
                just(Token::Eol).or_not(),
                just(Token::Kw("else")),
                just(START_BLOCK).or_not(),
            ))
            .ignore_then(expr_or_inline_stmt_or_block.clone())
            .or_not(),
        )))
        .map(|(cond, if_, else_)| {
            Expr::If(cond.indirect(), if_.indirect(), else_.map(|x| x.indirect()))
        })
        .spanned_expr()
        .labelled("if")
        .boxed();

    let qualified_ident = ident_expr
        .clone()
        .foldl_with(
            symbol(".").ignore_then(ident.clone()).repeated(),
            |lhs, rhs, e| Expr::RawAttribute(lhs.indirect(), rhs).spanned(e.span()),
        )
        .boxed();

    let (closed_pattern, as_pattern, nary_pattern) =
        match_pattern(ident.clone(), qualified_ident.clone(), literal.clone());

    let with = group((
        just(Token::Kw("with")).ignore_then(nary_pattern.clone()),
        symbol("=")
            .ignore_then(expr.clone())
            .then_ignore(just(START_BLOCK)),
        expr_or_inline_stmt_or_block.clone(),
    ))
    .map(|(pattern, value, body)| Expr::With(pattern.indirect(), value.indirect(), body.indirect()))
    .spanned_expr()
    .labelled("with")
    .boxed();

    let classic_match = just(Token::Ident("match"))
        .ignore_then(expr.clone())
        .then_ignore(just(START_BLOCK))
        .then(cases.clone())
        .map(|(scrutinee, cases)| Expr::Match(scrutinee.indirect(), cases))
        .spanned_expr()
        .labelled("classic-match")
        .as_context()
        .boxed();

    enum ControlKw {
        Await,
        Yield,
        YieldFrom,
    }

    let control_kw = choice((
        just(Token::Kw("await")).map(|_| ControlKw::Await),
        just(Token::Kw("yield"))
            .ignore_then(just(Token::Ident("from")).or_not())
            .map(|star| {
                if star.is_some() {
                    ControlKw::YieldFrom
                } else {
                    ControlKw::Yield
                }
            }),
    ))
    .repeated()
    .at_least(1)
    .foldr_with(expr.clone(), |lhs, expr, e| {
        let expr = match lhs {
            ControlKw::Await => Expr::Await(expr.indirect()),
            ControlKw::Yield => Expr::Yield(expr.indirect()),
            ControlKw::YieldFrom => Expr::YieldFrom(expr.indirect()),
        };

        expr.spanned(e.span())
    })
    .labelled("control-expression");

    let async_memo_expr = just(Token::Ident("async"))
        .then(just(Token::Ident("memo")))
        .then(just(START_BLOCK).or_not())
        .ignore_then(expr_or_inline_stmt_or_block.clone())
        .map(|x| Expr::Memo(x.indirect(), true))
        .spanned_expr()
        .labelled("async-memo-expression")
        .boxed();

    let memo_expr = just(Token::Ident("memo"))
        .then(just(START_BLOCK).or_not())
        .ignore_then(expr_or_inline_stmt_or_block.clone())
        .map(|x| Expr::Memo(x.indirect(), false))
        .spanned_expr()
        .labelled("memo-expression")
        .boxed();

    atom.define(
        choice((
            classic_match,
            memo_expr,
            async_memo_expr,
            ident_expr.clone(),
            classic_if,
            with,
            control_kw,
            class_,
            literal_expr.clone(),
            placeholder,
            list.clone(),
            mapping,
            fstr,
            round_brackets.clone(),
        ))
        .labelled("atom"),
    );

    enum Postfix<'a> {
        Call(Vec<CallItem<'a, STree<'a>>>),
        MethodCall(SIdent<'a>, Vec<CallItem<'a, STree<'a>>>),
        Subscript(Vec<ListItem<STree<'a>>>),
        Attribute(SIdent<'a>),
        ScopedAttribute(SExpr<'a>),
        RawAttribute(SIdent<'a>),
    }

    let call_args = enumeration(
        choice((
            symbol("*")
                .ignore_then(expr.clone())
                .map(|x| CallItem::ArgSpread(x.indirect())),
            symbol("**")
                .ignore_then(expr.clone())
                .map(|x| CallItem::KwargSpread(x.indirect())),
            ident
                .clone()
                .then_ignore(symbol("="))
                .then(expr.clone())
                .map(|(key, value)| CallItem::Kwarg(key, value.indirect())),
            expr.clone().map(|x| CallItem::Arg(x.indirect())),
        ))
        .boxed(),
        symbol(","),
    )
    .delimited_by_with_eol(just(Token::Symbol("(")), just(Token::Symbol(")")));

    let call = call_args
        .clone()
        .map(Postfix::Call)
        .labelled("argument-list")
        .boxed();

    let subscript = enumeration(
        choice((
            symbol("*")
                .ignore_then(expr.clone())
                .map(|x| ListItem::Spread(x.indirect())),
            expr.clone().map(|x| ListItem::Item(x.indirect())),
        ))
        .boxed(),
        symbol(","),
    )
    .delimited_by_with_eol(just(Token::Symbol("[")), just(Token::Symbol("]")))
    .map(Postfix::Subscript)
    .labelled("subscript")
    .boxed();

    let method_call = symbol(".")
        .ignore_then(ident.clone())
        .then(call_args.clone())
        .map(|(ident, args)| Postfix::MethodCall(ident, args))
        .labelled("method-call");

    let attribute = symbol(".")
        .ignore_then(ident.clone())
        .map(Postfix::Attribute)
        .labelled("extension-attribute");

    let then = symbol(".")
        .ignore_then(expr.clone().delimited_by_with_eol(symbol("("), symbol(")")))
        .map(|rhs| Postfix::ScopedAttribute(rhs))
        .labelled("then-attribute")
        .boxed();

    let raw_attr = symbol("::")
        .ignore_then(ident.clone())
        .map(Postfix::RawAttribute)
        .labelled("raw-attribute")
        .boxed();

    let postfix = atom
        .clone()
        .foldl_with(
            symbol("?")
                .to(1)
                .or_not()
                .then(choice((
                    call,
                    subscript,
                    method_call,
                    attribute,
                    then,
                    raw_attr,
                )))
                .repeated(),
            |expr, (coal, op), e| -> SExpr {
                if coal.is_none() {
                    match op {
                        Postfix::Call(args) => Expr::Call(expr.indirect(), args),
                        Postfix::MethodCall(name, args) => Expr::Call(
                            Expr::Attribute(expr.indirect(), name)
                                .spanned(e.span())
                                .indirect(),
                            args,
                        ),
                        Postfix::Subscript(args) => Expr::Subscript(expr.indirect(), args),
                        Postfix::RawAttribute(attr) => Expr::RawAttribute(expr.indirect(), attr),
                        Postfix::ScopedAttribute(rhs) => {
                            Expr::ScopedAttribute(expr.indirect(), rhs.indirect())
                        }
                        Postfix::Attribute(rhs) => Expr::Attribute(expr.indirect(), rhs),
                    }
                } else {
                    match op {
                        Postfix::Call(args) => Expr::MappedCall(expr.indirect(), args),
                        Postfix::MethodCall(name, args) => Expr::MappedCall(
                            Expr::MappedAttribute(expr.indirect(), name)
                                .spanned(e.span())
                                .indirect(),
                            args,
                        ),
                        Postfix::Subscript(args) => Expr::MappedSubscript(expr.indirect(), args),
                        Postfix::RawAttribute(attr) => {
                            Expr::MappedRawAttribute(expr.indirect(), attr)
                        }
                        Postfix::ScopedAttribute(rhs) => {
                            Expr::MappedScopedAttribute(expr.indirect(), rhs.indirect())
                        }
                        Postfix::Attribute(rhs) => Expr::MappedAttribute(expr.indirect(), rhs),
                    }
                }
                .spanned(e.span())
            },
        )
        .labelled("postfix")
        .boxed();

    let decorator = postfix
        .clone()
        .then(symbol("&").ignore_then(expr.clone()).or_not())
        .map_with(|(lhs, rhs), e| {
            if let Some(rhs) = rhs {
                Expr::Decorated(lhs.indirect(), rhs.indirect()).spanned(e.span())
            } else {
                lhs
            }
        })
        .labelled("decorator")
        .boxed();

    let mut checked = Recursive::<chumsky::recursive::Indirect<TInput, SExpr, TExtra>>::declare();

    unary.define(
        select! {
            Token::Symbol("@") => UnaryOp::Bind,
            Token::Symbol("+") => UnaryOp::Pos,
            Token::Symbol("-") => UnaryOp::Neg,
            Token::Symbol("~") => UnaryOp::Inv,
        }
        .repeated()
        .foldr_with(
            choice((decorator, checked.clone())),
            |op: UnaryOp, rhs: SExpr, e| Expr::Unary(op, rhs.indirect()).spanned(e.span()),
        )
        .labelled("unary-expression")
        .boxed(),
    );

    fn make_binary_op<'tokens, 'src: 'tokens, I, POp, PArg>(
        arg: PArg,
        op: POp,
        right_assoc: bool,
    ) -> impl Parser<'tokens, I, SExpr<'src>, extra::Err<Rich<'tokens, Token<'src>, Span>>> + Clone
    where
        PArg: Parser<'tokens, I, SExpr<'src>, extra::Err<Rich<'tokens, Token<'src>, Span>>>
            + Clone
            + 'tokens,
        POp: Parser<'tokens, I, BinaryOp, extra::Err<Rich<'tokens, Token<'src>, Span>>>
            + Clone
            + 'tokens,
        I: ValueInput<'tokens, Token = Token<'src>, Span = Span>,
    {
        if !right_assoc {
            arg.clone()
                .foldl_with(op.then(arg).repeated(), |lhs, (op, rhs), e| {
                    Expr::Binary(op, lhs.indirect(), rhs.indirect()).spanned(e.span())
                })
                .boxed()
        } else {
            recursive(|bin| {
                arg.clone()
                    .then(op.then(bin.or(arg.clone())).or_not())
                    .map_with(|(lhs, matched), e| {
                        if let Some((op, rhs)) = matched {
                            Expr::Binary(op, lhs.indirect(), rhs.indirect()).spanned(e.span())
                        } else {
                            lhs
                        }
                    })
            })
            .boxed()
        }
    }

    let binary0 = make_binary_op(
        unary.clone(),
        select! {
            Token::Symbol("**") => BinaryOp::Exp,
        },
        true,
    );

    let binary1 = make_binary_op(
        binary0,
        select! {
            Token::Symbol("*") => BinaryOp::Mul,
            Token::Symbol("/") => BinaryOp::Div,
            Token::Symbol("//") => BinaryOp::FloorDiv,
            Token::Symbol("%") => BinaryOp::Mod,
            Token::Symbol("@") => BinaryOp::MatMul,
        },
        false,
    );

    let binary2 = make_binary_op(
        binary1,
        select! {
            Token::Symbol("+") => BinaryOp::Add,
            Token::Symbol("-") => BinaryOp::Sub,
        },
        false,
    );

    let binary3 = make_binary_op(
        binary2.clone(),
        select! {
            Token::Kw("in") => BinaryOp::In,
            Token::Symbol("<") => BinaryOp::Lt,
            Token::Symbol("<=") => BinaryOp::Leq,
            Token::Symbol(">") => BinaryOp::Gt,
            Token::Symbol(">=") => BinaryOp::Geq,
            Token::Symbol("==") => BinaryOp::Eq,
            Token::Symbol("<>") => BinaryOp::Neq,
            Token::Symbol("===") => BinaryOp::Is,
            Token::Symbol("<=>") => BinaryOp::Nis,
        }
        .or(just(Token::Kw("not"))
            .then(just(Token::Kw("in")))
            .to(BinaryOp::Nin)),
        false,
    );

    let mut not_or_try =
        Recursive::<chumsky::recursive::Indirect<TInput, SExpr, TExtra>>::declare();

    let not = just(Token::Kw("not"))
        .ignore_then(not_or_try.clone())
        .map(|expr| Expr::Unary(UnaryOp::Not, expr.indirect()))
        .spanned_expr()
        .boxed();

    checked.define(
        just(Token::Kw("try"))
            .ignore_then(not_or_try.clone())
            .then(
                just(Token::Kw("except"))
                    .ignore_then(closed_pattern.clone())
                    .or_not(),
            )
            .map(|(expr, typs)| Expr::Checked(expr.indirect(), typs.map(|x| x.indirect())))
            .spanned_expr()
            .labelled("checked")
            .boxed(),
    );

    not_or_try.define(choice((not, checked, binary3.clone())));

    let binary4 = make_binary_op(
        not_or_try,
        select! {
            Token::Symbol("??") => BinaryOp::Coalesce,
        },
        false,
    );

    let slice0 = group((
        binary4.clone(),
        symbol("..").ignore_then(binary4.clone().or_not()).or_not(),
        symbol("..").ignore_then(binary4.clone().or_not()).or_not(),
    ))
    .map_with(|(lhs, a, b), e| {
        if a.is_none() && b.is_none() {
            lhs
        } else {
            Expr::Slice(
                Some(lhs.indirect()),
                a.flatten().map(|x| x.indirect()),
                b.flatten().map(|x| x.indirect()),
            )
            .spanned(e.span())
        }
    })
    .labelled("slice")
    .boxed();

    let slice1 = symbol("..")
        .ignore_then(binary4.clone().or_not())
        .then(symbol("..").ignore_then(binary4.clone().or_not()).or_not())
        .map(|(e1, e2)| {
            Expr::Slice(
                None,
                e1.map(|x| x.indirect()),
                e2.flatten().map(|x| x.indirect()),
            )
        })
        .spanned_expr()
        .labelled("slice")
        .boxed();

    let slices = choice((slice0, slice1));

    let (match_, cases_) = match_expr(
        slices,
        below_fn.clone(),
        nary_pattern.clone(),
        expr_or_inline_stmt_or_block.clone(),
    );

    cases.define(cases_);

    let matches = match_
        .then(
            just(Token::Ident("matches"))
                .ignore_then(just(Token::Kw("not")).to(0).or_not())
                .then(as_pattern.clone())
                .or_not(),
        )
        .map_with(|(expr, matches_part), e| {
            if let Some((not, pattern)) = matches_part {
                if not.is_some() {
                    Expr::Unary(
                        UnaryOp::Not,
                        Expr::Matches(expr.indirect(), pattern.indirect())
                            .spanned(e.span())
                            .indirect(),
                    )
                    .spanned(e.span())
                } else {
                    Expr::Matches(expr.indirect(), pattern.indirect()).spanned(e.span())
                }
            } else {
                expr
            }
        });

    let binary5 = make_binary_op(
        matches,
        select! {
            Token::Kw("and") => BinaryOp::And,
        },
        false,
    );

    let binary6 = make_binary_op(
        binary5,
        select! {
            Token::Kw("or") => BinaryOp::Or,
        },
        false,
    );

    let if_ = binary6
        .then(
            group((
                just(Token::Kw("then"))
                    .then(just(START_BLOCK).or_not())
                    .ignore_then(expr_or_inline_stmt_or_block.clone()),
                just(Token::Eol)
                    .or_not()
                    .then(just(Token::Kw("else")))
                    .then(just(START_BLOCK).or_not())
                    .ignore_then(expr_or_inline_stmt_or_block.clone())
                    .or_not(),
            ))
            .or_not(),
        )
        .map_with(|(cond, if_cases), e| {
            if let Some((if_, else_)) = if_cases {
                Expr::If(cond.indirect(), if_.indirect(), else_.map(|x| x.indirect()))
                    .spanned(e.span())
            } else {
                cond
            }
        });

    below_fn.define(if_);

    let fn_ = function(
        expr_or_inline_stmt_or_block.clone(),
        ident.clone(),
        expr.clone(),
        closed_pattern.clone(),
    );

    let binary6 = make_binary_op(
        choice((fn_, below_fn)),
        select! {
            Token::Symbol("|") => BinaryOp::Pipe,
        },
        false,
    );

    expr.define(
        binary6.labelled("expression").as_context().boxed(), // .memoized(),
    );

    let (stmt_, inline_stmt_) = statement(
        expr_or_inline_stmt_or_block.clone(),
        nary_tuple.clone(),
        expr.clone(),
        ident.clone(),
        nary_pattern.clone(),
        as_pattern.clone(),
    );

    stmt.define(stmt_.labelled("statement").boxed());
    inline_stmt.define(inline_stmt_.labelled("inline-statement").boxed());

    block.labelled("program")
}

pub fn parse_tokens<'tokens, 'src: 'tokens>(
    src: &'src str,
    tokens: &'tokens TokenList<'src>,
) -> (Option<SExpr<'src>>, Vec<Rich<'tokens, Token<'src>, Span>>) {
    parser()
        .parse(
            tokens
                .0
                .as_slice()
                // convert the span type with map
                .map((src.len()..src.len()).into(), |tok| (&tok.value, &tok.span)),
        )
        .into_output_errors()
}
