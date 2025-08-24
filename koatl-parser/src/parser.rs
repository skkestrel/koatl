#![allow(dead_code)]

use crate::cst::*;
use crate::lexer::*;
use crate::parser_error::TriviaRich;
use chumsky::input::BorrowInput;
use chumsky::label::LabelError;
use chumsky::prelude::*;

// Macro to define parsers with less boilerplate
macro_rules! simple_parser_fn {
    // Simple parser that takes no other parsers as input
    ($name:ident -> $ret:ty, $label:literal, $body:expr) => {
        pub fn $name<'src: 'tok, 'tok, I>() -> impl Parser<'tok, I, $ret, TExtra<'src, 'tok>> + Clone
        where
            I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
        {
            $body.labelled($label)
        }
    };

    // Parser that takes a simple parameter (not a parser)
    ($name:ident($param:ident: $param_ty:ty) -> $ret:ty, $label:literal, $body:expr) => {
        pub fn $name<'src: 'tok, 'tok, I>(
            $param: $param_ty,
        ) -> impl Parser<'tok, I, $ret, TExtra<'src, 'tok>> + Clone
        where
            I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
        {
            $body.labelled($label)
        }
    };
}

macro_rules! composite_parser_fn {
    // Parser that takes one other parser as input
    ($name:ident<$param_name:ident: $ParamType:ident: $param_ty:ty> -> $ret:ty, $label:literal, $body:expr) => {
        pub fn $name<'src: 'tok, 'tok, I, $ParamType>(
            $param_name: $ParamType,
        ) -> impl Parser<'tok, I, $ret, TExtra<'src, 'tok>> + Clone
        where
            I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
            $ParamType: Parser<'tok, I, $param_ty, TExtra<'src, 'tok>> + Clone + 'tok,
        {
            $body.labelled($label)
        }
    };

    // Parser that takes two other parsers as input
    ($name:ident<$param1_name:ident: $param1:ident: $param1_ty:ty, $param2_name:ident: $param2:ident: $param2_ty:ty> -> $ret:ty, $label:literal, $body:expr) => {
        pub fn $name<'src: 'tok, 'tok, I, $param1, $param2>(
            $param1_name: $param1,
            $param2_name: $param2,
        ) -> impl Parser<'tok, I, $ret, TExtra<'src, 'tok>> + Clone
        where
            I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
            $param1: Parser<'tok, I, $param1_ty, TExtra<'src, 'tok>> + Clone + 'tok,
            $param2: Parser<'tok, I, $param2_ty, TExtra<'src, 'tok>> + Clone + 'tok,
        {
            $body.labelled($label)
        }
    };
}

const START_BLOCK: Token = Token::Symbol(":");
type TErr<'src, 'tok> = TriviaRich<'tok, 'src, Span>;
type TExtra<'src, 'tok> = extra::Err<TErr<'src, 'tok>>;

fn token<'src: 'tok, 'tok, I>(
    token: Token<'src>,
) -> impl Parser<'tok, I, &'tok SToken<'src>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
{
    custom(move |input| {
        let start = input.cursor();

        let next: Option<&SToken<'src>> = input.next_ref();

        if let Some(found) = next {
            if found.token == token {
                return Ok(found);
            }
        }

        Err(LabelError::<I, _>::expected_found(
            vec![format!("{}", token)],
            next.map(|x| x.into()),
            input.span_since(&start),
        ))
    })
}

simple_parser_fn!(symbol(symbol: &'static str) -> &'tok SToken<'src>, "symbol",
    token(Token::Symbol(symbol))
);

simple_parser_fn!(kw(symbol: &'static str) -> &'tok SToken<'src>, "keyword",
    token(Token::Kw(symbol))
);

simple_parser_fn!(ident(symbol: &'static str) -> &'tok SToken<'src>, "identifier",
    token(Token::Ident(symbol))
);

simple_parser_fn!(any_ident -> &'tok SToken<'src>, "identifier",
    custom(|input| {
        let start = input.cursor();
        let next: Option<&SToken<'src>> = input.next_ref();

        if let Some(found) = next {
            if let Token::Ident(_) = found.token {
                return Ok(found);
            }
        }

        Err(LabelError::<I, _>::expected_found(
            vec!["identifier".to_string()],
            next.map(|x| x.into()),
            input.span_since(&start),
        ))
    })
);

simple_parser_fn!(literal -> &'tok SToken<'src>, "literal",
    custom(|input| {
        let start = input.cursor();
        let next: Option<&SToken<'src>> = input.next_ref();

        if let Some(found) = next {
            match found.token {
                Token::Num(_) | Token::Str(_) | Token::Bool(_) | Token::None => {
                    return Ok(found);
                }
                _ => {}
            }
        }

        Err(LabelError::<I, _>::expected_found(
            vec!["literal".to_string()],
            next.map(|x| x.into()),
            input.span_since(&start),
        ))
    })
);

fn listing<'src: 'tok, 'tok, I, O: 'tok, ItemParser>(
    begin: &'static str,
    end: &'static str,
    optional_separator: Token<'src>,
    item_parser: ItemParser,
) -> impl Parser<'tok, I, Listing<O, STree<'src, 'tok>>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
    ItemParser: Parser<'tok, I, O, TExtra<'src, 'tok>> + Clone + 'tok,
{
    let item_parser1 = item_parser.clone();
    let item_parser2 = item_parser;

    let sep_parser1 = token(optional_separator.clone());
    let sep_parser2 = token(optional_separator);

    choice((
        // Block listing (with indent/dedent)
        group((
            symbol(begin),
            token(Token::Indent),
            custom(move |input| {
                let mut acc = vec![];
                loop {
                    let before_item = input.save();
                    if let Ok(item) = input.parse(&item_parser1) {
                        let mut separator = None;
                        let before_sep = input.save();

                        if let Ok(sep) = input.parse(&sep_parser1) {
                            separator = Some(sep);
                        } else {
                            input.rewind(before_sep);
                        }

                        let mut newline = None;
                        let before_newl = input.save();

                        if let Ok(newl) = input.parse(token(Token::Eol)) {
                            newline = Some(newl);
                        } else {
                            input.rewind(before_newl);
                        }

                        acc.push(ListingItem {
                            item,
                            separator,
                            newline,
                        });

                        if newline.is_none() && separator.is_none() {
                            break;
                        }
                    } else {
                        input.rewind(before_item);
                        break;
                    }
                }

                Ok(acc)
            }),
            token(Token::Dedent),
            token(Token::Eol).or_not(),
            symbol(end),
        ))
        .map(|(begin, indent, items, dedent, newline, end)| Listing {
            begin,
            indent: Some(indent),
            items,
            newline,
            dedent: Some(dedent),
            end,
        })
        .labelled("block listing"),
        // Inline listing
        group((
            symbol(begin),
            custom(move |input| {
                let mut acc = vec![];
                loop {
                    let before_item = input.save();
                    if let Ok(item) = input.parse(&item_parser2) {
                        let mut separator = None;
                        let before_sep = input.save();

                        if let Ok(sep) = input.parse(&sep_parser2) {
                            separator = Some(sep);
                        } else {
                            input.rewind(before_sep);
                        }

                        acc.push(ListingItem {
                            item,
                            separator,
                            newline: None,
                        });

                        if separator.is_none() {
                            break;
                        }
                    } else {
                        input.rewind(before_item);
                        break;
                    }
                }

                Ok(acc)
            }),
            symbol(end),
        ))
        .map(|(begin, items, end)| Listing {
            begin,
            indent: None,
            items,
            newline: None,
            dedent: None,
            end,
        })
        .labelled("inline listing"),
    ))
    .labelled("listing")
    .boxed()
}

// F-string expression parser
simple_parser_fn!(fstr_expr -> SExpr<'src, 'tok>, "f-string expression",
    custom(|input| {
        let start = input.cursor();
        let next: Option<&SToken<'src>> = input.next_ref();

        if let Some(found) = next {
            if let Token::FstrBegin(_) = found.token {
                // For now, just parse f-string as a literal
                // TODO: Implement proper f-string parsing with expressions
                return Ok(SExprInner::Literal { token: found }.spanned(input.span_since(&start)));
            }
        }

        Err(LabelError::<I, _>::expected_found(
            vec!["f-string".to_string()],
            next.map(|x| x.into()),
            input.span_since(&start),
        ))
    })
);

// Basic expression parsers using macros
simple_parser_fn!(literal_expr -> SExpr<'src, 'tok>, "literal expression",
    literal().map_with(|token, e| SExprInner::Literal { token }.spanned(e.span()))
);

simple_parser_fn!(ident_expr -> SExpr<'src, 'tok>, "identifier expression",
    any_ident().map_with(|token, e| SExprInner::Ident { token }.spanned(e.span()))
);

simple_parser_fn!(placeholder_macro -> SExpr<'src, 'tok>, "placeholder",
    symbol("$").map_with(|dollar, e| SExprInner::Placeholder { dollar }.spanned(e.span()))
);

// Basic pattern parsers using macros
simple_parser_fn!(literal_pattern -> SPattern<'src, 'tok>, "literal pattern",
    literal().map_with(|token, e| SPatternInner::Literal { token }.spanned(e.span()))
);

simple_parser_fn!(capture_pattern -> SPattern<'src, 'tok>, "capture pattern",
    choice((
        symbol("_").map_with(|_, e| SPatternInner::Capture { name: None }.spanned(e.span())),
        any_ident().map_with(|name, e| SPatternInner::Capture { name: Some(name) }.spanned(e.span())),
    ))
);

// Expression parsers that take other parsers as parameters
composite_parser_fn!(list_item<expr_parser: ExprParser: SExpr<'src, 'tok>> -> SListItem<'src, 'tok>, "list item",
    choice((
        symbol("*")
            .then(expr_parser.clone())
            .map(|(star, expr)| SListItem::Spread {
                star,
                expr: expr.boxed(),
            }),
        expr_parser.map(|expr| SListItem::Item { expr: expr.boxed() }),
    ))
);

composite_parser_fn!(list_expr<expr_parser: ExprParser: SExpr<'src, 'tok>> -> SExpr<'src, 'tok>, "list expression",
    listing("[", "]", Token::Symbol(","), list_item(expr_parser))
        .map_with(|listing, e| SExprInner::List { listing }.spanned(e.span()))
);

composite_parser_fn!(pattern_sequence_item<pattern_parser: PatternParser: SPattern<'src, 'tok>> -> PatternSequenceItem<STree<'src, 'tok>>, "pattern sequence item",
    choice((
        symbol("*")
            .then(any_ident().or_not())
            .map(|(star, name)| PatternSequenceItem::Spread { star, name }),
        pattern_parser.map(|pattern| PatternSequenceItem::Item {
            pattern: Box::new(pattern),
        }),
    ))
);

// Sequence pattern parser
pub fn sequence_pattern<'src: 'tok, 'tok, I, PatternParser>(
    pattern_parser: PatternParser,
) -> impl Parser<'tok, I, SPattern<'src, 'tok>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
    PatternParser: Parser<'tok, I, SPattern<'src, 'tok>, TExtra<'src, 'tok>> + Clone + 'tok,
{
    choice((
        // Bracket sequence
        listing(
            "[",
            "]",
            Token::Symbol(","),
            pattern_sequence_item(pattern_parser.clone()),
        )
        .map_with(|listing, e| {
            SPatternInner::Sequence {
                lbracket: listing.begin,
                listing: Listing {
                    begin: listing.begin,
                    indent: listing.indent,
                    items: listing.items,
                    newline: listing.newline,
                    dedent: listing.dedent,
                    end: listing.end,
                },
                rbracket: listing.end,
            }
            .spanned(e.span())
        })
        .labelled("sequence pattern"),
        // Tuple sequence
        listing(
            "(",
            ")",
            Token::Symbol(","),
            pattern_sequence_item(pattern_parser),
        )
        .map_with(|listing, e| {
            SPatternInner::TupleSequence {
                lparen: listing.begin,
                listing: Listing {
                    begin: listing.begin,
                    indent: listing.indent,
                    items: listing.items,
                    newline: listing.newline,
                    dedent: listing.dedent,
                    end: listing.end,
                },
                rparen: listing.end,
            }
            .spanned(e.span())
        })
        .labelled("tuple sequence pattern"),
    ))
    .labelled("sequence pattern")
}

// Mapping item parsers
composite_parser_fn!(mapping_item<expr_parser: ExprParser: SExpr<'src, 'tok>> -> SMappingItem<'src, 'tok>, "mapping item",
    choice((
        symbol("**")
            .then(expr_parser.clone())
            .map(|(stars, expr)| SMappingItem::Spread {
                stars,
                expr: expr.boxed(),
            }),
        group((expr_parser.clone(), symbol(":"), expr_parser.clone())).map(
            |(key, colon, value)| SMappingItem::Item {
                key: key.boxed(),
                colon,
                value: value.boxed(),
            },
        ),
        any_ident().map(|ident| SMappingItem::Ident { ident }),
    ))
);

// Mapping expression parser
composite_parser_fn!(mapping_expr<expr_parser: ExprParser: SExpr<'src, 'tok>> -> SExpr<'src, 'tok>, "mapping expression",
    listing("{", "}", Token::Symbol(","), mapping_item(expr_parser))
        .map_with(|listing, e| SExprInner::Mapping { listing }.spanned(e.span()))
);

// Unary expression parser
composite_parser_fn!(unary_expr<expr_parser: ExprParser: SExpr<'src, 'tok>> -> SExpr<'src, 'tok>, "unary expression",
    choice((symbol("@"), symbol("+"), symbol("-"), symbol("~")))
        .repeated()
        .foldr_with(expr_parser, |op, expr, e| {
            SExprInner::Unary {
                op,
                expr: expr.boxed(),
            }
            .spanned(e.span())
        })
);

// Binary expression parser
pub fn binary_expr<'src: 'tok, 'tok, I, LhsParser, OpParser>(
    lhs_parser: LhsParser,
    op_parser: OpParser,
    right_assoc: bool,
) -> impl Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
    LhsParser: Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone + 'tok,
    OpParser: Parser<'tok, I, &'tok SToken<'src>, TExtra<'src, 'tok>> + Clone + 'tok,
{
    if !right_assoc {
        lhs_parser
            .clone()
            .foldl_with(
                op_parser.then(lhs_parser).repeated(),
                |lhs, (op, rhs), e| {
                    SExprInner::Binary {
                        lhs: lhs.boxed(),
                        op,
                        rhs: rhs.boxed(),
                    }
                    .spanned(e.span())
                },
            )
            .boxed()
    } else {
        recursive(|bin| {
            lhs_parser
                .clone()
                .then(op_parser.then(bin.or(lhs_parser.clone())).or_not())
                .map_with(|(lhs, matched), e| {
                    if let Some((op, rhs)) = matched {
                        SExprInner::Binary {
                            lhs: lhs.boxed(),
                            op,
                            rhs: rhs.boxed(),
                        }
                        .spanned(e.span())
                    } else {
                        lhs
                    }
                })
        })
        .boxed()
    }
}

// Call item parsers
composite_parser_fn!(call_item<expr_parser: ExprParser: SExpr<'src, 'tok>> -> SCallItem<'src, 'tok>, "call item",
    choice((
        symbol("**")
            .then(expr_parser.clone())
            .map(|(stars, expr)| SCallItem::KwargSpread {
                stars,
                expr: expr.boxed(),
            }),
        symbol("*")
            .then(expr_parser.clone())
            .map(|(star, expr)| SCallItem::ArgSpread {
                star,
                expr: expr.boxed(),
            }),
        group((any_ident(), symbol("="), expr_parser.clone())).map(|(name, eq, expr)| {
            SCallItem::Kwarg {
                name,
                eq,
                expr: expr.boxed(),
            }
        }),
        expr_parser.map(|expr| SCallItem::Arg { expr: expr.boxed() }),
    ))
);

simple_parser_fn!(placeholder -> SExpr<'src, 'tok>, "placeholder",
    symbol("$").map_with(|dollar, e| SExprInner::Placeholder { dollar }.spanned(e.span()))
);

// Parenthesized expression
composite_parser_fn!(parenthesized<expr_parser: ExprParser: SExpr<'src, 'tok>> -> SExpr<'src, 'tok>, "parenthesized expression",
    group((symbol("("), expr_parser, symbol(")")))
        .map_with(|(lparen, expr, rparen), e| {
            SExprInner::Parenthesized {
                lparen,
                expr: expr.boxed(),
                rparen,
            }
            .spanned(e.span())
        })
);

// Simple call expression (func followed by args)
pub fn call_expr<'src: 'tok, 'tok, I, FuncParser>(
    func_parser: FuncParser,
) -> impl Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
    FuncParser: Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone + 'tok,
{
    recursive(|expr| {
        group((
            func_parser.clone(),
            listing("(", ")", Token::Symbol(","), call_item(expr.clone())),
        ))
        .map_with(|(func, args_listing), e| {
            let lparen = args_listing.begin;
            let rparen = args_listing.end;
            SExprInner::Call {
                func: func.boxed(),
                lparen,
                args: args_listing,
                rparen,
            }
            .spanned(e.span())
        })
    })
    .labelled("call expression")
}

// Value pattern (dot-prefixed qualified identifier)
pub fn value_pattern<'src: 'tok, 'tok, I, ExprParser>(
    expr_parser: ExprParser,
) -> impl Parser<'tok, I, SPattern<'src, 'tok>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
    ExprParser: Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone + 'tok,
{
    group((symbol("."), expr_parser))
        .map_with(|(dot, expr), e| {
            SPatternInner::Value {
                dot,
                expr: expr.boxed(),
            }
            .spanned(e.span())
        })
        .labelled("value pattern")
}

// Helper function to create binary operation parsers with precedence
pub fn make_binary_op<'src: 'tok, 'tok, I, LhsParser, OpParser>(
    lhs_parser: LhsParser,
    op_parser: OpParser,
    right_assoc: bool,
) -> impl Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
    LhsParser: Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone + 'tok,
    OpParser: Parser<'tok, I, &'tok SToken<'src>, TExtra<'src, 'tok>> + Clone + 'tok,
{
    if !right_assoc {
        lhs_parser
            .clone()
            .foldl_with(
                op_parser.then(lhs_parser).repeated(),
                |lhs, (op, rhs), e| {
                    SExprInner::Binary {
                        lhs: lhs.boxed(),
                        op,
                        rhs: rhs.boxed(),
                    }
                    .spanned(e.span())
                },
            )
            .boxed()
    } else {
        recursive(|bin| {
            lhs_parser
                .clone()
                .then(op_parser.then(bin.or(lhs_parser.clone())).or_not())
                .map_with(|(lhs, matched), e| {
                    if let Some((op, rhs)) = matched {
                        SExprInner::Binary {
                            lhs: lhs.boxed(),
                            op,
                            rhs: rhs.boxed(),
                        }
                        .spanned(e.span())
                    } else {
                        lhs
                    }
                })
        })
        .boxed()
    }
}

// Helper function to create binary operation parsers with precedence

// Dot access expression
pub fn dot_expr<'src: 'tok, 'tok, I, LhsParser>(
    lhs_parser: LhsParser,
) -> impl Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
    LhsParser: Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone + 'tok,
{
    lhs_parser
        .foldl_with(
            symbol(".").then(any_ident()).repeated(),
            |lhs, (dot, field), e| {
                SExprInner::Attribute {
                    expr: lhs.boxed(),
                    dot,
                    attr: field,
                }
                .spanned(e.span())
            },
        )
        .labelled("dot expression")
}

// Index access expression
pub fn index_expr<'src: 'tok, 'tok, I, LhsParser>(
    lhs_parser: LhsParser,
) -> impl Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
    LhsParser: Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone + 'tok,
{
    recursive(|expr| {
        lhs_parser.foldl_with(
            symbol("[")
                .then(listing(
                    "[",
                    "]",
                    Token::Symbol(","),
                    list_item(expr.clone()),
                ))
                .then(symbol("]"))
                .repeated(),
            |lhs, ((lbracket, indices), rbracket), e| {
                SExprInner::Subscript {
                    expr: lhs.boxed(),
                    lbracket,
                    indices,
                    rbracket,
                }
                .spanned(e.span())
            },
        )
    })
    .labelled("index expression")
}

// Main expression parser with precedence
pub fn parser<'src: 'tok, 'tok, I>()
-> impl Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
{
    recursive(|expr| {
        // Basic atom expressions (highest precedence statements)
        let atom = choice((
            literal_expr(),
            ident_expr(),
            placeholder_macro(),
            fstr_expr(),
            parenthesized(expr.clone()),
            list_expr(expr.clone()),
            mapping_expr(expr.clone()),
            classic_if_expr(expr.clone()),
            control_expr(expr.clone()),
            memo_expr(expr.clone()),
        ))
        .labelled("atom");

        // Handle parenthesized expressions and tuples
        let tuples_and_parens = choice((
            atom.clone(),
            symbol("(")
                .then(symbol(")"))
                .map_with(|(lparen, rparen), e| {
                    // Create a dummy listing for empty tuple
                    let empty_listing = Listing {
                        begin: lparen,
                        indent: None,
                        items: vec![],
                        newline: None,
                        dedent: None,
                        end: rparen,
                    };
                    SExprInner::Tuple {
                        listing: empty_listing,
                    }
                    .spanned(e.span())
                }),
            symbol("(")
                .then(list_item(expr.clone()))
                .then(symbol(")"))
                .map_with(|((lparen, item), rparen), e| {
                    SExprInner::Parenthesized {
                        lparen,
                        expr: match item {
                            SListItem::Item { expr } => expr,
                            _ => {
                                // If it's not a simple item, wrap it in a tuple
                                let listing = Listing {
                                    begin: lparen,
                                    indent: None,
                                    items: vec![ListingItem {
                                        item,
                                        separator: None,
                                        newline: None,
                                    }],
                                    newline: None,
                                    dedent: None,
                                    end: rparen,
                                };
                                Box::new(SExprInner::Tuple { listing }.spanned(e.span()))
                            }
                        },
                        rparen,
                    }
                    .spanned(e.span())
                }),
        ))
        .labelled("atom with tuples");

        // Postfix operators: calls, dot access, indexing
        let postfix = choice((
            call_expr(tuples_and_parens.clone()),
            // Note: dot_expr and index_expr use wrong variant names, need to be fixed
            tuples_and_parens,
        ))
        .labelled("postfix");

        // Unary operators
        let unary = unary_expr(postfix).labelled("unary");

        // Binary operators with precedence (from highest to lowest)

        // Level 0: Exponentiation (**) - right associative
        let binary0 = make_binary_op(unary, symbol("**"), true);

        // Level 1: Multiplicative (*, /, //, %, @) - left associative
        let binary1 = make_binary_op(
            binary0,
            choice((
                symbol("*"),
                symbol("//"),
                symbol("/"),
                symbol("%"),
                symbol("@"),
            )),
            false,
        );

        // Level 2: Additive (+, -) - left associative
        let binary2 = make_binary_op(binary1, choice((symbol("+"), symbol("-"))), false);

        // Level 3: Comparison operators - left associative
        let binary3 = make_binary_op(
            binary2,
            choice((
                symbol("<="),
                symbol(">="),
                symbol("==="),
                symbol("<=>"),
                symbol("=="),
                symbol("<>"),
                symbol("<"),
                symbol(">"),
                kw("in"),
            )),
            false,
        );

        // Level 4: Logical NOT
        let not_expr = choice((
            kw("not").then(expr.clone()).map_with(|(not_kw, expr), e| {
                SExprInner::Unary {
                    op: not_kw,
                    expr: expr.boxed(),
                }
                .spanned(e.span())
            }),
            binary3,
        ))
        .labelled("not expression");

        // Level 5: Logical AND
        let binary5 = make_binary_op(not_expr, kw("and"), false);

        // Level 6: Logical OR
        let binary6 = make_binary_op(binary5, kw("or"), false);

        // Level 7: Inline If (ternary-like, lowest precedence)
        let inline_if =
            choice((inline_if_expr(binary6.clone()), binary6)).labelled("inline if expression");

        inline_if
    })
    .labelled("expression")
}

// Basic statement parsers to match old grammar
simple_parser_fn!(while_stmt(expr: impl Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone + 'tok) -> SStmt<'src, 'tok>, "while statement",
    kw("while")
        .then(expr.clone())
        .then(token(Token::Indent))
        .then(expr.clone())
        .then(token(Token::Dedent))
        .map_with(|((((while_kw, cond), _indent), body), _dedent), e| {
            SStmtInner::While {
                while_kw,
                cond: cond.boxed(),
                colon: while_kw, // Placeholder - should parse actual colon
                body: body.boxed(),
            }.spanned(e.span())
        })
);

simple_parser_fn!(return_stmt(expr: impl Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone + 'tok) -> SStmt<'src, 'tok>, "return statement",
    kw("return")
        .then(expr)
        .map_with(|(return_kw, value), e| {
            SStmtInner::Return {
                return_kw,
                expr: value.boxed(),
            }.spanned(e.span())
        })
);

simple_parser_fn!(break_stmt -> SStmt<'src, 'tok>, "break statement",
    kw("break").map_with(|break_kw, e| {
        SStmtInner::Break { break_kw }.spanned(e.span())
    })
);

simple_parser_fn!(continue_stmt -> SStmt<'src, 'tok>, "continue statement",
    kw("continue").map_with(|continue_kw, e| {
        SStmtInner::Continue { continue_kw }.spanned(e.span())
    })
);

// Statement parser that combines expressions and statements
pub fn statement<'src: 'tok, 'tok, I>()
-> impl Parser<'tok, I, SStmt<'src, 'tok>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
{
    choice((
        while_stmt(parser()),
        return_stmt(parser()),
        break_stmt(),
        continue_stmt(),
        // Expression statement (fallback)
        parser().map_with(|expr, e| SStmtInner::Expr { expr: expr.boxed() }.spanned(e.span())),
    ))
    .labelled("statement")
}

// Classic if expression - higher precedence (statement-like)
pub fn classic_if_expr<'src: 'tok, 'tok, I, ExprParser>(
    expr_parser: ExprParser,
) -> impl Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
    ExprParser: Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone + 'tok,
{
    // Classic if: if cond: body else: body
    group((
        kw("if"),
        expr_parser.clone(),
        symbol(":"),
        expr_parser.clone(),
        group((kw("else"), symbol(":"), expr_parser.clone())).or_not(),
    ))
    .map_with(|(if_kw, cond, colon, body, else_clause), e| {
        let else_clause = else_clause
            .map(|(else_kw, else_colon, else_body)| (else_kw, else_colon, else_body.boxed()));

        SExprInner::ClassicIf {
            if_kw,
            cond: cond.boxed(),
            colon,
            body: body.boxed(),
            else_clause,
        }
        .spanned(e.span())
    })
    .labelled("classic if expression")
}

// Inline if expression - lower precedence (ternary-like)
pub fn inline_if_expr<'src: 'tok, 'tok, I, ExprParser>(
    expr_parser: ExprParser,
) -> impl Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
    ExprParser: Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone + 'tok,
{
    // Inline if: cond then expr else expr
    group((
        expr_parser.clone(),
        kw("then"),
        symbol(":").or_not(),
        expr_parser.clone(),
        group((token(Token::Eol).or_not(), kw("else"), symbol(":").or_not()))
            .then(expr_parser.clone())
            .or_not(),
    ))
    .map_with(|(cond, then_kw, then_colon, then_expr, else_clause), e| {
        let else_clause = else_clause.map(|((_eol, else_kw, else_colon), else_expr)| {
            (else_kw, else_colon, else_expr.boxed())
        });

        SExprInner::If {
            cond: cond.boxed(),
            then_kw,
            then_clause: (then_kw, then_colon, then_expr.boxed()),
            else_clause,
        }
        .spanned(e.span())
    })
    .labelled("inline if expression")
}

// Await/Yield expressions
pub fn control_expr<'src: 'tok, 'tok, I, ExprParser>(
    expr_parser: ExprParser,
) -> impl Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
    ExprParser: Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone + 'tok,
{
    choice((
        kw("await")
            .then(expr_parser.clone())
            .map_with(|(await_kw, expr), e| {
                SExprInner::Await {
                    await_kw,
                    expr: expr.boxed(),
                }
                .spanned(e.span())
            }),
        kw("yield")
            .then(ident("from").or_not())
            .then(expr_parser.clone())
            .map_with(|((yield_kw, from_kw), expr), e| {
                if let Some(from_kw) = from_kw {
                    SExprInner::YieldFrom {
                        yield_kw,
                        from_kw,
                        expr: expr.boxed(),
                    }
                    .spanned(e.span())
                } else {
                    SExprInner::Yield {
                        yield_kw,
                        expr: expr.boxed(),
                    }
                    .spanned(e.span())
                }
            }),
    ))
    .labelled("control expression")
}

// Memo expressions
pub fn memo_expr<'src: 'tok, 'tok, I, ExprParser>(
    expr_parser: ExprParser,
) -> impl Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
    ExprParser: Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone + 'tok,
{
    choice((
        group((
            ident("async"),
            ident("memo"),
            symbol(":").or_not(),
            expr_parser.clone(),
        ))
        .map_with(|(async_kw, memo_kw, colon, expr), e| {
            SExprInner::Memo {
                async_kw: Some(async_kw),
                memo_kw,
                colon,
                expr: expr.boxed(),
            }
            .spanned(e.span())
        }),
        group((ident("memo"), symbol(":").or_not(), expr_parser.clone())).map_with(
            |(memo_kw, colon, expr), e| {
                SExprInner::Memo {
                    async_kw: None,
                    memo_kw,
                    colon,
                    expr: expr.boxed(),
                }
                .spanned(e.span())
            },
        ),
    ))
    .labelled("memo expression")
}

// Class pattern parser
pub fn class_pattern<'src: 'tok, 'tok, I, ExprParser>(
    expr_parser: ExprParser,
) -> impl Parser<'tok, I, SPattern<'src, 'tok>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
    ExprParser: Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone + 'tok,
{
    recursive(|pattern| {
        let class_item = choice((
            group((any_ident(), symbol("="), pattern.clone())).map(|(name, eq, pattern)| {
                PatternClassItem::Kw {
                    name,
                    eq,
                    pattern: Box::new(pattern),
                }
            }),
            pattern.clone().map(|pattern| PatternClassItem::Item {
                pattern: Box::new(pattern),
            }),
        ));

        group((
            expr_parser.clone(),
            listing("(", ")", Token::Symbol(","), class_item),
        ))
        .map_with(|(class, args), e| {
            let lparen = args.begin;
            let rparen = args.end;
            SPatternInner::Class {
                expr: class.boxed(),
                lparen,
                items: args,
                rparen,
            }
            .spanned(e.span())
        })
    })
    .labelled("class pattern")
}

// Or pattern parser
pub fn or_pattern<'src: 'tok, 'tok, I, PatternParser>(
    pattern_parser: PatternParser,
) -> impl Parser<'tok, I, SPattern<'src, 'tok>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
    PatternParser: Parser<'tok, I, SPattern<'src, 'tok>, TExtra<'src, 'tok>> + Clone + 'tok,
{
    pattern_parser
        .clone()
        .separated_by(
            symbol("|")
                .then(token(Token::Eol).or_not())
                .map(|(pipe, _)| pipe),
        )
        .at_least(1)
        .collect::<Vec<_>>()
        .map_with(|patterns, e| {
            if patterns.len() == 1 {
                patterns.into_iter().next().unwrap()
            } else {
                SPatternInner::Or {
                    patterns: patterns.into_iter().map(|p| (Box::new(p), None)).collect(),
                }
                .spanned(e.span())
            }
        })
        .labelled("or pattern")
}

// As pattern parser
pub fn as_pattern<'src: 'tok, 'tok, I, PatternParser>(
    pattern_parser: PatternParser,
) -> impl Parser<'tok, I, SPattern<'src, 'tok>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
    PatternParser: Parser<'tok, I, SPattern<'src, 'tok>, TExtra<'src, 'tok>> + Clone + 'tok,
{
    group((pattern_parser, kw("as").then(any_ident()).or_not()))
        .map_with(|(pattern, as_clause), e| {
            if let Some((as_kw, name)) = as_clause {
                SPatternInner::As {
                    pattern: Box::new(pattern),
                    as_kw,
                    name,
                }
                .spanned(e.span())
            } else {
                pattern
            }
        })
        .labelled("as pattern")
}

// Match expression parser
pub fn match_expr<'src: 'tok, 'tok, I, LhsParser, PatternParser>(
    lhs_parser: LhsParser,
    pattern_parser: PatternParser,
) -> IP!(SExpr<'src, 'tok>)
where
    LhsParser: AParser<SExpr<'src, 'tok>>,
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
    LhsParser: Parser<'tok, I, SExpr<'src, 'tok>, TExtra<'src, 'tok>> + Clone + 'tok,
    PatternParser: Parser<'tok, I, SPattern<'src, 'tok>, TExtra<'src, 'tok>> + Clone + 'tok,
{
    let case_item = group((
        pattern_parser.clone(),
        kw("if").then(expr.clone()).or_not(),
        symbol("=>").then(expr.clone()),
    ))
    .map(|(pattern, guard, (arrow, body))| SMatchCase {
        pattern: Some(Box::new(pattern)),
        guard: guard.map(|(_, g)| g.boxed()),
        arrow,
        body: body.boxed(),
    });

    group((
        kw("match"),
        lhs_parser,
        symbol(":").or_not(),
        choice((
            listing("", "", Token::Eol, choice((case_item, default_case)))
                .map(|cases| cases.items.into_iter().map(|item| item.item).collect()),
            // Single inline case
            case_item.map(|case| vec![case]),
        )),
    ))
    .map_with(|(match_kw, lhs, colon, cases), e| {
        SExprInner::Match {
            match_kw,
            scrutinee: lhs.boxed(),
            colon,
            cases,
        }
        .spanned(e.span())
    })
    .labelled("match expression")
}

// Try expression parser
composite_parser_fn!(try_expr<expr_parser: ExprParser: SExpr<'src, 'tok>> -> SExpr<'src, 'tok>, "try expression", {
    kw("try")
        .then(expr_parser)
        .map_with(|(try_kw, expr), e| {
            SExprInner::Checked {
                try_kw,
                expr: expr.boxed(),
                except_kw: None,
                pattern: None,
            }
            .spanned(e.span())
        })
});

// Lambda expression parser - removed due to CST complexity
// Use function expressions with explicit syntax instead

// Enhanced pattern parser with all pattern types
pub fn pattern_parser<'src: 'tok, 'tok, I>()
-> impl Parser<'tok, I, SPattern<'src, 'tok>, TExtra<'src, 'tok>> + Clone
where
    I: BorrowInput<'tok, Token = SToken<'src>, Span = Span>,
{
    recursive(|pattern| {
        let base_pattern = choice((
            literal_pattern(),
            capture_pattern(),
            value_pattern(ident_expr()),
            sequence_pattern(pattern.clone()),
            class_pattern(ident_expr()),
        ));

        // Apply as patterns
        let as_pattern = as_pattern(base_pattern);

        // Apply or patterns
        let or_pattern = or_pattern(as_pattern);

        or_pattern
    })
    .labelled("pattern")
}

// Test function - updated to use new parser
pub fn parse_tokens<'src: 'tok, 'tok>(
    src: &'src str,
    tokens: &'tok TokenList<'src>,
) -> (Option<SExpr<'src, 'tok>>, Vec<TriviaRich<'tok, 'src>>) {
    let input = tokens
        .0
        .as_slice()
        .map((src.len()..src.len()).into(), |tok| (tok, &tok.span));

    let p = group((
        token(Token::Indent),
        parser(),
        token(Token::Eol),
        token(Token::Dedent),
    ))
    .map(|(_, expr, _, _)| expr);

    p.parse(input).into_output_errors()
}
