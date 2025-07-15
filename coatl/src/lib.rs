pub mod parser;
pub mod py;
pub mod transform;

use parser::ast::{Block, Span};

use crate::py::{ast::PyBlock, emit::EmitCtx};
use crate::transform::transform_ast;
use parser::{TokenList, parse_tokens, tokenize};

pub enum TlErrKind {
    Parse,
    Transform,
    Emit,
}

pub struct TlErr {
    pub kind: TlErrKind,
    pub message: String,
    pub span: Option<Span>,
    pub contexts: Vec<(String, Span)>,
}

pub type TlResult<T> = Result<T, Vec<TlErr>>;

pub fn transpile_to_py_ast<'src>(src: &'src str) -> TlResult<PyBlock<'src>> {
    let tl_ast = parse_tl(src)?;

    let py_ast: PyBlock<'src> = transform_ast(&src, &tl_ast).map_err(|e| {
        e.0.into_iter()
            .map(|e| TlErr {
                kind: TlErrKind::Transform,
                message: e.message,
                span: e.span,
                contexts: vec![],
            })
            .collect::<Vec<_>>()
    })?;

    Ok(py_ast)
}

pub fn transpile(src: &str) -> TlResult<String> {
    let mut py_ast = transpile_to_py_ast(src)?;

    let mut ctx = EmitCtx {
        indentation: 0,
        source: String::new(),
    };
    py_ast.emit_to(&mut ctx, 0).map_err(|e| {
        e.0.into_iter()
            .map(|e| TlErr {
                kind: TlErrKind::Emit,
                message: e.message,
                span: e.span,
                contexts: vec![],
            })
            .collect::<Vec<_>>()
    })?;

    Ok(ctx.source)
}

pub fn parse_tl<'src>(src: &'src str) -> TlResult<::parser::ast::SBlock<'src>> {
    let mut errs = vec![];

    let (tokens, token_errs) = tokenize(&src);
    errs.extend(token_errs.into_iter().map(|e| {
        TlErr {
            kind: TlErrKind::Parse,
            message: e.reason().to_string(),
            span: Some(*e.span()),
            contexts: e
                .contexts()
                .map(|(label, span)| (label.to_string(), *span))
                .collect(),
        }
    }));

    let tokens: TokenList<'src> = match tokens {
        Some(tokens) => tokens,
        None => return Err(errs),
    };
    // println!("tokens: {tokens}");

    let (tl_ast, parser_errs) = parse_tokens(&src, &tokens);
    errs.extend(parser_errs.into_iter().map(|e| {
        TlErr {
            kind: TlErrKind::Parse,
            message: e.reason().to_string(),
            span: Some(*e.span()),
            contexts: e
                .contexts()
                .map(|(label, span)| (label.to_string(), *span))
                .collect(),
        }
    }));

    let tl_ast: (Block<'src>, Span) = tl_ast.ok_or_else(|| errs)?;
    // println!("AST: {ast:?}");

    Ok(tl_ast)
}
