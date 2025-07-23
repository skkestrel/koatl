pub mod linecol;
pub mod parser;
pub mod py;
pub mod transform;

use parser::ast::{Block, Span};
use parser::{TokenList, parse_tokens, tokenize};

use crate::py::ast::{PyAccessCtx, PyImportAlias, PyListItem, PyLiteral};
use crate::py::util::PyAstBuilder;
use crate::py::{ast::PyBlock, emit::EmitCtx};
use crate::transform::transform_ast;
use ariadne::{Color, Label, Report, ReportKind, sources};

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

pub struct TranspileOptions {
    pub inject_prelude: bool,
    pub inject_runtime: bool,
    pub set_exports: bool,
}

impl TranspileOptions {
    pub fn script() -> Self {
        TranspileOptions {
            inject_prelude: true,
            inject_runtime: true,
            set_exports: false,
        }
    }

    pub fn interactive() -> Self {
        let mut opt = TranspileOptions::script();
        opt.inject_prelude = false;
        opt.inject_runtime = false;
        opt
    }

    pub fn module() -> Self {
        TranspileOptions {
            inject_prelude: true,
            inject_runtime: true,
            set_exports: true,
        }
    }

    pub fn prelude() -> Self {
        let mut opt = TranspileOptions::module();
        opt.inject_prelude = false; // don't inject the prelude when loading prelude
        opt
    }
}

pub fn transpile_to_py_ast<'src>(
    src: &'src str,
    options: TranspileOptions,
) -> TlResult<PyBlock<'src>> {
    let tl_ast = parse_tl(src)?;

    let output = transform_ast(&src, &tl_ast).map_err(|e| {
        e.0.into_iter()
            .map(|e| TlErr {
                kind: TlErrKind::Transform,
                message: e.message,
                span: e.span,
                contexts: vec![],
            })
            .collect::<Vec<_>>()
    })?;

    let mut py_ast = output.py_block;

    let a = PyAstBuilder::new(Span {
        start: 0,
        end: 0,
        context: (),
    });

    if options.inject_prelude {
        py_ast.0.insert(
            0,
            a.import_from(
                Some("koatl.prelude".into()),
                vec![PyImportAlias {
                    name: "*".into(),
                    as_name: None,
                }],
                0,
            ),
        );
    }

    if options.inject_runtime {
        py_ast.0.insert(
            0,
            a.import_from(
                Some("koatl.runtime".into()),
                vec![PyImportAlias {
                    name: "*".into(),
                    as_name: None,
                }],
                0,
            ),
        );
    }

    if options.set_exports {
        py_ast.0.push(a.expr(a.call(
            a.load_ident("_set_exports"),
            vec![
                    a.call_arg(a.load_ident("__package__")),
                    a.call_arg(a.call(a.load_ident("globals"), vec![])),
                    a.call_arg(
                        a.tuple(
                            output
                                .exports
                                .iter()
                                .map(|x| {
                                    PyListItem::Item(a.literal(PyLiteral::Str(x.clone())))
                                })
                                .collect(),
                            PyAccessCtx::Load
                        ),
                    ),
                    a.call_arg(
                        a.tuple(
                            output
                                .module_star_exports
                                .iter()
                                .map(|x| {
                                    PyListItem::Item(a.literal(PyLiteral::Str(x.clone())))
                                })
                                .collect(),
                            PyAccessCtx::Load
                        ),
                ),
            ],
        )));
    }

    Ok(py_ast)
}

pub fn transpile(src: &str, options: TranspileOptions) -> TlResult<EmitCtx> {
    let mut py_ast = transpile_to_py_ast(src, options)?;

    let mut ctx = EmitCtx::new();
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

    Ok(ctx)
}

pub fn format_errs(errs: &[TlErr], filename: &str, src: &str) -> Vec<u8> {
    let filename = filename.to_string();
    let mut writer = Vec::<u8>::new();

    for e in errs {
        let range = e.span.map(|e| e.into_range()).unwrap_or(0..0);

        Report::build(ReportKind::Error, (filename.clone(), range.clone()))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message(&e.message)
            .with_label(
                Label::new((filename.clone(), range.clone()))
                    .with_message(e.message.clone())
                    .with_color(Color::Red),
            )
            .with_labels(e.contexts.iter().map(|(label, span)| {
                Label::new((filename.clone(), span.into_range()))
                    .with_message(format!("while parsing this {label}"))
                    .with_color(Color::Yellow)
            }))
            .finish()
            .write(sources([(filename.clone(), src)]), &mut writer)
            .unwrap();
    }

    writer
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
