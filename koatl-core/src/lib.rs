mod inference;
pub mod parser;
pub mod py;
mod resolve_scopes;
pub mod transform;
mod types;
pub mod util;

use ::parser::ast::SExpr;
use parser::ast::Span;
use parser::{TokenList, parse_tokens, tokenize};

use crate::py::ast::{PyAccessCtx, PyImportAlias, PyListItem, PyLiteral};
use crate::py::util::PyAstBuilder;
use crate::py::{ast::PyBlock, emit::EmitCtx};
use crate::transform::transform_ast;
use crate::util::{TlErr, TlErrKind, TlErrs, TlResult};
use ariadne::{Color, Label, Report, ReportKind, sources};

pub struct TranspileOptions {
    pub inject_prelude: bool,
    pub inject_runtime: bool,
    pub set_exports: bool,
    pub allow_await: bool,
    pub interactive: bool,
}

impl TranspileOptions {
    pub fn script() -> Self {
        TranspileOptions {
            inject_prelude: true,
            inject_runtime: true,
            interactive: false,
            set_exports: false,
            allow_await: false,
        }
    }

    pub fn interactive() -> Self {
        let mut opt = TranspileOptions::script();
        opt.inject_prelude = false;
        opt.inject_runtime = false;
        opt.allow_await = true;
        opt.interactive = true;
        opt
    }

    pub fn module() -> Self {
        TranspileOptions {
            allow_await: false,
            inject_prelude: true,
            inject_runtime: true,
            set_exports: true,
            interactive: false,
        }
    }

    pub fn no_prelude() -> Self {
        let mut opt = TranspileOptions::module();
        opt.inject_prelude = false; // don't inject the prelude when loading prelude
        opt
    }
}

pub fn transpile_to_py_ast<'src>(
    src: &'src str,
    filename: &'src str,
    options: TranspileOptions,
) -> TlResult<PyBlock<'src>> {
    let tl_ast = parse_tl(src)?;

    let mut errs = TlErrs::new();

    let (resolve_state, errors, tl_ast) =
        resolve_scopes::resolve_names(src, tl_ast, options.allow_await);

    errs.extend(errors);

    let inference = match inference::infer(&src, &tl_ast, &resolve_state) {
        Ok(inference) => inference,
        Err(e) => {
            errs.extend(e);
            return Err(errs);
        }
    };

    let output = match transform_ast(
        &src,
        &filename,
        &tl_ast,
        &resolve_state,
        &inference,
        options.interactive,
    ) {
        Ok(output) => Some(output),
        Err(e) => {
            errs.extend(e);
            return Err(errs);
        }
    };

    if errs.0.len() > 0 {
        return Err(errs);
    }

    let Some(output) = output else {
        return Err(TlErrs(vec![TlErr {
            kind: TlErrKind::Unknown,
            message: "Failed to transform AST".to_string(),
            span: None,
            contexts: vec![],
        }]));
    };

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
            a.tl_builtin("set_exports"),
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

pub fn transpile_to_source(
    src: &str,
    filename: &str,
    options: TranspileOptions,
) -> TlResult<EmitCtx> {
    let mut py_ast = transpile_to_py_ast(src, filename, options)?;

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

pub fn format_errs(errs: &TlErrs, filename: &str, src: &str) -> Vec<u8> {
    let filename = filename.to_string();
    let mut writer = Vec::<u8>::new();

    for e in &errs.0 {
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
                let msg = if let TlErrKind::Parse = e.kind {
                    format!("while parsing this {label}")
                } else {
                    label.clone()
                };

                Label::new((filename.clone(), span.into_range()))
                    .with_message(msg)
                    .with_color(Color::Yellow)
            }))
            .finish()
            .write(sources([(filename.clone(), src)]), &mut writer)
            .unwrap();
    }

    writer
}

pub fn parse_tl<'src>(src: &'src str) -> TlResult<SExpr<'src>> {
    let mut errs = TlErrs::new();

    let (tokens, token_errs) = tokenize(&src);
    errs.extend(TlErrs(
        token_errs
            .into_iter()
            .map(|e| TlErr {
                kind: TlErrKind::Tokenize,
                message: e.reason().to_string(),
                span: Some(*e.span()),
                contexts: e
                    .contexts()
                    .map(|(label, span)| (label.to_string(), *span))
                    .collect(),
            })
            .collect(),
    ));

    let tokens: TokenList<'src> = match tokens {
        Some(tokens) => tokens,
        None => return Err(errs),
    };
    // println!("tokens: {tokens}");

    let (tl_ast, parser_errs) = parse_tokens(&src, &tokens);
    errs.extend(TlErrs(
        parser_errs
            .into_iter()
            .map(|e| TlErr {
                kind: TlErrKind::Parse,
                message: e.reason().to_string(),
                span: Some(*e.span()),
                contexts: e
                    .contexts()
                    .map(|(label, span)| (label.to_string(), *span))
                    .collect(),
            })
            .collect(),
    ));

    let tl_ast = tl_ast.ok_or_else(|| errs)?;
    // println!("AST: {ast:?}");

    Ok(tl_ast)
}
