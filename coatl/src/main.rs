#![allow(unused_variables)]

pub mod py;

use ariadne::{Color, Label, Report, ReportKind, sources};
use pyo3::types::PyTracebackMethods;

use crate::py::{TlErrs, transpile};
use parser::{parse_tokens, tokenize};

fn main() {
    let filename = std::env::args().nth(1).unwrap();
    let src = std::fs::read_to_string(&filename).unwrap();

    let (tokens, errs) = tokenize(&src);
    let (ast, parse_errs) = parse_tokens(&src, &tokens);

    // println!("{tokens}");
    // println!("{ast:?}");

    errs.into_iter()
        .map(|e| e.map_token(|c| c.to_string()))
        .chain(
            parse_errs
                .into_iter()
                .map(|e| e.map_token(|c| c.to_string())),
        )
        .for_each(|e| {
            Report::build(ReportKind::Error, (filename.clone(), e.span().into_range()))
                .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                .with_message(e.to_string())
                .with_label(
                    Label::new((filename.clone(), e.span().into_range()))
                        .with_message(e.reason().to_string())
                        .with_color(Color::Red),
                )
                .with_labels(e.contexts().map(|(label, span)| {
                    Label::new((filename.clone(), span.into_range()))
                        .with_message(format!("while parsing this {label}"))
                        .with_color(Color::Yellow)
                }))
                .finish()
                .eprint(sources([(filename.clone(), src.clone())]))
                .unwrap()
        });

    if let Some(ast) = ast {
        match transpile(&src, &ast) {
            Ok(code) => {
                println!("{}", code);
            }
            Err(TlErrs(e)) => {
                for e in e {
                    let span = e.span.map(|x| x.into_range()).unwrap_or(0..0);
                    let mut py_err_details: Option<(String, String)> = None;

                    if let Some(py_err) = e.py_err {
                        pyo3::Python::with_gil(|py| {
                            let py_err_name = py_err.to_string();
                            let py_err_tb = py_err
                                .traceback(py)
                                .map(|tb| tb.format().unwrap_or_default());

                            py_err_details = Some((py_err_name, py_err_tb.unwrap_or_default()));
                        });
                    }

                    let mut builder =
                        Report::build(ReportKind::Error, (filename.clone(), span.clone()))
                            .with_config(
                                ariadne::Config::new().with_index_type(ariadne::IndexType::Byte),
                            )
                            .with_message(&e.message)
                            .with_label(
                                Label::new((filename.clone(), span.clone()))
                                    .with_message(&e.message)
                                    .with_color(Color::Red),
                            );

                    if let Some((name, tb)) = py_err_details {
                        builder = builder.with_note(format!("{name}\n{tb}"));
                    }

                    builder
                        .finish()
                        .eprint(sources([(filename.clone(), src.clone())]))
                        .unwrap();
                }
            }
        }
    }
}
