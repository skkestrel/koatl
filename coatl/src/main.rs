#![allow(unused_variables)]

pub mod py;

use ariadne::{Color, Label, Report, ReportKind, sources};
use pyo3::types::PyTracebackMethods;

use crate::py::{TlErr, transpile};
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
        match transpile(&ast) {
            Ok(code) => {
                println!("{}", code);
            }
            Err(e) => match e {
                TlErr::PyErr(e) => {
                    println!("{}", e);
                    pyo3::Python::with_gil(|py| {
                        if let Some(tb) = e.traceback(py) {
                            if let Ok(formatted) = tb.format() {
                                println!("{}", formatted);
                            }
                        }
                    });
                }
                TlErr::TranspileErr(e) => {
                    if let Some(span) = e.span {
                        Report::build(ReportKind::Error, (filename.clone(), span.into_range()))
                            .with_config(
                                ariadne::Config::new().with_index_type(ariadne::IndexType::Byte),
                            )
                            .with_message(&e.message)
                            .with_label(
                                Label::new((filename.clone(), span.into_range()))
                                    .with_message(&e.message)
                                    .with_color(Color::Red),
                            )
                            .finish()
                            .eprint(sources([(filename.clone(), src.clone())]))
                            .unwrap();
                    }
                }
            },
        }
    }
}
