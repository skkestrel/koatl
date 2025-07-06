#![allow(unused_variables)]

pub mod lexer;
pub mod parser;

use ariadne::{Color, Label, Report, ReportKind, sources};
use chumsky::prelude::*;

use crate::lexer::tokenize;
use crate::parser::parser;

fn main() {
    let filename = std::env::args().nth(1).unwrap();
    let src = std::fs::read_to_string(&filename).unwrap();

    let (tokens, mut errs) = tokenize(&src);
    let (ast, mut parse_errs) = parser()
        .parse(
            tokens
                .0
                .as_slice()
                .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
        )
        .into_output_errors();

    println!("{tokens}");
    println!("{ast:?}");

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
}
