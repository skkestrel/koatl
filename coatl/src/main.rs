#![allow(unused_variables)]

pub mod ast_util;
pub mod py;
pub mod py_ast;
pub mod py_gen;

use ariadne::{Color, Label, Report, ReportKind, sources};
use pyo3::types::PyTracebackMethods;

use crate::py::{TlErrs, transpile};
use parser::{parse_tokens, tokenize};
use std::io::Write;
use std::process::{Command, Stdio};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cmd = std::env::args().nth(1).ok_or("Missing command argument")?;
    let filename = std::env::args().nth(2).ok_or("Missing filename argument")?;

    let src = std::fs::read_to_string(&filename).unwrap();
    let mut error = None;

    let (tokens, token_errs) = tokenize(&src);
    let token_errs = token_errs
        .into_iter()
        .map(|e| e.map_token(|c| c.to_string()))
        .collect::<Vec<_>>();

    if !token_errs.is_empty() {
        error = Some("Tokenization errors occurred".into());
    }

    let mut parse_errs = vec![];

    if let Some(ref tokens) = tokens {
        // println!("tokens: {tokens}");

        let (ast, parse_errs_) = parse_tokens(&src, tokens);

        parse_errs.extend(
            parse_errs_
                .into_iter()
                .map(|e| e.map_token(|c| c.to_string())),
        );

        if error.is_none() && !parse_errs.is_empty() {
            error = Some("Parsing errors occurred".into());
        }

        if let Some(ast) = ast {
            // println!("AST: {ast:?}");

            match transpile(&src, &ast) {
                Ok(code) => {
                    if cmd == "trans" {
                        println!("{}", code);
                    } else if cmd == "run" {
                        let mut child = Command::new("python3").stdin(Stdio::piped()).spawn()?;

                        if let Some(stdin) = child.stdin.as_mut() {
                            stdin.write_all(code.as_bytes())?;
                        }

                        let output = child.wait_with_output()?;
                        if !output.status.success() {
                            error = Some("Python script execution failed".into());
                            eprintln!("{}", String::from_utf8_lossy(&output.stderr));
                        }
                    } else {
                        return Err("Unknown command. Use 'trans' or 'run'.".into());
                    }
                }
                Err(TlErrs(e)) => {
                    error = Some("Transpilation errors occurred".into());

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
                                    ariadne::Config::new()
                                        .with_index_type(ariadne::IndexType::Byte),
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

    token_errs.into_iter().for_each(|e| {
        Report::build(ReportKind::Error, (filename.clone(), e.span().into_range()))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message("Tokenizer Error: ".to_string() + &e.to_string())
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

    parse_errs.into_iter().for_each(|e| {
        Report::build(ReportKind::Error, (filename.clone(), e.span().into_range()))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message("Parser Error: ".to_string() + &e.to_string())
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

    if let Some(error) = error {
        return Err(error);
    }

    Ok(())
}
