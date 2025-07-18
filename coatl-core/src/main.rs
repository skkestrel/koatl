#![allow(unused_variables)]

use std::{
    io::Write,
    process::{Command, Stdio},
};

use ariadne::{Color, Label, Report, ReportKind, sources};
use coatl_core::{TlErrKind, TranspileOptions, transpile};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cmd = std::env::args().nth(1).ok_or("Missing command argument")?;
    let filename = std::env::args().nth(2).ok_or("Missing filename argument")?;
    let src = std::fs::read_to_string(&filename).unwrap();

    match transpile(&src, TranspileOptions::module()) {
        Ok(ctx) => match cmd.as_str() {
            "trans" => {
                println!("{}", ctx.source);
            }
            "run" => {
                let mut child = Command::new("python3").stdin(Stdio::piped()).spawn()?;

                if let Some(stdin) = child.stdin.as_mut() {
                    stdin.write_all(ctx.source.as_bytes())?;
                }

                let output = child.wait_with_output()?;
                if !output.status.success() {
                    eprintln!("{}", String::from_utf8_lossy(&output.stderr));
                    return Err("Python script execution failed.".into());
                }
            }
            _ => {
                return Err("Unknown command. Use 'trans' or 'run'.".into());
            }
        },
        Err(errs) => {
            errs.into_iter().for_each(|e| {
                let range = e.span.map(|e| e.into_range()).unwrap_or(0..0);
                let err_prefix = match e.kind {
                    TlErrKind::Parse => "Parser Error: ",
                    TlErrKind::Transform => "Transformation Error: ",
                    TlErrKind::Emit => "Emission Error: ",
                };

                Report::build(ReportKind::Error, (filename.clone(), range.clone()))
                    .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                    .with_message(err_prefix.to_string() + &e.message)
                    .with_label(
                        Label::new((filename.clone(), range.clone()))
                            .with_message(e.message)
                            .with_color(Color::Red),
                    )
                    .with_labels(e.contexts.into_iter().map(|(label, span)| {
                        Label::new((filename.clone(), span.into_range()))
                            .with_message(format!("while parsing this {label}"))
                            .with_color(Color::Yellow)
                    }))
                    .finish()
                    .eprint(sources([(filename.clone(), src.clone())]))
                    .unwrap()
            });

            return Err("Transpilation errors occurred.".into());
        }
    }

    Ok(())
}
