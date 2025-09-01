use std::{
    io::Write,
    process::{Command, Stdio},
};

use ariadne::{Color, Label, Report, ReportKind, sources};
use clap::{Parser, Subcommand};
use koatl_core::{TranspileOptions, transpile_to_source, util::TlErrKind};

#[derive(Parser)]
#[command(name = "koatl-core")]
#[command(about = "A Koatl transpiler and runner")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Transpile Koatl source to Python
    Trans {
        /// Input file to transpile
        filename: String,
    },
    /// Transpile and run Koatl source
    Run {
        /// Input file to run
        filename: String,
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    let (filename, is_run) = match cli.command {
        Commands::Trans { filename } => (filename, false),
        Commands::Run { filename } => (filename, true),
    };

    let src = std::fs::read_to_string(&filename).unwrap();

    match transpile_to_source(&src, &filename, TranspileOptions::module()) {
        Ok(ctx) => {
            if is_run {
                let mut child = Command::new("python3").stdin(Stdio::piped()).spawn()?;

                if let Some(stdin) = child.stdin.as_mut() {
                    stdin.write_all(ctx.source.as_bytes())?;
                }

                let output = child.wait_with_output()?;
                if !output.status.success() {
                    eprintln!("{}", String::from_utf8_lossy(&output.stderr));
                    return Err("Python script execution failed.".into());
                }
            } else {
                println!("{}", ctx.source);
            }
        }
        Err(errs) => {
            errs.0.into_iter().for_each(|e| {
                let range = e.span.map(|e| e.start..e.end).unwrap_or(0..0);
                let err_prefix = match e.kind {
                    TlErrKind::Tokenize => "Tokenization Error: ",
                    TlErrKind::Parse => "Parser Error: ",
                    TlErrKind::Transform => "Transformation Error: ",
                    TlErrKind::Emit => "Emission Error: ",
                    TlErrKind::Unknown => "Unknown Error: ",
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
                        Label::new((filename.clone(), span.start..span.end))
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
