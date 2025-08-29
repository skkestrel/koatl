//! Ohtli - A code formatter for the Koatl programming language
//!
//! This is a basic formatter that processes Koatl source files and applies
//! consistent formatting rules. It uses the official Koatl parser to build
//! a Concrete Syntax Tree (CST) and then traverses it to emit formatted code.
//!
//! Current status: Basic scaffolding with support for simple expressions,
//! literals, identifiers, and basic statements. Many Koatl language constructs
//! are not yet supported and will fall back to placeholder text.
//!
//! TODO: Expand support for:
//! - Function definitions and lambda expressions
//! - Match expressions and pattern matching
//! - Control flow (if/else, loops)
//! - Collections (lists, maps, tuples)
//! - Class definitions and method calls
//! - Import statements
//! - Complex expressions and operators

use anyhow::Result;
use clap::{Arg, Command};
use std::fs;
use std::path::Path;

mod config;
mod formatter;

use config::Config;
use formatter::Formatter;

fn main() -> Result<()> {
    let matches = Command::new("ohtli")
        .version("0.1.0")
        .author("Koatl Team")
        .about("A code formatter for the Koatl language")
        .arg(
            Arg::new("file")
                .help("The file to format")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::new("check")
                .long("check")
                .help("Check if the file is formatted without modifying it")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("diff")
                .long("diff")
                .help("Show diff of changes without modifying the file")
                .action(clap::ArgAction::SetTrue),
        )
        .get_matches();

    let file_path = matches.get_one::<String>("file").unwrap();
    let check_mode = matches.get_flag("check");
    let diff_mode = matches.get_flag("diff");

    let config = Config::default();
    let formatter = Formatter::new(config);

    if !Path::new(file_path).exists() {
        anyhow::bail!("File not found: {}", file_path);
    }

    let source = fs::read_to_string(file_path)?;

    match formatter.format(&source) {
        Ok(formatted) => {
            if check_mode {
                if source != formatted {
                    println!("File {} is not formatted correctly", file_path);
                    std::process::exit(1);
                } else {
                    println!("File {} is correctly formatted", file_path);
                }
            } else if diff_mode {
                if source != formatted {
                    println!("--- {}", file_path);
                    println!("+++ {} (formatted)", file_path);
                    // Simple diff - in a real formatter you'd want a proper diff library
                    let original_lines: Vec<&str> = source.lines().collect();
                    let formatted_lines: Vec<&str> = formatted.lines().collect();

                    for (i, (orig, new)) in original_lines
                        .iter()
                        .zip(formatted_lines.iter())
                        .enumerate()
                    {
                        if orig != new {
                            println!("@@ -{},{} +{},{} @@", i + 1, 1, i + 1, 1);
                            println!("-{}", orig);
                            println!("+{}", new);
                        }
                    }
                } else {
                    println!("No formatting changes needed for {}", file_path);
                }
            } else {
                fs::write(file_path, formatted)?;
                println!("Formatted {}", file_path);
            }
        }
        Err(e) => {
            eprintln!("Error formatting {}: {}", file_path, e);
            std::process::exit(1);
        }
    }

    Ok(())
}
