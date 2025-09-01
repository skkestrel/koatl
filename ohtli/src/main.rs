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
        .get_matches();

    let file_path = matches.get_one::<String>("file").unwrap();
    let check_mode = matches.get_flag("check");

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
