use std::time::Instant;

use koatl_core::parse_tl;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let filename = std::env::args().nth(1).ok_or("Missing filename argument")?;
    let src = std::fs::read_to_string(&filename).unwrap();

    let now = Instant::now();

    let n = 1000;

    for _ in 0..n {
        match parse_tl(&src) {
            Ok(_) => {}
            Err(errs) => {
                errs.0.into_iter().for_each(|e| {
                    eprintln!("Error: {}", e.message);
                    if let Some(span) = e.span {
                        eprintln!("At span: {:?}", span);
                    }
                });
                return Err("Parsing failed".into());
            }
        }
    }

    println!(
        "Parsed {} in {}us",
        filename,
        now.elapsed().as_nanos() as f64 / 1000.0 / n as f64
    );

    Ok(())
}
