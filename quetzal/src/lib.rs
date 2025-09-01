use koatl::ohtli::types::*;
use ohtli::{Config, Formatter};

wit_bindgen::generate!({
    world: "formatter",
});

struct FormatterImpl;

impl Guest for FormatterImpl {
    fn format(o: String) -> String {
        let formatter = Formatter::new(Config::default());
        let result = formatter.format(&o).unwrap_or("".to_string());
        result
    }

    fn format_diff(original: String) -> Vec<OhtliDiffHunk> {
        format_diff(original)
    }
}

fn format_diff(original: String) -> Vec<OhtliDiffHunk> {
    let formatter = Formatter::new(Config::default());
    let formatted = formatter.format(&original).unwrap_or(original.clone());

    let diff = similar::TextDiff::from_lines(original.as_str(), formatted.as_str());
    let mut hunks = Vec::new();

    for op in diff.ops() {
        let mut lines = Vec::new();

        // Convert the operation to individual line changes
        match op.tag() {
            similar::DiffTag::Equal => {
                for i in op.old_range() {
                    if let Some(line) = diff.old_slices().get(i) {
                        lines.push(OhtliDiffLine {
                            tag: OhtliDiffTag::Equal,
                            value: line.to_string(),
                        });
                    }
                }
            }
            similar::DiffTag::Delete => {
                for i in op.old_range() {
                    if let Some(line) = diff.old_slices().get(i) {
                        lines.push(OhtliDiffLine {
                            tag: OhtliDiffTag::Delete,
                            value: line.to_string(),
                        });
                    }
                }
            }
            similar::DiffTag::Insert => {
                for i in op.new_range() {
                    if let Some(line) = diff.new_slices().get(i) {
                        lines.push(OhtliDiffLine {
                            tag: OhtliDiffTag::Insert,
                            value: line.to_string(),
                        });
                    }
                }
            }
            similar::DiffTag::Replace => {
                // Handle replace as delete + insert
                for i in op.old_range() {
                    if let Some(line) = diff.old_slices().get(i) {
                        lines.push(OhtliDiffLine {
                            tag: OhtliDiffTag::Delete,
                            value: line.to_string(),
                        });
                    }
                }
                for i in op.new_range() {
                    if let Some(line) = diff.new_slices().get(i) {
                        lines.push(OhtliDiffLine {
                            tag: OhtliDiffTag::Insert,
                            value: line.to_string(),
                        });
                    }
                }
            }
        }

        hunks.push(OhtliDiffHunk {
            tag: match op.tag() {
                similar::DiffTag::Equal => OhtliDiffTag::Equal,
                similar::DiffTag::Delete => OhtliDiffTag::Delete,
                similar::DiffTag::Insert => OhtliDiffTag::Insert,
                similar::DiffTag::Replace => OhtliDiffTag::Insert, // Treat replace as insert for hunk tag
            },
            old_start: op.old_range().start as u32,
            old_count: op.old_range().len() as u32,
            new_start: op.new_range().start as u32,
            new_count: op.new_range().len() as u32,
            lines,
        });
    }

    hunks
}

export!(FormatterImpl);
