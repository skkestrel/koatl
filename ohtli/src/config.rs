#[derive(Debug, Clone)]
pub struct Config {
    /// Number of spaces per indentation level
    pub indent_width: usize,

    /// Maximum line length before wrapping
    pub max_line_length: usize,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            indent_width: 4,
            max_line_length: 100,
        }
    }
}

impl Config {
    /// Load configuration from a file (future feature)
    pub fn from_file(_path: &str) -> anyhow::Result<Self> {
        // TODO: Implement configuration file loading (TOML, JSON, etc.)
        Ok(Self::default())
    }
}
