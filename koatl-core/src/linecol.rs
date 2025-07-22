pub struct LineColCache {
    line_starts: Vec<usize>,
}

impl LineColCache {
    pub fn new(source: &str) -> Self {
        let mut line_starts = vec![0];

        for (i, c) in source.char_indices() {
            if c == '\n' {
                line_starts.push(i + 1);
            }
        }

        line_starts.push(usize::MAX);

        Self { line_starts }
    }

    pub fn linecol(&self, cursor: usize) -> (usize, usize) {
        for (i, pos) in self.line_starts.iter().enumerate() {
            if *pos > cursor {
                return (i, cursor - self.line_starts[i - 1]);
            }
        }
        panic!();
    }
}
