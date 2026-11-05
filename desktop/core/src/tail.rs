//! The last lines of the sidecar's stderr, shown when it fails.

use std::collections::VecDeque;

pub struct Tail {
    lines: VecDeque<String>,
    max: usize,
}

impl Tail {
    pub fn new(max: usize) -> Self {
        Tail { lines: VecDeque::with_capacity(max), max }
    }

    pub fn push(&mut self, line: &str) {
        if self.lines.len() == self.max {
            self.lines.pop_front();
        }
        self.lines.push_back(line.trim_end().to_string());
    }

    /// The kept lines, without leading or trailing blank ones.
    pub fn text(&self) -> String {
        let lines: Vec<&str> = self.lines.iter().map(String::as_str).collect();
        let start = lines.iter().position(|l| !l.trim().is_empty()).unwrap_or(lines.len());
        let end = lines.iter().rposition(|l| !l.trim().is_empty()).map_or(start, |i| i + 1);
        lines[start..end].join("\n")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keeps_the_last_lines() {
        let mut t = Tail::new(3);
        assert_eq!(t.text(), "");
        for l in ["", "a", "b\r", "c", "", ""] {
            t.push(l);
        }
        assert_eq!(t.text(), "c");
        t.push("d");
        assert_eq!(t.text(), "d");
        let mut t = Tail::new(3);
        for l in ["a", "b", "c", "d"] {
            t.push(l);
        }
        assert_eq!(t.text(), "b\nc\nd");
    }
}
