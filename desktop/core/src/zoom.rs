//! The window's zoom: the steps Ctrl+= and Ctrl+- go through (a browser's),
//! and how the level is kept between runs (a file holding the number).

/// Chromium's zoom levels.
pub const LEVELS: [f64; 15] = [0.25, 0.33, 0.5, 0.67, 0.75, 0.8, 0.9, 1.0, 1.1, 1.25, 1.5, 1.75, 2.0, 2.5, 3.0];

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Step {
    In,
    Out,
    Reset,
}

/// The level after `step` from `current` (which need not be one of
/// [LEVELS], e.g. after a pinch).
pub fn step(current: f64, step: Step) -> f64 {
    const EPS: f64 = 0.001;
    match step {
        Step::Reset => 1.0,
        Step::In => LEVELS.iter().copied().find(|l| *l > current + EPS).unwrap_or(LEVELS[LEVELS.len() - 1]),
        Step::Out => LEVELS.iter().rev().copied().find(|l| *l < current - EPS).unwrap_or(LEVELS[0]),
    }
}

/// The level kept in the zoom file; None when it is missing or not a level
/// in range (the window then starts at 100%).
pub fn parse(text: &str) -> Option<f64> {
    let level: f64 = text.trim().parse().ok()?;
    (level.is_finite() && (LEVELS[0]..=LEVELS[LEVELS.len() - 1]).contains(&level)).then_some(level)
}

pub fn format(level: f64) -> String {
    format!("{level:.2}\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn steps() {
        assert_eq!(step(1.0, Step::In), 1.1);
        assert_eq!(step(1.0, Step::Out), 0.9);
        assert_eq!(step(1.25, Step::In), 1.5);
        assert_eq!(step(1.3, Step::In), 1.5);
        assert_eq!(step(1.3, Step::Out), 1.25);
        assert_eq!(step(3.0, Step::In), 3.0);
        assert_eq!(step(0.25, Step::Out), 0.25);
        assert_eq!(step(2.5, Step::Reset), 1.0);
        // a level just off a step (float noise) moves on from it
        assert_eq!(step(1.1000001, Step::In), 1.25);
        assert_eq!(step(0.8999999, Step::Out), 0.8);
    }

    #[test]
    fn kept() {
        assert_eq!(parse("1.25\n"), Some(1.25));
        assert_eq!(parse(&format(1.5)), Some(1.5));
        assert_eq!(format(1.1), "1.10\n");
        assert_eq!(parse(""), None);
        assert_eq!(parse("big"), None);
        assert_eq!(parse("9"), None);
        assert_eq!(parse("0.1"), None);
        assert_eq!(parse("NaN"), None);
    }
}
