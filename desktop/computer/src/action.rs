//! What the model asks the computer to do (the `computer` tool's arguments,
//! passed through unchanged): points are in the scaled screenshot's space.

use crate::keysym::{parse_combo, parse_combos, Combo};
use serde_json::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Button {
    Left,
    Right,
    Middle,
    Back,
    Forward,
}

impl Button {
    pub fn name(self) -> &'static str {
        match self {
            Button::Left => "left",
            Button::Right => "right",
            Button::Middle => "middle",
            Button::Back => "back",
            Button::Forward => "forward",
        }
    }

    /// Linux's input event code (BTN_LEFT...).
    pub fn evdev(self) -> u32 {
        match self {
            Button::Left => 0x110,
            Button::Right => 0x111,
            Button::Middle => 0x112,
            Button::Back => 0x116,
            Button::Forward => 0x115,
        }
    }

    /// The X button number.
    pub fn x11(self) -> u8 {
        match self {
            Button::Left => 1,
            Button::Middle => 2,
            Button::Right => 3,
            Button::Back => 8,
            Button::Forward => 9,
        }
    }
}

pub type Point = (i64, i64);

#[derive(Debug, Clone, PartialEq)]
pub enum Action {
    Screenshot,
    Click { at: Option<Point>, button: Button, count: u32, keys: Option<Combo> },
    Move { at: Point },
    Drag { path: Vec<Point>, button: Button, keys: Option<Combo> },
    MouseDown { at: Option<Point>, button: Button },
    MouseUp { at: Option<Point>, button: Button },
    Scroll { at: Option<Point>, dx: i32, dy: i32, keys: Option<Combo> },
    Type { text: String },
    Key { combos: Vec<Combo> },
    HoldKey { combo: Combo, duration: f64 },
    Wait { seconds: f64 },
    Zoom { region: (i64, i64, i64, i64) },
    CursorPosition,
}

const ACTIONS: &str =
    "screenshot, click, move, drag, mouse_down, mouse_up, scroll, type, key, hold_key, wait, zoom, cursor_position";

fn int(v: &Value, what: &str) -> Result<i64, String> {
    match v.as_f64() {
        Some(f) if f.is_finite() => Ok(f.round() as i64),
        _ => Err(format!("{what} must be a number, got {v}")),
    }
}

impl Action {
    pub fn parse(v: &Value) -> Result<Action, String> {
        let obj = v
            .as_object()
            .ok_or_else(|| format!("an action must be an object like {{\"action\":\"screenshot\"}}, got {v}"))?;
        let get = |k: &str| obj.get(k).filter(|v| !v.is_null());
        let name =
            get("action").and_then(Value::as_str).ok_or_else(|| format!("\"action\" is missing: one of {ACTIONS}"))?;
        let point = |kx: &str, ky: &str| -> Result<Option<Point>, String> {
            match (get(kx), get(ky)) {
                (None, None) => Ok(None),
                (Some(x), Some(y)) => Ok(Some((int(x, kx)?, int(y, ky)?))),
                _ => Err(format!("{name}: give both {kx} and {ky}, or neither (the pointer's current position)")),
            }
        };
        let required_point = || -> Result<Point, String> {
            point("x", "y")?.ok_or_else(|| format!("{name} needs x and y (in the screenshot's pixels)"))
        };
        let button = || -> Result<Button, String> {
            match get("button").map(|b| b.as_str().unwrap_or("")) {
                None | Some("left") => Ok(Button::Left),
                Some("right") => Ok(Button::Right),
                Some("middle") | Some("wheel") => Ok(Button::Middle),
                Some("back") => Ok(Button::Back),
                Some("forward") => Ok(Button::Forward),
                Some(other) => Err(format!("button {other:?}: use left, right, middle, back or forward")),
            }
        };
        let keys = || -> Result<Option<Combo>, String> {
            match get("keys") {
                None => Ok(None),
                Some(Value::String(s)) if s.trim().is_empty() => Ok(None),
                Some(Value::String(s)) => parse_combo(s).map(Some),
                Some(other) => Err(format!("keys must be a string like \"ctrl+shift\", got {other}")),
            }
        };
        let string = |k: &str| -> Result<String, String> {
            get(k).and_then(Value::as_str).map(str::to_string).ok_or_else(|| format!("{name} needs {k} (a string)"))
        };
        let seconds = |k: &str, max: f64| -> Result<f64, String> {
            let s = get(k).and_then(Value::as_f64).ok_or_else(|| format!("{name} needs {k} (seconds)"))?;
            if !(0.0..=max).contains(&s) {
                return Err(format!("{k} must be between 0 and {max} seconds, got {s}"));
            }
            Ok(s)
        };
        Ok(match name {
            "screenshot" => Action::Screenshot,
            "click" => {
                let count = match get("count") {
                    None => 1,
                    Some(c) => {
                        let c = int(c, "count")?;
                        if !(1..=3).contains(&c) {
                            return Err(format!("count must be 1, 2 or 3, got {c}"));
                        }
                        c as u32
                    }
                };
                Action::Click { at: point("x", "y")?, button: button()?, count, keys: keys()? }
            }
            "move" => Action::Move { at: required_point()? },
            "drag" => {
                let path = get("path")
                    .and_then(Value::as_array)
                    .ok_or_else(|| "drag needs path: [[x, y], [x, y], ...] (at least two points)".to_string())?
                    .iter()
                    .map(|p| match p.as_array().map(Vec::as_slice) {
                        Some([x, y]) => Ok((int(x, "x")?, int(y, "y")?)),
                        _ => Err(format!("each point of path must be [x, y], got {p}")),
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                if path.len() < 2 {
                    return Err("drag's path needs at least two points: where to press and where to release".into());
                }
                Action::Drag { path, button: button()?, keys: keys()? }
            }
            "mouse_down" => Action::MouseDown { at: point("x", "y")?, button: button()? },
            "mouse_up" => Action::MouseUp { at: point("x", "y")?, button: button()? },
            "scroll" => {
                let notches = |k: &str| -> Result<i32, String> {
                    match get(k) {
                        None => Ok(0),
                        Some(v) => {
                            let n = int(v, k)?;
                            if n.abs() > 100 {
                                return Err(format!("{k} is in wheel notches: at most 100 at a time, got {n}"));
                            }
                            Ok(n as i32)
                        }
                    }
                };
                let (dx, dy) = (notches("dx")?, notches("dy")?);
                if dx == 0 && dy == 0 {
                    return Err("scroll needs dx or dy (wheel notches: +dy scrolls down, +dx right)".into());
                }
                Action::Scroll { at: point("x", "y")?, dx, dy, keys: keys()? }
            }
            "type" => {
                let text = string("text")?;
                if text.is_empty() {
                    return Err("type needs some text".into());
                }
                Action::Type { text }
            }
            "key" => Action::Key { combos: parse_combos(&string("combo")?)? },
            "hold_key" => {
                Action::HoldKey { combo: parse_combo(&string("combo")?)?, duration: seconds("duration", 10.0)? }
            }
            "wait" => Action::Wait { seconds: seconds("seconds", 30.0)? },
            "zoom" => {
                let region = get("region").and_then(Value::as_array).map(Vec::as_slice);
                match region {
                    Some([a, b, c, d]) => {
                        let (x0, y0, x1, y1) = (int(a, "x0")?, int(b, "y0")?, int(c, "x1")?, int(d, "y1")?);
                        if x1 <= x0 || y1 <= y0 {
                            return Err(format!(
                                "zoom's region is [x0, y0, x1, y1] with x0 < x1 and y0 < y1, got [{x0}, {y0}, {x1}, {y1}]"
                            ));
                        }
                        Action::Zoom { region: (x0, y0, x1, y1) }
                    }
                    _ => return Err("zoom needs region: [x0, y0, x1, y1] (the screenshot's pixels)".into()),
                }
            }
            "cursor_position" => Action::CursorPosition,
            other => return Err(format!("unknown action {other:?}: use one of {ACTIONS}")),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn p(v: Value) -> String {
        match Action::parse(&v) {
            Ok(a) => format!("{a:?}"),
            Err(e) => format!("error: {e}"),
        }
    }

    #[test]
    fn parses() {
        assert_eq!(p(json!({"action":"screenshot"})), "Screenshot");
        assert_eq!(
            p(json!({"action":"click","x":640,"y":312})),
            "Click { at: Some((640, 312)), button: Left, count: 1, keys: None }"
        );
        assert!(p(json!({"action":"click","x":1,"y":2,"button":"right","count":2,"keys":"ctrl"}))
            .starts_with("Click { at: Some((1, 2)), button: Right, count: 2, keys: Some(Combo { keys: [65507]"));
        assert_eq!(
            p(json!({"action":"click","x":1})),
            "error: click: give both x and y, or neither (the pointer's current position)"
        );
        assert_eq!(p(json!({"action":"click","count":4})), "error: count must be 1, 2 or 3, got 4");
        assert_eq!(p(json!({"action":"move"})), "error: move needs x and y (in the screenshot's pixels)");
        assert!(p(json!({"action":"drag","path":[[1,2]]})).contains("at least two points"));
        assert_eq!(
            p(json!({"action":"drag","path":[[1,2],[3,4.6]]})),
            "Drag { path: [(1, 2), (3, 5)], button: Left, keys: None }"
        );
        assert_eq!(
            p(json!({"action":"scroll","x":5,"y":6,"dy":3})),
            "Scroll { at: Some((5, 6)), dx: 0, dy: 3, keys: None }"
        );
        assert!(p(json!({"action":"scroll"})).contains("needs dx or dy"));
        assert_eq!(p(json!({"action":"type","text":"hi"})), "Type { text: \"hi\" }");
        assert!(p(json!({"action":"key","combo":"ctrl+x Return"})).starts_with("Key { combos: [Combo"));
        assert!(p(json!({"action":"hold_key","combo":"shift","duration":11})).contains("between 0 and 10"));
        assert_eq!(p(json!({"action":"wait","seconds":2})), "Wait { seconds: 2.0 }");
        assert_eq!(p(json!({"action":"zoom","region":[0,0,10,10]})), "Zoom { region: (0, 0, 10, 10) }");
        assert!(p(json!({"action":"zoom","region":[10,0,0,10]})).contains("x0 < x1"));
        assert_eq!(p(json!({"action":"cursor_position"})), "CursorPosition");
        assert!(p(json!({"action":"fly"})).starts_with("error: unknown action \"fly\": use one of screenshot"));
        assert!(p(json!(3)).starts_with("error: an action must be an object"));
        assert!(p(json!({"action":"mouse_down","button":"thumb"})).contains("use left, right"));
    }
}
