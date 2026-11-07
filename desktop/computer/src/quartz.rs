//! What the macOS backend ([crate::macos]) sends, decided apart from the
//! calls that send it, so that it is tested on every platform: Quartz event
//! types, virtual keycodes and modifier flags, click counts, scroll steps,
//! and the conversion between the display's pixels (what the helper works
//! in) and Quartz's global points (what events take).

use crate::action::Button;
use crate::keysym::{of_char, Keysym};
use std::time::{Duration, Instant};

pub mod event_type {
    pub const LEFT_MOUSE_DOWN: u32 = 1;
    pub const LEFT_MOUSE_UP: u32 = 2;
    pub const RIGHT_MOUSE_DOWN: u32 = 3;
    pub const RIGHT_MOUSE_UP: u32 = 4;
    pub const MOUSE_MOVED: u32 = 5;
    pub const LEFT_MOUSE_DRAGGED: u32 = 6;
    pub const RIGHT_MOUSE_DRAGGED: u32 = 7;
    pub const OTHER_MOUSE_DOWN: u32 = 25;
    pub const OTHER_MOUSE_UP: u32 = 26;
    pub const OTHER_MOUSE_DRAGGED: u32 = 27;
}

/// CGMouseButton: also the kCGMouseEventButtonNumber of "other" buttons.
pub fn button_number(b: Button) -> u32 {
    match b {
        Button::Left => 0,
        Button::Right => 1,
        Button::Middle => 2,
        Button::Back => 3,
        Button::Forward => 4,
    }
}

pub fn button_event(b: Button, down: bool) -> u32 {
    use event_type::*;
    match (b, down) {
        (Button::Left, true) => LEFT_MOUSE_DOWN,
        (Button::Left, false) => LEFT_MOUSE_UP,
        (Button::Right, true) => RIGHT_MOUSE_DOWN,
        (Button::Right, false) => RIGHT_MOUSE_UP,
        (_, true) => OTHER_MOUSE_DOWN,
        (_, false) => OTHER_MOUSE_UP,
    }
}

/// A move while a button is held is a drag, or apps see no drag.
pub fn move_event(held: &[Button]) -> u32 {
    use event_type::*;
    match held.first() {
        None => MOUSE_MOVED,
        Some(Button::Left) => LEFT_MOUSE_DRAGGED,
        Some(Button::Right) => RIGHT_MOUSE_DRAGGED,
        Some(_) => OTHER_MOUSE_DRAGGED,
    }
}

/// Successive presses of one button at one place count up (1, 2, 3: a
/// double, a triple click), as the system counts a real mouse's: a synthetic
/// event carries its count itself (kCGMouseEventClickState).
#[derive(Debug, Default)]
pub struct Clicks {
    last: Option<(Button, (i32, i32), Instant, i64)>,
}

impl Clicks {
    pub const INTERVAL: Duration = Duration::from_millis(500);
    /// How far (in pixels) a press may be from the last and still count.
    pub const SLOP: i32 = 4;

    /// The count of a press of `b` at `at`.
    pub fn press(&mut self, b: Button, at: (i32, i32), now: Instant) -> i64 {
        let count = match self.last {
            Some((lb, (lx, ly), t, n))
                if lb == b
                    && now.saturating_duration_since(t) <= Self::INTERVAL
                    && (lx - at.0).abs() <= Self::SLOP
                    && (ly - at.1).abs() <= Self::SLOP =>
            {
                n + 1
            }
            _ => 1,
        };
        self.last = Some((b, at, now, count));
        count
    }

    /// The count of a release: its press's.
    pub fn release(&mut self, b: Button, now: Instant) -> i64 {
        match &mut self.last {
            Some((lb, _, t, n)) if *lb == b => {
                *t = now;
                *n
            }
            _ => 1,
        }
    }
}

/// CGEventFlags.
pub mod flag {
    pub const CAPS_LOCK: u64 = 0x0001_0000;
    pub const SHIFT: u64 = 0x0002_0000;
    pub const CONTROL: u64 = 0x0004_0000;
    pub const OPTION: u64 = 0x0008_0000;
    pub const COMMAND: u64 = 0x0010_0000;
}

/// The flag a modifier keysym holds: alt is Option and super Command (so
/// "cmd+c" and "super+c" are both ⌘C).
pub fn modifier_flag(k: Keysym) -> u64 {
    match k {
        0xffe1 | 0xffe2 => flag::SHIFT,
        0xffe3 | 0xffe4 => flag::CONTROL,
        0xffe9 | 0xffea => flag::OPTION,
        0xffeb | 0xffec => flag::COMMAND,
        0xffe5 => flag::CAPS_LOCK,
        _ => 0,
    }
}

/// The flags of the modifiers held, which every event carries.
pub fn flags(held: &[Keysym]) -> u64 {
    held.iter().fold(0, |f, k| f | modifier_flag(*k))
}

/// A virtual keycode (kVK_*), and whether the character needs Shift on it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Key {
    pub code: u16,
    pub shift: bool,
}

pub mod kvk {
    pub const RETURN: u16 = 0x24;
    pub const TAB: u16 = 0x30;
    pub const SPACE: u16 = 0x31;
}

/// Keys by position, as on the ANSI (US) keyboard: shortcuts ("cmd+c") are
/// sent by position, as most layouts keep letters there; text goes as
/// Unicode ([text_events]), whatever the layout.
const CHARS: &[(char, u16)] = &[
    ('a', 0x00),
    ('s', 0x01),
    ('d', 0x02),
    ('f', 0x03),
    ('h', 0x04),
    ('g', 0x05),
    ('z', 0x06),
    ('x', 0x07),
    ('c', 0x08),
    ('v', 0x09),
    ('b', 0x0b),
    ('q', 0x0c),
    ('w', 0x0d),
    ('e', 0x0e),
    ('r', 0x0f),
    ('y', 0x10),
    ('t', 0x11),
    ('1', 0x12),
    ('2', 0x13),
    ('3', 0x14),
    ('4', 0x15),
    ('6', 0x16),
    ('5', 0x17),
    ('=', 0x18),
    ('9', 0x19),
    ('7', 0x1a),
    ('-', 0x1b),
    ('8', 0x1c),
    ('0', 0x1d),
    (']', 0x1e),
    ('o', 0x1f),
    ('u', 0x20),
    ('[', 0x21),
    ('i', 0x22),
    ('p', 0x23),
    ('l', 0x25),
    ('j', 0x26),
    ('\'', 0x27),
    ('k', 0x28),
    (';', 0x29),
    ('\\', 0x2a),
    (',', 0x2b),
    ('/', 0x2c),
    ('n', 0x2d),
    ('m', 0x2e),
    ('.', 0x2f),
    ('`', 0x32),
    (' ', kvk::SPACE),
];

/// The US layout's shifted characters, and the key they are on.
const SHIFTED: &[(char, char)] = &[
    ('!', '1'),
    ('@', '2'),
    ('#', '3'),
    ('$', '4'),
    ('%', '5'),
    ('^', '6'),
    ('&', '7'),
    ('*', '8'),
    ('(', '9'),
    (')', '0'),
    ('_', '-'),
    ('+', '='),
    ('{', '['),
    ('}', ']'),
    ('|', '\\'),
    (':', ';'),
    ('"', '\''),
    ('<', ','),
    ('>', '.'),
    ('?', '/'),
    ('~', '`'),
];

const NAMED: &[(Keysym, u16)] = &[
    (0xff0d, kvk::RETURN),
    (0xff8d, 0x4c), // KP_Enter
    (0xff09, kvk::TAB),
    (0xff1b, 0x35),
    (0xff08, 0x33), // BackSpace: Delete
    (0xffff, 0x75), // Delete: Forward Delete
    (0xff63, 0x72), // Insert: Help, where Insert is on PC keyboards
    (0xff50, 0x73),
    (0xff57, 0x77),
    (0xff55, 0x74),
    (0xff56, 0x79),
    (0xff51, 0x7b),
    (0xff52, 0x7e),
    (0xff53, 0x7c),
    (0xff54, 0x7d),
    (0xffe1, 0x38),
    (0xffe2, 0x3c),
    (0xffe3, 0x3b),
    (0xffe4, 0x3e),
    (0xffe9, 0x3a),
    (0xffea, 0x3d),
    (0xffeb, 0x37),
    (0xffec, 0x36),
    (0xffe5, 0x39),
    (0x1008ff13, 0x48),
    (0x1008ff11, 0x49),
    (0x1008ff12, 0x4a),
];

/// F1..F20.
const FUNCTION: [u16; 20] = [
    0x7a, 0x78, 0x63, 0x76, 0x60, 0x61, 0x62, 0x64, 0x65, 0x6d, 0x67, 0x6f, 0x69, 0x6b, 0x71, 0x6a, 0x40, 0x4f, 0x50,
    0x5a,
];

/// The key for a keysym, if the ANSI keyboard has one.
pub fn key(k: Keysym) -> Option<Key> {
    if let Some((_, code)) = NAMED.iter().find(|(s, _)| *s == k) {
        return Some(Key { code: *code, shift: false });
    }
    if (0xffbe..0xffbe + FUNCTION.len() as u32).contains(&k) {
        return Some(Key { code: FUNCTION[(k - 0xffbe) as usize], shift: false });
    }
    let c = char::from_u32(k).filter(char::is_ascii)?;
    let (base, shift) = match SHIFTED.iter().find(|(s, _)| *s == c) {
        Some((_, base)) => (*base, true),
        None if c.is_ascii_uppercase() => (c.to_ascii_lowercase(), true),
        None => (c, false),
    };
    CHARS.iter().find(|(ch, _)| *ch == base).map(|(_, code)| Key { code: *code, shift })
}

/// Typing: newlines and tabs are their keys (apps act on them, a
/// character would not do), everything else a Unicode key event each.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TextEvent {
    Key(u16),
    Unicode(Vec<u16>),
}

pub fn text_events(text: &str) -> Vec<TextEvent> {
    let mut out = Vec::new();
    let mut chars = text.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '\r' if chars.peek() == Some(&'\n') => {}
            '\n' | '\r' => out.push(TextEvent::Key(kvk::RETURN)),
            '\t' => out.push(TextEvent::Key(kvk::TAB)),
            c => {
                let mut units = [0; 2];
                out.push(TextEvent::Unicode(c.encode_utf16(&mut units).to_vec()));
            }
        }
    }
    out
}

/// Lines per wheel notch, about what a notch of a real wheel scrolls.
pub const LINES_PER_NOTCH: i32 = 3;

/// The scroll-wheel events (in lines: vertical, horizontal) for `dx`, `dy`
/// notches (+dy down, +dx right), one per notch. Quartz's wheels count up
/// for up and left.
pub fn scroll_events(dx: i32, dy: i32) -> Vec<(i32, i32)> {
    let n = dx.abs().max(dy.abs());
    (0..n)
        .map(|i| {
            let step = |d: i32| if i < d.abs() { -d.signum() * LINES_PER_NOTCH } else { 0 };
            (step(dy), step(dx))
        })
        .collect()
}

/// The controlled display: where it is in Quartz's global space (points,
/// the main display's top left the origin) and its size in points and
/// pixels (twice the points on Retina, unless scaled).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Display {
    pub origin: (f64, f64),
    pub points: (f64, f64),
    pub pixels: (u32, u32),
}

impl Display {
    /// Pixels per point.
    pub fn scale(&self) -> f64 {
        if self.points.0 > 0.0 {
            self.pixels.0 as f64 / self.points.0
        } else {
            1.0
        }
    }

    /// A pixel's centre in global points.
    pub fn to_points(&self, x: i32, y: i32) -> (f64, f64) {
        let s = self.scale();
        (self.origin.0 + (x as f64 + 0.5) / s, self.origin.1 + (y as f64 + 0.5) / s)
    }

    /// The pixel at a global point.
    pub fn to_pixels(&self, x: f64, y: f64) -> (i32, i32) {
        let s = self.scale();
        (((x - self.origin.0) * s).floor() as i32, ((y - self.origin.1) * s).floor() as i32)
    }
}

/// What to call the app macOS asks about (it gives permissions to the app
/// that started the helper, not the helper): from the bundle id
/// LaunchServices puts in the environment of an app it opens.
pub fn responsible_app(bundle_id: Option<&str>, term_program: Option<&str>) -> String {
    let known = |id: &str| match id {
        "io.github.evan-a-w.prigh" => Some("prigh (the desktop app)"),
        "com.apple.Terminal" => Some("Terminal"),
        "com.googlecode.iterm2" => Some("iTerm"),
        "com.mitchellh.ghostty" => Some("Ghostty"),
        "net.kovidgoyal.kitty" => Some("kitty"),
        "org.alacritty" => Some("Alacritty"),
        "dev.warp.Warp-Stable" => Some("Warp"),
        "com.github.wez.wezterm" => Some("WezTerm"),
        "com.microsoft.VSCode" => Some("Visual Studio Code"),
        _ => None,
    };
    match (bundle_id.filter(|b| !b.is_empty()), term_program.filter(|t| !t.is_empty())) {
        (Some(id), _) => known(id).map_or_else(|| format!("the app that started prigh ({id})"), str::to_string),
        (None, Some(t)) => format!("your terminal ({t})"),
        (None, None) => "the app that started prigh (the prigh desktop app, or your terminal)".into(),
    }
}

/// Why a permission is missing, and where to give it.
pub fn permission_error(what: &str, pane: &str, app: &str, after: &str) -> String {
    format!(
        "macOS does not let prigh {what} yet: in System Settings > Privacy & Security > {pane}, turn on {app} (macOS \
         asks about it the first time), then {after}"
    )
}

/// Keys for a combo's keysym, or the text it types when there is no key.
pub fn combo_key(k: Keysym) -> Result<Key, Option<char>> {
    key(k).ok_or_else(|| {
        let c = if k >= 0x0100_0000 { char::from_u32(k - 0x0100_0000) } else { char::from_u32(k) };
        c.filter(|c| of_char(*c) == k)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::keysym::{of_name, parse_combo};

    fn show(combo: &str) -> String {
        let keys = parse_combo(combo).unwrap().keys;
        let held: Vec<Keysym> = keys.iter().copied().filter(|k| modifier_flag(*k) != 0).collect();
        let parts: Vec<String> = keys
            .iter()
            .map(|k| match combo_key(*k) {
                Ok(Key { code, shift }) => format!("{code:#04x}{}", if shift { "⇧" } else { "" }),
                Err(Some(c)) => format!("text {c:?}"),
                Err(None) => "none".into(),
            })
            .collect();
        format!("{} flags {:#x}", parts.join(" "), flags(&held))
    }

    #[test]
    fn combos() {
        assert_eq!(show("cmd+c"), "0x37 0x08 flags 0x100000");
        assert_eq!(show("super+c"), "0x37 0x08 flags 0x100000");
        assert_eq!(show("cmd+shift+t"), "0x37 0x38 0x11 flags 0x120000");
        assert_eq!(show("ctrl+alt+Delete"), "0x3b 0x3a 0x75 flags 0xc0000");
        assert_eq!(show("option+Left"), "0x3a 0x7b flags 0x80000");
        assert_eq!(show("cmd+space"), "0x37 0x31 flags 0x100000");
        assert_eq!(show("Return"), "0x24 flags 0x0");
        assert_eq!(show("BackSpace"), "0x33 flags 0x0");
        assert_eq!(show("esc"), "0x35 flags 0x0");
        assert_eq!(show("pgdn"), "0x79 flags 0x0");
        assert_eq!(show("F1"), "0x7a flags 0x0");
        assert_eq!(show("F12"), "0x6f flags 0x0");
        assert_eq!(show("F20"), "0x5a flags 0x0");
        assert_eq!(show("F21"), "none flags 0x0");
        assert_eq!(show("cmd+A"), "0x37 0x00⇧ flags 0x100000");
        assert_eq!(show("cmd++"), "0x37 0x18⇧ flags 0x100000");
        assert_eq!(show("cmd+?"), "0x37 0x2c⇧ flags 0x100000");
        assert_eq!(show("é"), "text 'é' flags 0x0");
        assert_eq!(show("ctrl+€"), "0x3b text '€' flags 0x40000");
        assert_eq!(show("volumeup"), "0x48 flags 0x0");
        assert_eq!(show("Print"), "none flags 0x0");
        // every ASCII character but DEL and the controls has a key
        for c in ' '..='~' {
            assert!(key(of_char(c)).is_some(), "{c:?}");
        }
        // and distinct characters distinct keys (or shift)
        let mut seen = std::collections::HashSet::new();
        for c in ' '..='~' {
            assert!(seen.insert(key(of_char(c)).unwrap()), "{c:?}");
        }
        assert_eq!(modifier_flag(of_name("caps").unwrap()), flag::CAPS_LOCK);
    }

    #[test]
    fn text() {
        use TextEvent::*;
        assert_eq!(
            text_events("hi\r\nwö\t😀\r"),
            vec![
                Unicode(vec![0x68]),
                Unicode(vec![0x69]),
                Key(0x24),
                Unicode(vec![0x77]),
                Unicode(vec![0xf6]),
                Key(0x30),
                Unicode(vec![0xd83d, 0xde00]),
                Key(0x24),
            ]
        );
    }

    #[test]
    fn scrolls() {
        assert_eq!(scroll_events(0, 2), vec![(-3, 0), (-3, 0)]);
        assert_eq!(scroll_events(0, -1), vec![(3, 0)]);
        assert_eq!(scroll_events(1, -2), vec![(3, -3), (3, 0)]);
        assert_eq!(scroll_events(-1, 0), vec![(0, 3)]);
        assert_eq!(scroll_events(0, 0), vec![]);
    }

    #[test]
    fn mouse() {
        use event_type::*;
        assert_eq!(move_event(&[]), MOUSE_MOVED);
        assert_eq!(move_event(&[Button::Left]), LEFT_MOUSE_DRAGGED);
        assert_eq!(move_event(&[Button::Right]), RIGHT_MOUSE_DRAGGED);
        assert_eq!(move_event(&[Button::Middle]), OTHER_MOUSE_DRAGGED);
        assert_eq!(button_event(Button::Left, true), LEFT_MOUSE_DOWN);
        assert_eq!(button_event(Button::Right, false), RIGHT_MOUSE_UP);
        assert_eq!(button_event(Button::Back, true), OTHER_MOUSE_DOWN);
        assert_eq!(button_number(Button::Forward), 4);
    }

    #[test]
    fn clicks() {
        let mut c = Clicks::default();
        let t = Instant::now();
        let ms = |n| t + Duration::from_millis(n);
        // the driver's triple click: presses 40ms apart
        assert_eq!((c.press(Button::Left, (10, 10), t), c.release(Button::Left, ms(1))), (1, 1));
        assert_eq!((c.press(Button::Left, (10, 10), ms(41)), c.release(Button::Left, ms(42))), (2, 2));
        assert_eq!((c.press(Button::Left, (11, 9), ms(82)), c.release(Button::Left, ms(83))), (3, 3));
        // too late: a new click
        assert_eq!(c.press(Button::Left, (11, 9), ms(700)), 1);
        // elsewhere
        c.release(Button::Left, ms(701));
        assert_eq!(c.press(Button::Left, (30, 9), ms(710)), 1);
        // another button
        c.release(Button::Left, ms(711));
        assert_eq!(c.press(Button::Right, (30, 9), ms(720)), 1);
        // a drag's release, long after its press, keeps the press's count
        assert_eq!(c.release(Button::Right, ms(5000)), 1);
    }

    #[test]
    fn retina() {
        // a MacBook's built-in display at its default "looks like 1512×982"
        let d = Display { origin: (0.0, 0.0), points: (1512.0, 982.0), pixels: (3024, 1964) };
        assert_eq!(d.scale(), 2.0);
        assert_eq!(d.to_points(1000, 501), (500.25, 250.75));
        assert_eq!(d.to_pixels(500.0, 250.5), (1000, 501));
        assert_eq!(d.to_pixels(500.4, 250.9), (1000, 501));
        for x in 0..3024 {
            let (px, py) = d.to_points(x, x / 2);
            assert_eq!(d.to_pixels(px, py), (x, x / 2));
        }
        // a 4K display scaled to "looks like 2560×1440" (1.5 pixels a point),
        // right of the main one
        let d = Display { origin: (1512.0, -200.0), points: (2560.0, 1440.0), pixels: (3840, 2160) };
        assert_eq!(d.scale(), 1.5);
        assert_eq!(d.to_points(0, 0), (1512.0 + 1.0 / 3.0, -200.0 + 1.0 / 3.0));
        let (px, py) = d.to_points(2, 299);
        assert!((px - (1513.0 + 2.0 / 3.0)).abs() < 1e-9 && (py + 1.0 / 3.0).abs() < 1e-9, "{px} {py}");
        assert_eq!(d.to_pixels(1514.0, 0.0), (3, 300));
        for x in 0..3840 {
            let (px, py) = d.to_points(x, 2159);
            assert_eq!(d.to_pixels(px, py), (x, 2159));
        }
        // with the screen's pixels mapped to the model's space
        let m = crate::scale::Mapping::new(&crate::scale::Limits::default(), 3024, 1964);
        assert_eq!((m.scaled_w, m.scaled_h), (1280, 831));
        let (rx, ry) = m.to_real(640, 415).unwrap();
        assert_eq!((rx, ry), (1512, 980));
        assert_eq!(d.to_pixels(d.to_points(rx, ry).0, d.to_points(rx, ry).1), (rx, ry));
        // a pointer off the display (on another one) is still placed
        let main = Display { origin: (0.0, 0.0), points: (1512.0, 982.0), pixels: (3024, 1964) };
        assert_eq!(main.to_pixels(-10.0, 5.25), (-20, 10));
    }

    #[test]
    fn apps() {
        assert_eq!(responsible_app(Some("io.github.evan-a-w.prigh"), None), "prigh (the desktop app)");
        assert_eq!(responsible_app(Some("com.apple.Terminal"), Some("Apple_Terminal")), "Terminal");
        assert_eq!(responsible_app(Some("org.example.x"), None), "the app that started prigh (org.example.x)");
        assert_eq!(responsible_app(None, Some("tmux")), "your terminal (tmux)");
        assert_eq!(
            responsible_app(Some(""), None),
            "the app that started prigh (the prigh desktop app, or your terminal)"
        );
        assert_eq!(
            permission_error("record the screen", "Screen Recording", "Terminal", "quit and reopen Terminal"),
            "macOS does not let prigh record the screen yet: in System Settings > Privacy & Security > Screen \
             Recording, turn on Terminal (macOS asks about it the first time), then quit and reopen Terminal"
        );
    }
}
