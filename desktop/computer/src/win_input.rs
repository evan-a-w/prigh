//! The Windows backend's pure parts ([crate::windows] does the calls), here
//! so they are tested on every platform: keysyms as virtual keys, text as
//! `KEYEVENTF_UNICODE` events, pointer positions in `SendInput`'s
//! normalized virtual-desktop space, wheel and button flags, pixel
//! conversion, and the errors for what Windows will not let a program do.

use crate::action::Button;
use crate::keysym::Keysym;

/// A key as `SendInput` sends it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Key {
    /// A virtual key; `extended` sets `KEYEVENTF_EXTENDEDKEY` (the arrows,
    /// navigation keys, right-hand modifiers...).
    Vk { vk: u16, extended: bool },
    /// A character whose virtual key depends on the keyboard layout: the
    /// backend asks `VkKeyScanW`, else types it with `KEYEVENTF_UNICODE`.
    Char(char),
}

const fn vk(vk: u16) -> Option<Key> {
    Some(Key::Vk { vk, extended: false })
}

const fn ext(vk: u16) -> Option<Key> {
    Some(Key::Vk { vk, extended: true })
}

pub const VK_SHIFT: u16 = 0x10;
pub const VK_CONTROL: u16 = 0x11;
pub const VK_MENU: u16 = 0x12;
pub const VK_RETURN: u16 = 0x0d;
pub const VK_TAB: u16 = 0x09;

/// The key for a keysym from a combo (`None`: not one Windows has).
pub fn key_of_keysym(k: Keysym) -> Option<Key> {
    match k {
        0xff0d => vk(VK_RETURN),
        0xff1b => vk(0x1b),
        0xff09 => vk(VK_TAB),
        0xff08 => vk(0x08),
        0xffff => ext(0x2e),
        0xff63 => ext(0x2d),
        0xff50 => ext(0x24),
        0xff57 => ext(0x23),
        0xff55 => ext(0x21),
        0xff56 => ext(0x22),
        0xff51 => ext(0x25),
        0xff52 => ext(0x26),
        0xff53 => ext(0x27),
        0xff54 => ext(0x28),
        0xffe1 => vk(0xa0),
        0xffe2 => vk(0xa1),
        0xffe3 => vk(0xa2),
        0xffe4 => ext(0xa3),
        0xffe9 => vk(0xa4),
        0xffea => ext(0xa5),
        0xffeb => ext(0x5b),
        0xffec => ext(0x5c),
        0xffe5 => vk(0x14),
        0xff7f => ext(0x90),
        0xff14 => vk(0x91),
        0xff61 => ext(0x2c),
        0xff13 => vk(0x13),
        0xff67 => ext(0x5d),
        0x20 => vk(0x20),
        // F1..F24 (Windows has no F25..F35)
        k if (0xffbe..0xffbe + 24).contains(&k) => vk(0x70 + (k - 0xffbe) as u16),
        0x1008ff13 => ext(0xaf),
        0x1008ff11 => ext(0xae),
        0x1008ff12 => ext(0xad),
        // letters and digits have their own virtual keys in every layout
        k if (b'a' as u32..=b'z' as u32).contains(&k) => vk(k as u16 - 0x20),
        k if (b'A' as u32..=b'Z' as u32).contains(&k) || (b'0' as u32..=b'9' as u32).contains(&k) => vk(k as u16),
        k if (0x21..=0xff).contains(&k) => char::from_u32(k).map(Key::Char),
        k if k >= 0x0100_0000 => char::from_u32(k - 0x0100_0000).map(Key::Char),
        _ => None,
    }
}

/// The modifiers `VkKeyScanW`'s high byte asks for (shift 1, ctrl 2, alt
/// 4), as virtual keys, pressed before the key.
pub fn scan_modifiers(state: u8) -> Vec<u16> {
    [(1, VK_SHIFT), (2, VK_CONTROL), (4, VK_MENU)].iter().filter(|(bit, _)| state & bit != 0).map(|&(_, v)| v).collect()
}

/// One keyboard event of typed text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextEvent {
    Vk { vk: u16, up: bool },
    /// A UTF-16 unit (`KEYEVENTF_UNICODE`); characters beyond the BMP are
    /// two, each pressed and released.
    Unicode { unit: u16, up: bool },
}

/// Typing `text`: newlines and tabs as their keys (so they act in every
/// app), every other character as Unicode, independent of the layout.
pub fn text_events(text: &str) -> Vec<TextEvent> {
    let mut out = Vec::new();
    let mut last_cr = false;
    for c in text.chars() {
        let key = match c {
            '\n' if last_cr => None,
            '\n' | '\r' => Some(VK_RETURN),
            '\t' => Some(VK_TAB),
            _ => {
                let mut buf = [0u16; 2];
                for &unit in c.encode_utf16(&mut buf).iter() {
                    out.push(TextEvent::Unicode { unit, up: false });
                    out.push(TextEvent::Unicode { unit, up: true });
                }
                None
            }
        };
        if let Some(vk) = key {
            out.push(TextEvent::Vk { vk, up: false });
            out.push(TextEvent::Vk { vk, up: true });
        }
        last_cr = c == '\r';
    }
    out
}

/// The virtual desktop: all monitors' bounding box, in physical pixels.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VirtualDesk {
    pub x: i32,
    pub y: i32,
    pub width: i32,
    pub height: i32,
}

impl VirtualDesk {
    /// `MOUSEEVENTF_ABSOLUTE | MOUSEEVENTF_VIRTUALDESK` coordinates
    /// (0..=65535 across the virtual desktop) for a desktop pixel: the
    /// smallest that Windows maps back to it (it takes `n * size / 65536`).
    pub fn normalize(&self, x: i32, y: i32) -> (i32, i32) {
        fn axis(v: i32, origin: i32, size: i32) -> i32 {
            if size <= 0 {
                return 0;
            }
            let p = (v - origin).clamp(0, size - 1) as i64;
            ((p * 65536 + size as i64 - 1) / size as i64).min(65535) as i32
        }
        (axis(x, self.x, self.width), axis(y, self.y, self.height))
    }

    /// Where Windows puts the pointer for normalized coordinates.
    pub fn denormalize(&self, nx: i32, ny: i32) -> (i32, i32) {
        (
            self.x + (nx as i64 * self.width as i64 / 65536) as i32,
            self.y + (ny as i64 * self.height as i64 / 65536) as i32,
        )
    }
}

pub const MOUSEEVENTF_WHEEL: u32 = 0x0800;
pub const MOUSEEVENTF_HWHEEL: u32 = 0x1000;
pub const WHEEL_DELTA: i32 = 120;

/// The wheel events for `dx`, `dy` notches (+dy down, +dx right): Windows'
/// vertical wheel is positive away from the user (up).
pub fn wheel(dx: i32, dy: i32) -> Vec<(u32, i32)> {
    let mut out = Vec::new();
    if dy != 0 {
        out.push((MOUSEEVENTF_WHEEL, -dy * WHEEL_DELTA));
    }
    if dx != 0 {
        out.push((MOUSEEVENTF_HWHEEL, dx * WHEEL_DELTA));
    }
    out
}

/// A button's `MOUSEINPUT` flags and `mouseData` (the X button's number).
pub fn button_flags(button: Button, down: bool) -> (u32, u32) {
    let (d, u, data) = match button {
        Button::Left => (0x0002, 0x0004, 0),
        Button::Right => (0x0008, 0x0010, 0),
        Button::Middle => (0x0020, 0x0040, 0),
        Button::Back => (0x0080, 0x0100, 1),
        Button::Forward => (0x0080, 0x0100, 2),
    };
    (if down { d } else { u }, data)
}

/// GDI's and DXGI's B8G8R8A8 rows (`stride` bytes apart) as RGBA, opaque.
pub fn bgra_to_rgba(src: &[u8], width: u32, height: u32, stride: usize) -> Result<image::RgbaImage, String> {
    let row = width as usize * 4;
    if stride < row || src.len() < stride * (height as usize).saturating_sub(1) + row {
        return Err(format!("a {width}×{height} capture came with {} bytes ({stride} a row)", src.len()));
    }
    let mut out = Vec::with_capacity(row * height as usize);
    for y in 0..height as usize {
        for px in src[y * stride..y * stride + row].chunks_exact(4) {
            out.extend_from_slice(&[px[2], px[1], px[0], 255]);
        }
    }
    image::RgbaImage::from_raw(width, height, out).ok_or_else(|| "capture size mismatch".into())
}

pub const SECURE_DESKTOP: &str = "the secure desktop is showing (a UAC prompt, the lock screen or Ctrl+Alt+Del): \
Windows lets no program see or drive it. Answer or unlock it yourself, then ask again";

/// The error for input the window at the target would not get: Windows'
/// User Interface Privilege Isolation drops input a program sends to
/// windows of a more privileged (elevated) one, without telling it.
pub fn uipi_blocked(what: &str, target: &str) -> String {
    format!(
        "{target} runs as administrator: Windows (UIPI) silently drops {what} that prigh-computer, which is not \
         elevated, sends to it. Use it yourself, or run the desktop app as administrator (then the agent can drive \
         elevated windows too)"
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::keysym::{of_char, of_name};

    fn k(name: &str) -> Option<Key> {
        key_of_keysym(of_name(name).unwrap())
    }

    #[test]
    fn keys() {
        assert_eq!(k("Return"), vk(0x0d));
        assert_eq!(k("ctrl"), vk(0xa2));
        assert_eq!(k("shift"), vk(0xa0));
        assert_eq!(k("alt"), vk(0xa4));
        assert_eq!(k("win"), ext(0x5b));
        assert_eq!(k("cmd"), ext(0x5b));
        assert_eq!(k("altgr"), ext(0xa5));
        assert_eq!(k("Left"), ext(0x25));
        assert_eq!(k("pgdn"), ext(0x22));
        assert_eq!(k("Delete"), ext(0x2e));
        assert_eq!(k("F1"), vk(0x70));
        assert_eq!(k("F24"), vk(0x87));
        assert_eq!(k("F25"), None);
        assert_eq!(k("a"), vk(0x41));
        assert_eq!(k("T"), vk(0x54));
        assert_eq!(k("7"), vk(0x37));
        assert_eq!(k("space"), vk(0x20));
        assert_eq!(k("plus"), Some(Key::Char('+')));
        assert_eq!(k("é"), Some(Key::Char('é')));
        assert_eq!(key_of_keysym(of_char('€')), Some(Key::Char('€')));
        assert_eq!(k("volumeup"), ext(0xaf));
        assert_eq!(scan_modifiers(0), Vec::<u16>::new());
        assert_eq!(scan_modifiers(1), vec![VK_SHIFT]);
        assert_eq!(scan_modifiers(6), vec![VK_CONTROL, VK_MENU]);
    }

    #[test]
    fn text() {
        let show = |s: &str| {
            text_events(s)
                .iter()
                .map(|e| match e {
                    TextEvent::Vk { vk, up } => format!("vk{vk:02x}{}", if *up { "↑" } else { "↓" }),
                    TextEvent::Unicode { unit, up } => format!("u{unit:04x}{}", if *up { "↑" } else { "↓" }),
                })
                .collect::<Vec<_>>()
                .join(" ")
        };
        assert_eq!(show("hé"), "u0068↓ u0068↑ u00e9↓ u00e9↑");
        assert_eq!(show("a\nb"), "u0061↓ u0061↑ vk0d↓ vk0d↑ u0062↓ u0062↑");
        assert_eq!(show("\r\n\t"), "vk0d↓ vk0d↑ vk09↓ vk09↑");
        assert_eq!(show("😀"), "ud83d↓ ud83d↑ ude00↓ ude00↑");
    }

    #[test]
    fn normalizes() {
        // two monitors: a 1920 one left of the primary 2560×1440 one
        let d = VirtualDesk { x: -1920, y: 0, width: 4480, height: 1440 };
        assert_eq!(d.normalize(-1920, 0), (0, 0));
        assert_eq!(d.normalize(4480 - 1920 - 1, 1439), (65522, 65491));
        assert_eq!(d.denormalize(65522, 65491), (2559, 1439));
        for (w, origin) in [(1920, 0), (4480, -1920), (3840, 0), (1366, 0), (7, 0), (65535, 0), (7680, -3840)] {
            let d = VirtualDesk { x: origin, y: 0, width: w, height: 1 };
            for x in origin..origin + w {
                let (nx, _) = d.normalize(x, 0);
                assert!((0..=65535).contains(&nx));
                assert_eq!(d.denormalize(nx, 0).0, x, "width {w}, x {x}");
            }
        }
        // off the desktop: clamped to its edge
        let d = VirtualDesk { x: 0, y: 0, width: 1920, height: 1080 };
        assert_eq!(d.normalize(-5, 5000), (0, d.normalize(0, 1079).1));
    }

    #[test]
    fn wheels_and_buttons() {
        assert_eq!(wheel(0, 2), vec![(MOUSEEVENTF_WHEEL, -240)]);
        assert_eq!(wheel(-1, -3), vec![(MOUSEEVENTF_WHEEL, 360), (MOUSEEVENTF_HWHEEL, -120)]);
        assert_eq!(button_flags(Button::Left, true), (0x0002, 0));
        assert_eq!(button_flags(Button::Right, false), (0x0010, 0));
        assert_eq!(button_flags(Button::Back, true), (0x0080, 1));
        assert_eq!(button_flags(Button::Forward, false), (0x0100, 2));
    }

    #[test]
    fn pixels() {
        // 2×2 with 4 bytes of row padding
        let src = [1, 2, 3, 0, 4, 5, 6, 0, 9, 9, 9, 9, 7, 8, 9, 0, 10, 11, 12, 0];
        let img = bgra_to_rgba(&src, 2, 2, 12).unwrap();
        assert_eq!(img.into_raw(), vec![3, 2, 1, 255, 6, 5, 4, 255, 9, 8, 7, 255, 12, 11, 10, 255]);
        assert!(bgra_to_rgba(&src, 3, 2, 12).unwrap_err().contains("3×2"));
        assert!(uipi_blocked("clicks", "\"Task Manager\"").starts_with("\"Task Manager\" runs as administrator"));
    }
}
