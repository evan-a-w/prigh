//! The xkb keymap a virtual keyboard uploads (as wtype does): one key per
//! keysym the helper has needed so far, each with that keysym alone, so any
//! character can be typed without depending on the user's layout.
//! Modifiers are sent as a modifier mask, not as keys.

use crate::keysym::{xkb_name, Keysym};

/// The keysyms in the keymap, in order: the n-th is keycode n+9 (xkb's
/// keycodes are evdev codes plus 8; evdev code 1 is the first usable one).
#[derive(Debug, Default, Clone)]
pub struct Keymap {
    keysyms: Vec<Keysym>,
}

impl Keymap {
    /// The xkb keycode for `k`, adding it if needed; the bool says whether
    /// it was added (the keymap must be uploaded again).
    pub fn keycode(&mut self, k: Keysym) -> (u32, bool) {
        match self.keysyms.iter().position(|s| *s == k) {
            Some(i) => (i as u32 + 9, false),
            None => {
                self.keysyms.push(k);
                (self.keysyms.len() as u32 + 8, true)
            }
        }
    }

    pub fn len(&self) -> usize {
        self.keysyms.len()
    }

    pub fn is_empty(&self) -> bool {
        self.keysyms.is_empty()
    }

    /// The evdev code a virtual keyboard sends for an xkb keycode.
    pub fn evdev(keycode: u32) -> u32 {
        keycode - 8
    }

    pub fn text(&self) -> String {
        let max = 9 + self.keysyms.len().max(1) - 1;
        let mut s = String::from("xkb_keymap {\nxkb_keycodes \"(unnamed)\" {\nminimum = 8;\n");
        s.push_str(&format!("maximum = {};\n", max.max(255)));
        for i in 0..self.keysyms.len() {
            s.push_str(&format!("<K{}> = {};\n", i + 1, i + 9));
        }
        s.push_str("};\nxkb_types \"(unnamed)\" { include \"complete\" };\n");
        s.push_str("xkb_compatibility \"(unnamed)\" { include \"complete\" };\n");
        s.push_str("xkb_symbols \"(unnamed)\" {\n");
        for (i, k) in self.keysyms.iter().enumerate() {
            s.push_str(&format!("key <K{}> {{[{}]}};\n", i + 1, xkb_name(*k)));
        }
        s.push_str("};\n};\n");
        s
    }
}

/// xkb's modifier mask bits for the modifier keysyms (Shift, Control, Mod1
/// = Alt, Mod4 = Super), as the compositor's keymap interprets them.
pub fn modifier_mask(k: Keysym) -> u32 {
    match k {
        0xffe1 | 0xffe2 => 1,
        0xffe3 | 0xffe4 => 4,
        0xffe9 | 0xffea => 8,
        0xffeb | 0xffec => 64,
        0xffe5 => 2,
        _ => 0,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::keysym::{of_char, of_name};

    #[test]
    fn grows_and_writes() {
        let mut m = Keymap::default();
        assert_eq!(m.keycode(of_char('h')), (9, true));
        assert_eq!(m.keycode(of_char('€')), (10, true));
        assert_eq!(m.keycode(of_char('h')), (9, false));
        assert_eq!(m.keycode(of_name("Return").unwrap()), (11, true));
        assert_eq!(Keymap::evdev(9), 1);
        let expected = "xkb_keymap {
xkb_keycodes \"(unnamed)\" {
minimum = 8;
maximum = 255;
<K1> = 9;
<K2> = 10;
<K3> = 11;
};
xkb_types \"(unnamed)\" { include \"complete\" };
xkb_compatibility \"(unnamed)\" { include \"complete\" };
xkb_symbols \"(unnamed)\" {
key <K1> {[h]};
key <K2> {[U20AC]};
key <K3> {[Return]};
};
};
";
        assert_eq!(m.text(), expected);
    }

    #[test]
    fn masks() {
        assert_eq!(modifier_mask(of_name("shift").unwrap()), 1);
        assert_eq!(modifier_mask(of_name("ctrl").unwrap()), 4);
        assert_eq!(modifier_mask(of_name("alt").unwrap()), 8);
        assert_eq!(modifier_mask(of_name("super").unwrap()), 64);
        assert_eq!(modifier_mask(of_char('a')), 0);
    }
}
