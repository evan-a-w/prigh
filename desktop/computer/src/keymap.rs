//! The virtual keyboard's keys, as a physical keyboard's would be: the
//! user's layout (the compositor's, when it says, else xkbcommon's
//! default) with its real evdev codes, modifier keys pressed and released
//! like any other, and the modifier state xkb derives from them.
//!
//! A character the layout lacks gets a key of its own, appended to the
//! layout at keycodes past every evdev code (`KEY_MAX`), so a compositor
//! that reads a virtual keyboard's keys through another keymap (Hyprland
//! matches binds through the configured layout) sees nothing there, never
//! Escape or a bound key. X clients (Xwayland) cannot get such keycodes:
//! there those characters do not type.

use crate::keysym::{xkb_name, Keysym};
use crate::xkb;
use std::collections::HashMap;

/// The first custom key's xkb keycode: evdev `KEY_MAX` + 1, plus 8.
pub const CUSTOM_BASE: u32 = 0x300 + 8;
/// Custom keys kept before the keymap starts again (when none is held).
const CUSTOM_MAX: usize = 240;

/// What the compositor must be sent, in order.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Event {
    Keymap(String),
    Key { evdev: u32, down: bool },
    Mods(xkb::Mods),
}

pub struct Keyboard {
    base: String,
    places: HashMap<Keysym, xkb::Place>,
    shift: Option<u32>,
    shift_mask: u32,
    custom: Vec<Keysym>,
    state: xkb::State,
    /// Keys held by [Keyboard::press], with the keycodes each pressed.
    held: Vec<(Keysym, Vec<u32>)>,
    sent: xkb::Mods,
    events: Vec<Event>,
}

/// The modifier a custom key for a modifier keysym sets (layouts have the
/// usual ones; this is for the others).
fn modifier_name(k: Keysym) -> Option<&'static str> {
    match k {
        0xffe1 | 0xffe2 => Some("Shift"),
        0xffe3 | 0xffe4 => Some("Control"),
        0xffe5 => Some("Lock"),
        0xffe7..=0xffea => Some("Mod1"),
        0xffeb..=0xffee => Some("Mod4"),
        _ => None,
    }
}

fn custom_name(i: usize) -> String {
    format!("Z{i:03}")
}

/// `base` (a keymap as xkbcommon writes it) with the custom keys added.
pub fn with_custom(base: &str, custom: &[Keysym]) -> Result<String, String> {
    if custom.is_empty() {
        return Ok(base.to_string());
    }
    let bad = || "the layout's keymap is not as xkbcommon writes it".to_string();
    let kc = base.find("xkb_keycodes").ok_or_else(bad)?;
    let max_at = kc + base[kc..].find("maximum = ").ok_or_else(bad)? + "maximum = ".len();
    let max_end = max_at + base[max_at..].find(';').ok_or_else(bad)?;
    let max: u32 = base[max_at..max_end].trim().parse().map_err(|_| bad())?;
    let last = CUSTOM_BASE + custom.len() as u32 - 1;
    let line_end = max_end + base[max_end..].find('\n').ok_or_else(bad)? + 1;
    let sym = base.find("xkb_symbols").ok_or_else(bad)?;
    let sym_body = sym + base[sym..].find('\n').ok_or_else(bad)? + 1;
    if sym_body < line_end {
        return Err(bad());
    }
    let mut out = String::with_capacity(base.len() + custom.len() * 64);
    out.push_str(&base[..max_at]);
    out.push_str(&max.max(last).to_string());
    out.push_str(&base[max_end..line_end]);
    for i in 0..custom.len() {
        out.push_str(&format!("\t<{}> = {};\n", custom_name(i), CUSTOM_BASE + i as u32));
    }
    out.push_str(&base[line_end..sym_body]);
    for (i, k) in custom.iter().enumerate() {
        out.push_str(&format!("\tkey <{}> {{ [ {} ] }};\n", custom_name(i), xkb_name(*k)));
        if let Some(m) = modifier_name(*k) {
            out.push_str(&format!("\tmodifier_map {m} {{ <{}> }};\n", custom_name(i)));
        }
    }
    out.push_str(&base[sym_body..]);
    Ok(out)
}

impl Keyboard {
    /// A keyboard with `layout`'s keys; its first event uploads the keymap.
    pub fn new(layout: &xkb::Keymap) -> Result<Keyboard, String> {
        let base = layout.text();
        let keymap = xkb::Keymap::from_text(&base)?;
        let places: HashMap<Keysym, xkb::Place> = keymap.places().into_iter().collect();
        let shift = places.get(&0xffe1).filter(|p| !p.shift).map(|p| p.keycode);
        let mut k = Keyboard {
            shift_mask: keymap.mod_mask("Shift"),
            state: xkb::State::new(&keymap)?,
            base,
            places,
            shift,
            custom: Vec::new(),
            held: Vec::new(),
            sent: xkb::Mods::default(),
            events: Vec::new(),
        };
        k.events.push(Event::Keymap(k.base.clone()));
        Ok(k)
    }

    pub fn take_events(&mut self) -> Vec<Event> {
        std::mem::take(&mut self.events)
    }

    fn held_codes(&self) -> impl Iterator<Item = u32> + '_ {
        self.held.iter().flat_map(|(_, c)| c.iter().copied())
    }

    fn key(&mut self, keycode: u32, down: bool) {
        self.state.key(keycode, down);
        self.events.push(Event::Key { evdev: keycode - 8, down });
        let mods = self.state.mods();
        if mods != self.sent {
            self.sent = mods;
            self.events.push(Event::Mods(mods));
        }
    }

    fn shift_down(&self) -> bool {
        self.state.mods().depressed & self.shift_mask != 0
    }

    /// Gives the keysyms the layout lacks custom keys, uploading the keymap
    /// once if any are new.
    pub fn prepare(&mut self, keys: &[Keysym]) -> Result<(), String> {
        let mut new: Vec<Keysym> = Vec::new();
        for &k in keys {
            if !self.places.contains_key(&k) && !self.custom.contains(&k) && !new.contains(&k) {
                new.push(k);
            }
        }
        if new.is_empty() {
            return Ok(());
        }
        if self.custom.len() + new.len() > CUSTOM_MAX && !self.held_codes().any(|c| c >= CUSTOM_BASE) {
            self.custom.clear();
        }
        self.custom.extend(new);
        let text = with_custom(&self.base, &self.custom)?;
        let keymap = xkb::Keymap::from_text(&text)?;
        let mut state = xkb::State::new(&keymap)?;
        for c in self.held_codes() {
            state.key(c, true);
        }
        self.state = state;
        self.events.push(Event::Keymap(text));
        // a new keymap starts with no modifiers: say again what is held
        self.sent = self.state.mods();
        self.events.push(Event::Mods(self.sent));
        Ok(())
    }

    /// The keycodes that make `k`: Shift first when its level needs it and
    /// Shift is not already down.
    fn codes(&mut self, k: Keysym) -> Result<Vec<u32>, String> {
        if let Some(p) = self.places.get(&k).copied() {
            return Ok(match self.shift {
                Some(s) if p.shift && !self.shift_down() => vec![s, p.keycode],
                _ => vec![p.keycode],
            });
        }
        self.prepare(&[k])?;
        let i = self.custom.iter().position(|c| *c == k).unwrap();
        Ok(vec![CUSTOM_BASE + i as u32])
    }

    /// Presses `k` and keeps it down (with any Shift it needed) until
    /// [Keyboard::release].
    pub fn press(&mut self, k: Keysym) -> Result<(), String> {
        let codes = self.codes(k)?;
        for &c in &codes {
            self.key(c, true);
        }
        self.held.push((k, codes));
        Ok(())
    }

    pub fn release(&mut self, k: Keysym) {
        if let Some(i) = self.held.iter().rposition(|(h, _)| *h == k) {
            let (_, codes) = self.held.remove(i);
            for &c in codes.iter().rev() {
                self.key(c, false);
            }
        }
    }

    /// Presses and releases the key typing `k`.
    pub fn tap(&mut self, k: Keysym) -> Result<(), String> {
        let codes = self.codes(k)?;
        for &c in &codes {
            self.key(c, true);
        }
        for &c in codes.iter().rev() {
            self.key(c, false);
        }
        Ok(())
    }

    /// Releases every held key, last pressed first.
    pub fn release_all(&mut self) {
        while let Some((k, _)) = self.held.last() {
            let k = *k;
            self.release(k);
        }
    }

    pub fn holding(&self) -> bool {
        !self.held.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::keysym::{of_char, of_name, parse_combo};

    fn layout(l: &str) -> Keyboard {
        let names = xkb::Names { layout: l.into(), ..Default::default() };
        Keyboard::new(&xkb::Keymap::from_names(&names).unwrap()).unwrap()
    }

    fn show(events: Vec<Event>) -> Vec<String> {
        events
            .into_iter()
            .map(|e| match e {
                Event::Keymap(_) => "keymap".to_string(),
                Event::Key { evdev, down } => format!("{} {evdev}", if down { "down" } else { "up" }),
                Event::Mods(m) => format!("mods {}/{}/{}/{}", m.depressed, m.latched, m.locked, m.group),
            })
            .collect()
    }

    fn combo(k: &mut Keyboard, s: &str) -> Vec<String> {
        let c = parse_combo(s).unwrap();
        for key in &c.keys {
            k.press(*key).unwrap();
        }
        for key in c.keys.iter().rev() {
            k.release(*key);
        }
        show(k.take_events())
    }

    #[test]
    fn combos_use_real_evdev_codes() {
        let mut k = layout("us");
        assert_eq!(show(k.take_events()), ["keymap"]);
        // KEY_LEFTMETA 125, KEY_D 32; Mod4 is bit 6
        assert_eq!(
            combo(&mut k, "super+d"),
            ["down 125", "mods 64/0/0/0", "down 32", "up 32", "up 125", "mods 0/0/0/0"]
        );
        // KEY_LEFTCTRL 29, KEY_LEFTSHIFT 42, KEY_T 20
        let ctrl_shift_t = [
            "down 29",
            "mods 4/0/0/0",
            "down 42",
            "mods 5/0/0/0",
            "down 20",
            "up 20",
            "up 42",
            "mods 4/0/0/0",
            "up 29",
            "mods 0/0/0/0",
        ];
        assert_eq!(combo(&mut k, "ctrl+shift+t"), ctrl_shift_t);
        assert_eq!(combo(&mut k, "ctrl+shift+T"), ctrl_shift_t);
        // a capital needs Shift, added (as xdotool does) when not held
        assert_eq!(combo(&mut k, "ctrl+T"), ctrl_shift_t);
        // KEY_LEFTALT 56, KEY_F4 62, KEY_ENTER 28, KEY_ESC 1, KEY_VOLUMEUP 115, KEY_EQUAL 13
        assert_eq!(combo(&mut k, "alt+F4"), ["down 56", "mods 8/0/0/0", "down 62", "up 62", "up 56", "mods 0/0/0/0"]);
        assert_eq!(combo(&mut k, "Return"), ["down 28", "up 28"]);
        assert_eq!(combo(&mut k, "Escape"), ["down 1", "up 1"]);
        assert_eq!(combo(&mut k, "XF86AudioRaiseVolume"), ["down 115", "up 115"]);
        assert_eq!(combo(&mut k, "+"), ["down 42", "mods 1/0/0/0", "down 13", "up 13", "up 42", "mods 0/0/0/0"]);
    }

    #[test]
    fn the_users_layout() {
        // German: z and y swap places (KEY_Y 21, KEY_Z 44)
        let mut k = layout("de");
        k.take_events();
        assert_eq!(
            combo(&mut k, "super+z"),
            ["down 125", "mods 64/0/0/0", "down 21", "up 21", "up 125", "mods 0/0/0/0"]
        );
        assert_eq!(combo(&mut k, "y"), ["down 44", "up 44"]);
        // ö is a key of its own there (KEY_SEMICOLON 39)
        assert_eq!(combo(&mut k, "ö"), ["down 39", "up 39"]);
    }

    #[test]
    fn custom_keys_are_past_every_evdev_code() {
        let mut k = layout("us");
        k.take_events();
        k.prepare(&"hé€é→".chars().map(of_char).collect::<Vec<_>>()).unwrap();
        let events = k.take_events();
        let Event::Keymap(text) = &events[0] else { panic!("{events:?}") };
        assert!(text.contains("\t<Z000> = 776;\n\t<Z001> = 777;\n\t<Z002> = 778;\n"), "{text}");
        assert!(
            text.contains("\tkey <Z000> { [ U00E9 ] };\n\tkey <Z001> { [ U20AC ] };\n\tkey <Z002> { [ U2192 ] };\n")
        );
        assert!(!text.contains("<Z003>"));
        assert_eq!(show(events)[1..], ["mods 0/0/0/0"]);
        for c in "hé€→".chars() {
            k.tap(of_char(c)).unwrap();
        }
        // h is KEY_H; the rest are custom, from evdev 768 (KEY_MAX + 1)
        assert_eq!(
            show(k.take_events()),
            ["down 35", "up 35", "down 768", "up 768", "down 769", "up 769", "down 770", "up 770"]
        );
        // capitals take Shift for just that key
        k.tap(of_char('H')).unwrap();
        assert_eq!(show(k.take_events()), ["down 42", "mods 1/0/0/0", "down 35", "up 35", "up 42", "mods 0/0/0/0"]);
    }

    #[test]
    fn custom_keys_never_land_on_a_layouts_key() {
        // no layout names a keycode past KEY_MAX, so another keymap reads
        // nothing there
        for l in ["us", "de", "fr", "ru", "jp", "us(dvorak)"] {
            let names = xkb::Names { layout: l.into(), ..Default::default() };
            let keymap = xkb::Keymap::from_names(&names).unwrap();
            assert!(keymap.max_keycode() < CUSTOM_BASE, "{l}: {}", keymap.max_keycode());
        }
    }

    #[test]
    fn a_custom_key_mid_combo_keeps_the_held_modifiers() {
        let mut k = layout("us");
        k.take_events();
        k.press(of_name("ctrl").unwrap()).unwrap();
        k.press(of_char('€')).unwrap();
        k.release_all();
        assert_eq!(
            show(k.take_events()),
            ["down 29", "mods 4/0/0/0", "keymap", "mods 4/0/0/0", "down 768", "up 768", "up 29", "mods 0/0/0/0"]
        );
    }

    #[test]
    fn release_all_lets_go_of_everything() {
        let mut k = layout("us");
        k.take_events();
        for key in parse_combo("ctrl+alt+shift+super+T").unwrap().keys {
            k.press(key).unwrap();
        }
        assert!(k.holding());
        k.take_events();
        k.release_all();
        assert_eq!(
            show(k.take_events()),
            [
                "up 20",
                "up 125",
                "mods 13/0/0/0",
                "up 42",
                "mods 12/0/0/0",
                "up 56",
                "mods 4/0/0/0",
                "up 29",
                "mods 0/0/0/0"
            ]
        );
        assert!(!k.holding());
        k.release_all();
        assert!(k.take_events().is_empty());
    }
}
