//! Keys as X keysyms (what both X11 and xkb, so wlroots, speak), and the
//! key combos the model writes: xdotool's names ("ctrl+shift+t",
//! "Return", "Page_Down") with the aliases models use ("enter", "esc",
//! "cmd", "pgdn"), case-insensitive.

pub type Keysym = u32;

/// Keysyms with names, beyond single characters. The first name is the xkb
/// name written into keymaps; the others are aliases accepted in combos.
const NAMED: &[(Keysym, &[&str])] = &[
    (0xff0d, &["Return", "enter", "ret", "kp_enter_main"]),
    (0xff1b, &["Escape", "esc"]),
    (0xff09, &["Tab"]),
    (0xff08, &["BackSpace", "backspace", "bksp"]),
    (0xffff, &["Delete", "del"]),
    (0xff63, &["Insert", "ins"]),
    (0xff50, &["Home"]),
    (0xff57, &["End"]),
    (0xff55, &["Prior", "page_up", "pageup", "pgup"]),
    (0xff56, &["Next", "page_down", "pagedown", "pgdn"]),
    (0xff51, &["Left", "arrowleft", "arrow_left"]),
    (0xff52, &["Up", "arrowup", "arrow_up"]),
    (0xff53, &["Right", "arrowright", "arrow_right"]),
    (0xff54, &["Down", "arrowdown", "arrow_down"]),
    (0xffe1, &["Shift_L", "shift"]),
    (0xffe2, &["Shift_R"]),
    (0xffe3, &["Control_L", "ctrl", "control"]),
    (0xffe4, &["Control_R"]),
    (0xffe9, &["Alt_L", "alt", "option", "opt"]),
    (0xffea, &["Alt_R", "altgr"]),
    (0xffeb, &["Super_L", "super", "meta", "cmd", "command", "win", "windows", "logo"]),
    (0xffec, &["Super_R"]),
    (0xffe5, &["Caps_Lock", "capslock", "caps"]),
    (0xff7f, &["Num_Lock", "numlock"]),
    (0xff14, &["Scroll_Lock", "scrolllock"]),
    (0xff61, &["Print", "printscreen", "prtsc"]),
    (0xff13, &["Pause"]),
    (0xff67, &["Menu", "contextmenu", "apps"]),
    (0x0020, &["space", "spacebar"]),
    (0x002b, &["plus"]),
    (0x002d, &["minus"]),
    (0x003d, &["equal"]),
    (0x002c, &["comma"]),
    (0x002e, &["period"]),
    (0x002f, &["slash"]),
    (0x005c, &["backslash"]),
    (0x003b, &["semicolon"]),
    (0x0027, &["apostrophe", "quote"]),
    (0x0060, &["grave", "backtick"]),
    (0x005b, &["bracketleft"]),
    (0x005d, &["bracketright"]),
    (0x1008ff13, &["XF86AudioRaiseVolume", "volumeup", "audiovolumeup"]),
    (0x1008ff11, &["XF86AudioLowerVolume", "volumedown", "audiovolumedown"]),
    (0x1008ff12, &["XF86AudioMute", "volumemute", "audiovolumemute"]),
];

/// The modifier keysyms: held rather than typed.
pub fn is_modifier(k: Keysym) -> bool {
    matches!(k, 0xffe1..=0xffee)
}

/// Function keys F1..F35 are contiguous from 0xffbe.
fn function_key(name: &str) -> Option<Keysym> {
    let n: u32 = name.strip_prefix(['f', 'F'])?.parse().ok()?;
    (1..=35).contains(&n).then(|| 0xffbe + n - 1)
}

/// The keysym typing `c` produces: Latin-1 is its code point, the rest
/// Unicode keysyms (0x01000000 + code point), as xkb and X agree.
pub fn of_char(c: char) -> Keysym {
    match c {
        '\n' | '\r' => 0xff0d,
        '\t' => 0xff09,
        c if (' '..='~').contains(&c) || ('\u{a0}'..='\u{ff}').contains(&c) => c as u32,
        c => 0x0100_0000 + c as u32,
    }
}

/// A key name in a combo: a single character, a function key, or a name.
pub fn of_name(name: &str) -> Option<Keysym> {
    let mut chars = name.chars();
    if let (Some(c), None) = (chars.next(), chars.next()) {
        return Some(of_char(c));
    }
    if let Some(k) = function_key(name) {
        return Some(k);
    }
    NAMED.iter().find(|(_, names)| names.iter().any(|n| n.eq_ignore_ascii_case(name))).map(|(k, _)| *k)
}

/// The name xkb knows `k` by, for keymaps: the table's, a letter or digit
/// itself, a function key, else Unicode's `Uxxxx` or the hex value.
pub fn xkb_name(k: Keysym) -> String {
    if let Some((_, names)) = NAMED.iter().find(|(s, _)| *s == k) {
        return names[0].to_string();
    }
    if (0xffbe..0xffbe + 35).contains(&k) {
        return format!("F{}", k - 0xffbe + 1);
    }
    match char::from_u32(k) {
        Some(c) if c.is_ascii_alphanumeric() => return c.to_string(),
        _ => {}
    }
    if (0x20..=0xff).contains(&k) {
        return format!("U{k:04X}");
    }
    if k >= 0x0100_0000 {
        return format!("U{:04X}", k - 0x0100_0000);
    }
    format!("0x{k:08x}")
}

/// Keys pressed together, modifiers first, released in reverse.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Combo {
    pub keys: Vec<Keysym>,
    pub text: String,
}

/// One combo: names joined by `+` ("ctrl+shift+t"; "ctrl++" is ctrl and
/// plus).
pub fn parse_combo(s: &str) -> Result<Combo, String> {
    let s = s.trim();
    if s.is_empty() {
        return Err("an empty key combo: give keys like \"ctrl+shift+t\" or \"Return\"".into());
    }
    let mut names: Vec<&str> = Vec::new();
    let mut rest = s;
    while !rest.is_empty() {
        match rest.find('+') {
            Some(0) => {
                names.push("+");
                rest = rest[1..].strip_prefix('+').unwrap_or(&rest[1..]);
            }
            Some(i) => {
                names.push(&rest[..i]);
                rest = &rest[i + 1..];
            }
            None => {
                names.push(rest);
                rest = "";
            }
        }
    }
    let keys = names
        .iter()
        .map(|n| {
            of_name(n.trim()).ok_or_else(|| {
                format!(
                    "unknown key {n:?} in {s:?}: use a character or a name such as Return, Escape, Tab, BackSpace, \
                     Delete, Home, End, Page_Up, Page_Down, Left, Up, Right, Down, F1..F35, space, ctrl, shift, alt, \
                     super"
                )
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Combo { keys, text: s.to_string() })
}

/// Combos separated by whitespace, pressed one after another
/// ("ctrl+a Delete").
pub fn parse_combos(s: &str) -> Result<Vec<Combo>, String> {
    let combos = s.split_whitespace().map(parse_combo).collect::<Result<Vec<_>, _>>()?;
    if combos.is_empty() {
        return parse_combo("").map(|c| vec![c]);
    }
    Ok(combos)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn show(s: &str) -> String {
        match parse_combos(s) {
            Ok(cs) => cs
                .iter()
                .map(|c| c.keys.iter().map(|k| xkb_name(*k)).collect::<Vec<_>>().join("+"))
                .collect::<Vec<_>>()
                .join(" "),
            Err(e) => format!("error: {e}"),
        }
    }

    #[test]
    fn combos() {
        assert_eq!(show("ctrl+shift+t"), "Control_L+Shift_L+t");
        assert_eq!(show("CTRL+Shift+T"), "Control_L+Shift_L+T");
        assert_eq!(show("enter"), "Return");
        assert_eq!(show("Return"), "Return");
        assert_eq!(show("cmd+space"), "Super_L+space");
        assert_eq!(show("ctrl++"), "Control_L+plus");
        assert_eq!(show("+"), "plus");
        assert_eq!(show("ctrl+a Delete"), "Control_L+a Delete");
        assert_eq!(show("alt+F4"), "Alt_L+F4");
        assert_eq!(show("pgdn esc"), "Next Escape");
        assert_eq!(show("é"), "U00E9");
        assert_eq!(show("ctrl+€"), "Control_L+U20AC");
        assert!(show("ctrl+bogus").starts_with("error: unknown key \"bogus\""));
        assert!(show("  ").starts_with("error: an empty key combo"));
    }

    #[test]
    fn chars() {
        assert_eq!(of_char('a'), 0x61);
        assert_eq!(of_char('\n'), 0xff0d);
        assert_eq!(of_char('ü'), 0xfc);
        assert_eq!(of_char('€'), 0x10020ac);
        assert_eq!(xkb_name(of_char('!')), "U0021");
        assert_eq!(xkb_name(of_char('A')), "A");
        assert!(is_modifier(of_name("ctrl").unwrap()));
        assert!(!is_modifier(of_name("t").unwrap()));
    }
}
