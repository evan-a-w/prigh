//! The little of libxkbcommon the virtual keyboard needs: compiling a
//! layout (from rules, model, layout, variant and options, or keymap text),
//! writing it back as text, finding the key and level of a keysym, and the
//! modifier state that pressing and releasing keys leaves, as a physical
//! keyboard's would be.

use std::ffi::{c_char, c_int, c_void, CStr, CString};
use std::ptr::NonNull;

#[repr(C)]
struct RuleNames {
    rules: *const c_char,
    model: *const c_char,
    layout: *const c_char,
    variant: *const c_char,
    options: *const c_char,
}

const FORMAT_TEXT_V1: c_int = 1;
const MODS_DEPRESSED: c_int = 1;
const MODS_LATCHED: c_int = 2;
const MODS_LOCKED: c_int = 4;
const LAYOUT_EFFECTIVE: c_int = 1 << 7;

#[link(name = "xkbcommon")]
extern "C" {
    fn xkb_context_new(flags: c_int) -> *mut c_void;
    fn xkb_context_unref(ctx: *mut c_void);
    fn xkb_keymap_new_from_names(ctx: *mut c_void, names: *const RuleNames, flags: c_int) -> *mut c_void;
    fn xkb_keymap_new_from_string(ctx: *mut c_void, s: *const c_char, format: c_int, flags: c_int) -> *mut c_void;
    fn xkb_keymap_get_as_string(keymap: *mut c_void, format: c_int) -> *mut c_char;
    fn xkb_keymap_unref(keymap: *mut c_void);
    fn xkb_keymap_min_keycode(keymap: *mut c_void) -> u32;
    fn xkb_keymap_max_keycode(keymap: *mut c_void) -> u32;
    fn xkb_keymap_num_levels_for_key(keymap: *mut c_void, key: u32, layout: u32) -> u32;
    fn xkb_keymap_key_get_syms_by_level(
        keymap: *mut c_void,
        key: u32,
        layout: u32,
        level: u32,
        syms: *mut *const u32,
    ) -> c_int;
    fn xkb_keymap_key_get_mods_for_level(
        keymap: *mut c_void,
        key: u32,
        layout: u32,
        level: u32,
        masks: *mut u32,
        size: usize,
    ) -> usize;
    fn xkb_keymap_mod_get_index(keymap: *mut c_void, name: *const c_char) -> u32;
    fn xkb_state_new(keymap: *mut c_void) -> *mut c_void;
    fn xkb_state_unref(state: *mut c_void);
    fn xkb_state_update_key(state: *mut c_void, key: u32, direction: c_int) -> c_int;
    fn xkb_state_serialize_mods(state: *mut c_void, components: c_int) -> u32;
    fn xkb_state_serialize_layout(state: *mut c_void, components: c_int) -> u32;
}

/// What a layout is made of (xkb's RMLVO); empty fields take xkbcommon's
/// defaults (`$XKB_DEFAULT_LAYOUT`, ..., else evdev, pc105, us).
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Names {
    pub rules: String,
    pub model: String,
    pub layout: String,
    pub variant: String,
    pub options: String,
}

struct Context(NonNull<c_void>);

impl Context {
    fn new() -> Result<Context, String> {
        NonNull::new(unsafe { xkb_context_new(0) }).map(Context).ok_or_else(|| "xkbcommon: no context".to_string())
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe { xkb_context_unref(self.0.as_ptr()) }
    }
}

pub struct Keymap(NonNull<c_void>);

/// Where a keysym is: its xkb keycode and whether it needs Shift.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Place {
    pub keycode: u32,
    pub shift: bool,
}

impl Keymap {
    pub fn from_names(names: &Names) -> Result<Keymap, String> {
        let c = |s: &str| CString::new(s).map_err(|_| format!("a NUL in the keyboard layout {s:?}"));
        let (r, m, l, v, o) =
            (c(&names.rules)?, c(&names.model)?, c(&names.layout)?, c(&names.variant)?, c(&names.options)?);
        let p = |s: &CString| if s.is_empty() { std::ptr::null() } else { s.as_ptr() };
        let rmlvo = RuleNames { rules: p(&r), model: p(&m), layout: p(&l), variant: p(&v), options: p(&o) };
        let ctx = Context::new()?;
        NonNull::new(unsafe { xkb_keymap_new_from_names(ctx.0.as_ptr(), &rmlvo, 0) })
            .map(Keymap)
            .ok_or_else(|| format!("xkbcommon cannot compile the keyboard layout {names:?}"))
    }

    pub fn from_text(text: &str) -> Result<Keymap, String> {
        let s = CString::new(text).map_err(|_| "a NUL in a keymap".to_string())?;
        let ctx = Context::new()?;
        NonNull::new(unsafe { xkb_keymap_new_from_string(ctx.0.as_ptr(), s.as_ptr(), FORMAT_TEXT_V1, 0) })
            .map(Keymap)
            .ok_or_else(|| "xkbcommon cannot compile the keymap".to_string())
    }

    pub fn text(&self) -> String {
        unsafe {
            let p = xkb_keymap_get_as_string(self.0.as_ptr(), FORMAT_TEXT_V1);
            if p.is_null() {
                return String::new();
            }
            let s = CStr::from_ptr(p).to_string_lossy().into_owned();
            libc::free(p as *mut c_void);
            s
        }
    }

    pub fn max_keycode(&self) -> u32 {
        unsafe { xkb_keymap_max_keycode(self.0.as_ptr()) }
    }

    /// The modifier mask bit of a modifier by its name ("Shift", "Mod4").
    pub fn mod_mask(&self, name: &str) -> u32 {
        let n = CString::new(name).unwrap();
        let i = unsafe { xkb_keymap_mod_get_index(self.0.as_ptr(), n.as_ptr()) };
        if i < 32 {
            1 << i
        } else {
            0
        }
    }

    /// Every keysym reachable in the first layout with no modifiers or with
    /// Shift alone, at the lowest keycode, unshifted first.
    pub fn places(&self) -> Vec<(u32, Place)> {
        let k = self.0.as_ptr();
        let shift = self.mod_mask("Shift");
        let mut out: Vec<(u32, Place)> = Vec::new();
        let (min, max) = unsafe { (xkb_keymap_min_keycode(k), xkb_keymap_max_keycode(k)) };
        for pass_shift in [false, true] {
            for code in min..=max {
                for level in 0..unsafe { xkb_keymap_num_levels_for_key(k, code, 0) } {
                    let mut masks = [0u32; 16];
                    let n = unsafe { xkb_keymap_key_get_mods_for_level(k, code, 0, level, masks.as_mut_ptr(), 16) };
                    let want = if pass_shift { shift } else { 0 };
                    if !masks[..n.min(16)].contains(&want) {
                        continue;
                    }
                    let mut syms: *const u32 = std::ptr::null();
                    let count = unsafe { xkb_keymap_key_get_syms_by_level(k, code, 0, level, &mut syms) };
                    if count != 1 {
                        continue;
                    }
                    let sym = unsafe { *syms };
                    if !out.iter().any(|(s, _)| *s == sym) {
                        out.push((sym, Place { keycode: code, shift: pass_shift }));
                    }
                }
            }
        }
        out
    }
}

impl Drop for Keymap {
    fn drop(&mut self) {
        unsafe { xkb_keymap_unref(self.0.as_ptr()) }
    }
}

/// A keyboard's state: which modifiers its pressed keys make.
pub struct State(NonNull<c_void>);

/// The modifiers as `wl_keyboard.modifiers` sends them.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Mods {
    pub depressed: u32,
    pub latched: u32,
    pub locked: u32,
    pub group: u32,
}

impl State {
    pub fn new(keymap: &Keymap) -> Result<State, String> {
        NonNull::new(unsafe { xkb_state_new(keymap.0.as_ptr()) }).map(State).ok_or_else(|| "xkbcommon: no state".into())
    }

    pub fn key(&mut self, keycode: u32, down: bool) {
        unsafe { xkb_state_update_key(self.0.as_ptr(), keycode, down as c_int) };
    }

    pub fn mods(&self) -> Mods {
        let s = self.0.as_ptr();
        unsafe {
            Mods {
                depressed: xkb_state_serialize_mods(s, MODS_DEPRESSED),
                latched: xkb_state_serialize_mods(s, MODS_LATCHED),
                locked: xkb_state_serialize_mods(s, MODS_LOCKED),
                group: xkb_state_serialize_layout(s, LAYOUT_EFFECTIVE),
            }
        }
    }
}

impl Drop for State {
    fn drop(&mut self) {
        unsafe { xkb_state_unref(self.0.as_ptr()) }
    }
}
