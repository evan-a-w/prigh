//! X11: screenshots with GetImage, input with XTEST, the pointer with
//! QueryPointer (so the kill switch works). The controlled area is RandR's
//! primary monitor (else the first, else the whole root window).
//!
//! Characters the keyboard map lacks are typed through spare keycodes
//! mapped to them (as xdotool does), kept until the helper exits, when they
//! are unmapped again.

use crate::action::Button;
use crate::backend::{draw_pointer, Backend, Screen};
use crate::keysym::{of_char, Keysym};
use image::RgbaImage;
use x11rb::connection::Connection;
use x11rb::protocol::randr::ConnectionExt as _;
use x11rb::protocol::xproto::{ConnectionExt as _, ImageFormat};
use x11rb::protocol::xtest::ConnectionExt as _;
use x11rb::rust_connection::RustConnection;

const KEY_PRESS: u8 = 2;
const KEY_RELEASE: u8 = 3;
const BUTTON_PRESS: u8 = 4;
const BUTTON_RELEASE: u8 = 5;
const MOTION: u8 = 6;

pub struct X11 {
    conn: RustConnection,
    root: u32,
    /// The monitor's place on the root window.
    origin: (i32, i32),
    screen: Screen,
    /// RGB byte positions in a 32-bit pixel.
    order: [usize; 3],
    min_keycode: u8,
    per_keycode: usize,
    /// The keyboard map from min_keycode, as last read or changed.
    map: Vec<u32>,
    /// Spare keycodes, and the keysym each was given (0: none yet).
    spare: Vec<(u8, u32)>,
    next_spare: usize,
}

fn err<E: std::fmt::Display>(what: &'static str) -> impl Fn(E) -> String {
    move |e| format!("X11 {what}: {e}")
}

impl X11 {
    pub fn connect() -> Result<X11, String> {
        let (conn, n) = RustConnection::connect(None).map_err(|e| format!("cannot connect to the X server: {e}"))?;
        let setup = conn.setup();
        let s = &setup.roots[n];
        let root = s.root;
        let mut origin = (0, 0);
        let (mut width, mut height) = (s.width_in_pixels as u32, s.height_in_pixels as u32);
        let mut output = None;
        if let Ok(reply) = conn
            .randr_get_monitors(root, true)
            .map_err(|e| e.to_string())
            .and_then(|c| c.reply().map_err(|e| e.to_string()))
        {
            if let Some(m) = reply.monitors.iter().find(|m| m.primary).or(reply.monitors.first()) {
                origin = (m.x as i32, m.y as i32);
                (width, height) = (m.width as u32, m.height as u32);
                output = conn
                    .get_atom_name(m.name)
                    .ok()
                    .and_then(|c| c.reply().ok())
                    .map(|r| String::from_utf8_lossy(&r.name).into_owned());
            }
        }
        conn.xtest_get_version(2, 2)
            .map_err(err("XTEST"))?
            .reply()
            .map_err(|e| format!("the X server lacks the XTEST extension ({e}), which input needs"))?;
        let visual = s
            .allowed_depths
            .iter()
            .flat_map(|d| d.visuals.iter())
            .find(|v| v.visual_id == s.root_visual)
            .ok_or("the X server's root visual is missing")?;
        let order = if visual.red_mask == 0xff0000 { [2, 1, 0] } else { [0, 1, 2] };
        let detail = String::from_utf8_lossy(&setup.vendor).into_owned();
        let (min_keycode, max_keycode) = (setup.min_keycode, setup.max_keycode);
        let mapping = conn
            .get_keyboard_mapping(min_keycode, max_keycode - min_keycode + 1)
            .map_err(err("keyboard mapping"))?
            .reply()
            .map_err(err("keyboard mapping"))?;
        let per_keycode = mapping.keysyms_per_keycode as usize;
        let map = mapping.keysyms;
        let spare = (0..=(max_keycode - min_keycode) as usize)
            .filter(|i| map[i * per_keycode..(i + 1) * per_keycode].iter().all(|k| *k == 0))
            .map(|i| (min_keycode + i as u8, 0))
            .collect();
        Ok(X11 {
            conn,
            root,
            origin,
            screen: Screen { backend: "x11", detail, output, width, height, scale: 1.0 },
            order,
            min_keycode,
            per_keycode,
            map,
            spare,
            next_spare: 0,
        })
    }

    fn fake(&self, kind: u8, detail: u8, x: i16, y: i16) -> Result<(), String> {
        self.conn.xtest_fake_input(kind, detail, x11rb::CURRENT_TIME, self.root, x, y, 0).map_err(err("XTEST"))?;
        Ok(())
    }

    /// A keycode whose unshifted keysym is `k`, mapping a spare one to it
    /// if none is.
    fn keycode(&mut self, k: Keysym) -> Result<u8, String> {
        let per = self.per_keycode;
        if let Some(i) = (0..self.map.len() / per).find(|i| self.map[i * per] == k) {
            return Ok(self.min_keycode + i as u8);
        }
        if self.spare.is_empty() {
            return Err(format!("no spare keycode to type keysym 0x{k:x} with"));
        }
        let slot = self.next_spare % self.spare.len();
        self.next_spare += 1;
        let code = self.spare[slot].0;
        self.spare[slot].1 = k;
        let syms = vec![k; per];
        self.conn.change_keyboard_mapping(1, code, per as u8, &syms).map_err(err("keyboard mapping"))?;
        let i = (code - self.min_keycode) as usize;
        self.map[i * per..(i + 1) * per].copy_from_slice(&syms);
        self.sync()?;
        // Clients apply the new mapping when they read MappingNotify: give
        // them the time before the key arrives.
        std::thread::sleep(std::time::Duration::from_millis(50));
        Ok(code)
    }
}

impl Drop for X11 {
    fn drop(&mut self) {
        let per = self.per_keycode;
        for &(code, k) in &self.spare {
            if k != 0 {
                let _ = self.conn.change_keyboard_mapping(1, code, per as u8, &vec![0; per]);
            }
        }
        let _ = self.conn.flush();
    }
}

impl Backend for X11 {
    fn screen(&self) -> Screen {
        self.screen.clone()
    }

    fn capture(&mut self) -> Result<RgbaImage, String> {
        let (w, h) = (self.screen.width, self.screen.height);
        let reply = self
            .conn
            .get_image(
                ImageFormat::Z_PIXMAP,
                self.root,
                self.origin.0 as i16,
                self.origin.1 as i16,
                w as u16,
                h as u16,
                !0,
            )
            .map_err(err("GetImage"))?
            .reply()
            .map_err(err("GetImage"))?;
        let data = reply.data;
        if data.len() < (w * h * 4) as usize {
            return Err(format!("X11 GetImage: depth {} images are not supported (24 or 32 bits are)", reply.depth));
        }
        let o = self.order;
        let mut img = RgbaImage::from_fn(w, h, |x, y| {
            let p = ((y * w + x) * 4) as usize;
            image::Rgba([data[p + o[0]], data[p + o[1]], data[p + o[2]], 255])
        });
        if let Some((x, y)) = self.cursor()? {
            draw_pointer(&mut img, x, y);
        }
        Ok(img)
    }

    fn move_to(&mut self, x: i32, y: i32) -> Result<(), String> {
        self.fake(MOTION, 0, (x + self.origin.0) as i16, (y + self.origin.1) as i16)?;
        self.sync()
    }

    fn button(&mut self, button: Button, down: bool) -> Result<(), String> {
        self.fake(if down { BUTTON_PRESS } else { BUTTON_RELEASE }, button.x11(), 0, 0)?;
        self.sync()
    }

    fn scroll(&mut self, dx: i32, dy: i32) -> Result<(), String> {
        let steps = [(dy, 5u8, 4u8), (dx, 7, 6)];
        for (n, positive, negative) in steps {
            let b = if n > 0 { positive } else { negative };
            for _ in 0..n.abs() {
                self.fake(BUTTON_PRESS, b, 0, 0)?;
                self.fake(BUTTON_RELEASE, b, 0, 0)?;
            }
        }
        self.sync()
    }

    fn keys(&mut self, keys: &[Keysym], down: bool) -> Result<(), String> {
        for k in keys {
            let code = self.keycode(*k)?;
            self.fake(if down { KEY_PRESS } else { KEY_RELEASE }, code, 0, 0)?;
        }
        self.sync()
    }

    fn type_text(&mut self, text: &str) -> Result<(), String> {
        for c in text.chars() {
            let code = self.keycode(of_char(c))?;
            self.fake(KEY_PRESS, code, 0, 0)?;
            self.fake(KEY_RELEASE, code, 0, 0)?;
            self.sync()?;
        }
        Ok(())
    }

    fn cursor(&mut self) -> Result<Option<(i32, i32)>, String> {
        let r =
            self.conn.query_pointer(self.root).map_err(err("QueryPointer"))?.reply().map_err(err("QueryPointer"))?;
        Ok(Some((r.root_x as i32 - self.origin.0, r.root_y as i32 - self.origin.1)))
    }

    fn sync(&mut self) -> Result<(), String> {
        self.conn.get_input_focus().map_err(err("sync"))?.reply().map_err(err("sync"))?;
        Ok(())
    }
}
