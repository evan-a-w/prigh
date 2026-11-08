//! An in-memory desktop for tests (`--backend fake`): a framebuffer with a
//! pointer drawn on it, and a log of the input it was given, in real
//! pixels. `fake_user_move` moves its pointer as a user would, to test the
//! kill switch.

use crate::action::Button;
use crate::backend::{draw_pointer, Backend, Screen};
use crate::keysym::{xkb_name, Keysym};
use image::{Rgba, RgbaImage};

pub struct Fake {
    width: u32,
    height: u32,
    pointer: (i32, i32),
    /// Where buttons went down, drawn as dots.
    marks: Vec<(i32, i32)>,
    pub log: Vec<String>,
    held: Vec<Keysym>,
    /// A key the fake fails to press, to test what a failure leaves held.
    pub refuse: Option<Keysym>,
}

impl Fake {
    pub fn new(width: u32, height: u32) -> Fake {
        Fake {
            width,
            height,
            pointer: (width as i32 / 2, height as i32 / 2),
            marks: Vec::new(),
            log: Vec::new(),
            held: Vec::new(),
            refuse: None,
        }
    }

    pub fn user_move(&mut self, x: i32, y: i32) {
        self.pointer = (x, y);
        self.log.push(format!("user moved to {x},{y}"));
    }
}

impl Backend for Fake {
    fn screen(&self) -> Screen {
        Screen {
            backend: "fake",
            detail: "in-memory framebuffer".into(),
            output: Some("FAKE-1".into()),
            width: self.width,
            height: self.height,
            scale: 1.0,
        }
    }

    fn capture(&mut self) -> Result<RgbaImage, String> {
        let mut img = RgbaImage::from_fn(self.width, self.height, |x, y| {
            // a "window" in the middle of a grey desktop
            if x > self.width / 4 && x < self.width * 3 / 4 && y > self.height / 4 && y < self.height * 3 / 4 {
                Rgba([250, 250, 250, 255])
            } else {
                Rgba([90, 110, 130, 255])
            }
        });
        for &(mx, my) in &self.marks {
            for dy in -3..=3 {
                for dx in -3..=3 {
                    let (px, py) = (mx + dx, my + dy);
                    if px >= 0 && py >= 0 && (px as u32) < self.width && (py as u32) < self.height {
                        img.put_pixel(px as u32, py as u32, Rgba([220, 40, 40, 255]));
                    }
                }
            }
        }
        draw_pointer(&mut img, self.pointer.0, self.pointer.1);
        Ok(img)
    }

    fn move_to(&mut self, x: i32, y: i32) -> Result<(), String> {
        self.pointer = (x, y);
        self.log.push(format!("move {x},{y}"));
        Ok(())
    }

    fn button(&mut self, button: Button, down: bool) -> Result<(), String> {
        if down {
            self.marks.push(self.pointer);
        }
        self.log.push(format!("button {} {}", button.name(), if down { "down" } else { "up" }));
        Ok(())
    }

    fn scroll(&mut self, dx: i32, dy: i32) -> Result<(), String> {
        self.log.push(format!("scroll {dx},{dy}"));
        Ok(())
    }

    fn keys(&mut self, keys: &[Keysym], down: bool) -> Result<(), String> {
        let names: Vec<String> = keys.iter().map(|k| xkb_name(*k)).collect();
        self.log.push(format!("key {} {}", names.join("+"), if down { "down" } else { "up" }));
        for &k in keys {
            if !down {
                if let Some(i) = self.held.iter().rposition(|h| *h == k) {
                    self.held.remove(i);
                }
            } else if self.refuse == Some(k) {
                return Err(format!("the fake refuses {}", xkb_name(k)));
            } else {
                self.held.push(k);
            }
        }
        Ok(())
    }

    fn release_all(&mut self) -> Result<(), String> {
        if !self.held.is_empty() {
            let names: Vec<String> = self.held.iter().rev().map(|k| xkb_name(*k)).collect();
            self.log.push(format!("released {}", names.join("+")));
            self.held.clear();
        }
        Ok(())
    }

    fn type_text(&mut self, text: &str) -> Result<(), String> {
        self.log.push(format!("text {text}"));
        Ok(())
    }

    fn cursor(&mut self) -> Result<Option<(i32, i32)>, String> {
        Ok(Some(self.pointer))
    }

    fn as_fake(&mut self) -> Option<&mut Fake> {
        Some(self)
    }
}
