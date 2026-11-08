//! What each desktop gives the helper: the controlled output's pixels and
//! input on it, all in that output's real (physical) pixels. The driver
//! ([crate::driver]) does the rest (scaling, the kill switch, settling,
//! encoding), so a new platform (xdg-desktop-portal, macOS, Windows) only
//! implements this trait and joins [crate::detect].

use crate::action::Button;
use crate::keysym::Keysym;
use image::RgbaImage;

/// What the backend controls, as `info` reports it.
#[derive(Debug, Clone, PartialEq)]
pub struct Screen {
    /// "wlroots", "x11", "fake", ...
    pub backend: &'static str,
    /// The compositor or server ("Hyprland", "sway", "X.Org").
    pub detail: String,
    /// The output's name ("DP-1"), if it has one.
    pub output: Option<String>,
    pub width: u32,
    pub height: u32,
    /// The compositor's HiDPI scale factor for the output (1.0 without one).
    pub scale: f64,
}

pub trait Backend {
    fn screen(&self) -> Screen;

    /// The whole output, at its real resolution, with the pointer drawn.
    fn capture(&mut self) -> Result<RgbaImage, String>;

    fn move_to(&mut self, x: i32, y: i32) -> Result<(), String>;
    fn button(&mut self, button: Button, down: bool) -> Result<(), String>;
    /// Wheel notches: +dy down, +dx right.
    fn scroll(&mut self, dx: i32, dy: i32) -> Result<(), String>;
    /// Presses (in order) or releases (pass them reversed) keys.
    fn keys(&mut self, keys: &[Keysym], down: bool) -> Result<(), String>;
    /// Types text, whatever its characters, independent of the user's layout.
    fn type_text(&mut self, text: &str) -> Result<(), String>;
    /// Releases every key still held: after a failure, and at exit.
    fn release_all(&mut self) -> Result<(), String> {
        Ok(())
    }

    /// Where the pointer is now, if this desktop can tell: the kill switch
    /// compares it with where the helper left it.
    fn cursor(&mut self) -> Result<Option<(i32, i32)>, String>;

    /// Makes sure everything sent so far has been processed.
    fn sync(&mut self) -> Result<(), String> {
        Ok(())
    }

    /// For the fake backend's test ops ([crate::fake]).
    fn as_fake(&mut self) -> Option<&mut crate::fake::Fake> {
        None
    }
}

/// Draws an arrow pointer with its tip at (x, y): white with a black
/// outline, so it shows on any background. For backends whose capture
/// leaves the pointer out (X11's GetImage).
pub fn draw_pointer(img: &mut RgbaImage, x: i32, y: i32) {
    const SHAPE: [&str; 16] = [
        "X",
        "XX",
        "X.X",
        "X..X",
        "X...X",
        "X....X",
        "X.....X",
        "X......X",
        "X.......X",
        "X........X",
        "X.....XXXXX",
        "X..X..X",
        "X.X X..X",
        "XX  X..X",
        "X    X..X",
        "     XXX",
    ];
    for (dy, row) in SHAPE.iter().enumerate() {
        for (dx, c) in row.chars().enumerate() {
            let colour = match c {
                'X' => [0, 0, 0, 255],
                '.' => [255, 255, 255, 255],
                _ => continue,
            };
            let (px, py) = (x + dx as i32, y + dy as i32);
            if px >= 0 && py >= 0 && (px as u32) < img.width() && (py as u32) < img.height() {
                img.put_pixel(px as u32, py as u32, image::Rgba(colour));
            }
        }
    }
}
