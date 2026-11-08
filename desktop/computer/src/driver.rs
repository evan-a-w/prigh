//! The JSON-lines protocol (one request per line, answered in order) over a
//! [Backend]: actions in the scaled space are mapped to real pixels,
//! performed, left to settle and answered with a scaled screenshot.
//!
//! The kill switch: before each action, and between the steps of long ones
//! (typing, drags, held keys), the pointer must still be where the helper
//! left it; if the user moved it, the action stops with `interrupted`.

use crate::action::{Action, Button, Point};
use crate::backend::Backend;
use crate::keysym::Keysym;
use crate::scale::{Limits, Mapping};
use base64::Engine;
use image::RgbaImage;
use serde_json::{json, Map, Value};
use std::time::{Duration, Instant};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Format {
    Png,
    Jpeg,
}

#[derive(Debug, Clone)]
pub struct Config {
    pub limits: Limits,
    pub format: Format,
    pub settle: Duration,
    /// How long after the helper's last action a moved pointer still counts
    /// as the user taking over (rather than the user working between the
    /// agent's turns).
    pub watch: Duration,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            limits: Limits::default(),
            format: Format::Png,
            settle: Duration::from_millis(300),
            watch: Duration::from_secs(30),
        }
    }
}

pub const INTERRUPTED: &str = "you moved the mouse: stopped (computer use paused until the next prompt)";

/// Set when the helper is told to stop (a signal): the action in hand stops
/// at its next step, letting go of what it holds.
pub static STOPPING: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

enum Failure {
    Error(String),
    Interrupted,
}

impl From<String> for Failure {
    fn from(e: String) -> Self {
        Failure::Error(e)
    }
}

type R<T> = Result<T, Failure>;

pub struct Driver {
    backend: Box<dyn Backend>,
    config: Config,
    /// Where the helper last put the pointer (real pixels), and when.
    left_at: Option<((i32, i32), Instant)>,
}

/// The base64 image with its size, as `screenshot` in a reply.
pub struct Encoded {
    pub mime_type: &'static str,
    pub data: String,
    pub width: u32,
    pub height: u32,
}

pub fn encode(img: &RgbaImage, format: Format) -> Result<Encoded, String> {
    let rgb = image::DynamicImage::ImageRgba8(img.clone()).into_rgb8();
    let mut bytes = Vec::new();
    let mime_type = match format {
        Format::Png => {
            rgb.write_to(&mut std::io::Cursor::new(&mut bytes), image::ImageFormat::Png)
                .map_err(|e| format!("encoding the screenshot: {e}"))?;
            "image/png"
        }
        Format::Jpeg => {
            image::codecs::jpeg::JpegEncoder::new_with_quality(&mut bytes, 80)
                .encode_image(&rgb)
                .map_err(|e| format!("encoding the screenshot: {e}"))?;
            "image/jpeg"
        }
    };
    Ok(Encoded {
        mime_type,
        data: base64::engine::general_purpose::STANDARD.encode(bytes),
        width: img.width(),
        height: img.height(),
    })
}

/// `img` shrunk to `w`×`h` (as it is when that is its size).
fn resized(img: RgbaImage, w: u32, h: u32) -> RgbaImage {
    if img.width() == w && img.height() == h {
        img
    } else {
        image::imageops::resize(&img, w, h, image::imageops::FilterType::Triangle)
    }
}

impl Driver {
    pub fn new(backend: Box<dyn Backend>, config: Config) -> Driver {
        Driver { backend, config, left_at: None }
    }

    fn mapping(&self) -> Mapping {
        let s = self.backend.screen();
        Mapping::new(&self.config.limits, s.width, s.height)
    }

    pub fn info(&self) -> Value {
        let s = self.backend.screen();
        let m = self.mapping();
        json!({
            "backend": s.backend,
            "detail": s.detail,
            "platform": std::env::consts::OS,
            "output": s.output,
            "width": s.width,
            "height": s.height,
            "scale": s.scale,
            "scaled_width": m.scaled_w,
            "scaled_height": m.scaled_h,
        })
    }

    /// Fails with [Failure::Interrupted] when the pointer is not where the
    /// helper left it (recently), and when the helper is [STOPPING].
    fn check(&mut self) -> R<()> {
        if STOPPING.load(std::sync::atomic::Ordering::Relaxed) {
            return Err(Failure::Error("the helper was stopped".into()));
        }
        let Some((left, at)) = self.left_at else { return Ok(()) };
        if at.elapsed() > self.config.watch {
            self.left_at = None;
            return Ok(());
        }
        if let Some((x, y)) = self.backend.cursor()? {
            if (x - left.0).abs() > 2 || (y - left.1).abs() > 2 {
                self.left_at = None;
                return Err(Failure::Interrupted);
            }
        }
        Ok(())
    }

    fn move_real(&mut self, x: i32, y: i32) -> R<()> {
        self.backend.move_to(x, y)?;
        self.left_at = Some(((x, y), Instant::now()));
        Ok(())
    }

    /// Notes that the helper acted without moving the pointer (the switch
    /// stays armed).
    fn touch(&mut self) {
        if let Some((p, _)) = self.left_at {
            self.left_at = Some((p, Instant::now()));
        } else if let Ok(Some(p)) = self.backend.cursor() {
            self.left_at = Some((p, Instant::now()));
        }
    }

    fn to_real(&self, p: Point) -> R<(i32, i32)> {
        Ok(self.mapping().to_real(p.0, p.1)?)
    }

    fn at(&mut self, p: Option<Point>) -> R<Option<(i32, i32)>> {
        match p {
            None => Ok(None),
            Some(p) => {
                let r = self.to_real(p)?;
                self.move_real(r.0, r.1)?;
                Ok(Some(r))
            }
        }
    }

    fn with_keys<T>(&mut self, keys: Option<&[Keysym]>, f: impl FnOnce(&mut Self) -> R<T>) -> R<T> {
        if let Some(k) = keys {
            self.backend.keys(k, true)?;
        }
        let result = f(self);
        if let Some(k) = keys {
            let mut rev = k.to_vec();
            rev.reverse();
            let released = self.backend.keys(&rev, false);
            if result.is_ok() {
                released?;
            }
        }
        result
    }

    fn sleep_checking(&mut self, d: Duration) -> R<()> {
        let end = Instant::now() + d;
        while Instant::now() < end {
            std::thread::sleep((end - Instant::now()).min(Duration::from_millis(100)));
            self.check()?;
        }
        Ok(())
    }

    fn screenshot(&mut self) -> R<Encoded> {
        let m = self.mapping();
        let img = self.backend.capture()?;
        let img = resized(img, m.scaled_w, m.scaled_h);
        Ok(encode(&img, self.config.format)?)
    }

    fn zoom(&mut self, (x0, y0, x1, y1): (i64, i64, i64, i64)) -> R<Encoded> {
        let m = self.mapping();
        let clamp = |v: i64, max: u32| v.clamp(0, max as i64 - 1);
        let (rx0, ry0) = m.to_real(clamp(x0, m.scaled_w), clamp(y0, m.scaled_h))?;
        let (rx1, ry1) = m.to_real(clamp(x1 - 1, m.scaled_w), clamp(y1 - 1, m.scaled_h))?;
        // to the far edge of the last scaled pixel
        let rx1 = ((clamp(x1, m.scaled_w + 1) * m.real_w as i64) / m.scaled_w as i64).max(rx1 as i64 + 1) as u32;
        let ry1 = ((clamp(y1, m.scaled_h + 1) * m.real_h as i64) / m.scaled_h as i64).max(ry1 as i64 + 1) as u32;
        let img = self.backend.capture()?;
        let (w, h) = (rx1.min(img.width()) - rx0 as u32, ry1.min(img.height()) - ry0 as u32);
        let crop = image::imageops::crop_imm(&img, rx0 as u32, ry0 as u32, w, h).to_image();
        let (fw, fh) = self.config.limits.fit(w, h);
        Ok(encode(&resized(crop, fw, fh), self.config.format)?)
    }

    fn click(&mut self, button: Button, count: u32) -> R<()> {
        for i in 0..count {
            if i > 0 {
                std::thread::sleep(Duration::from_millis(40));
            }
            self.backend.button(button, true)?;
            self.backend.button(button, false)?;
        }
        Ok(())
    }

    fn type_text(&mut self, text: &str) -> R<()> {
        let chars: Vec<char> = text.chars().collect();
        for chunk in chars.chunks(8) {
            self.check()?;
            self.backend.type_text(&chunk.iter().collect::<String>())?;
        }
        Ok(())
    }

    /// Performs an action: what it did (in words), and the real point it
    /// aimed at.
    fn perform(&mut self, action: &Action) -> R<(String, Option<(i32, i32)>)> {
        let show = |p: Point| format!("({}, {})", p.0, p.1);
        let fmt_at = |p: Option<Point>| p.map(|p| format!(" at {}", show(p))).unwrap_or_default();
        self.check()?;
        Ok(match action {
            Action::Screenshot => ("screenshot".into(), None),
            Action::CursorPosition => ("cursor position".into(), None),
            Action::Wait { seconds } => {
                std::thread::sleep(Duration::from_secs_f64(*seconds));
                (format!("waited {seconds}s"), None)
            }
            Action::Zoom { region } => {
                (format!("zoomed into [{}, {}, {}, {}]", region.0, region.1, region.2, region.3), None)
            }
            Action::Move { at } => {
                let r = self.at(Some(*at))?;
                (format!("moved to {}", show(*at)), r)
            }
            Action::Click { at, button, count, keys } => {
                let r = self.at(*at)?;
                let keys = keys.as_ref().map(|k| k.keys.clone());
                self.with_keys(keys.as_deref(), |d| d.click(*button, *count))?;
                self.touch();
                let what = match count {
                    1 => "clicked",
                    2 => "double-clicked",
                    _ => "triple-clicked",
                };
                (format!("{what} {}{}", button.name(), fmt_at(*at)), r)
            }
            Action::MouseDown { at, button } | Action::MouseUp { at, button } => {
                let down = matches!(action, Action::MouseDown { .. });
                let r = self.at(*at)?;
                self.backend.button(*button, down)?;
                self.touch();
                (format!("{} {}{}", if down { "pressed" } else { "released" }, button.name(), fmt_at(*at)), r)
            }
            Action::Drag { path, button, keys } => {
                let keys = keys.as_ref().map(|k| k.keys.clone());
                let reals = path.iter().map(|p| self.to_real(*p)).collect::<R<Vec<_>>>()?;
                self.move_real(reals[0].0, reals[0].1)?;
                let last = *reals.last().unwrap();
                self.with_keys(keys.as_deref(), |d| {
                    d.backend.button(*button, true)?;
                    let moved = (|| -> R<()> {
                        for &(x, y) in &reals[1..] {
                            std::thread::sleep(Duration::from_millis(30));
                            d.check()?;
                            d.move_real(x, y)?;
                        }
                        Ok(())
                    })();
                    let released = d.backend.button(*button, false);
                    moved?;
                    Ok(released?)
                })?;
                (
                    format!("dragged {} from {} to {}", button.name(), show(path[0]), show(*path.last().unwrap())),
                    Some(last),
                )
            }
            Action::Scroll { at, dx, dy, keys } => {
                let r = self.at(*at)?;
                let keys = keys.as_ref().map(|k| k.keys.clone());
                self.with_keys(keys.as_deref(), |d| Ok(d.backend.scroll(*dx, *dy)?))?;
                self.touch();
                (format!("scrolled {dx},{dy}{}", fmt_at(*at)), r)
            }
            Action::Type { text } => {
                self.type_text(text)?;
                self.touch();
                (format!("typed {} characters", text.chars().count()), None)
            }
            Action::Key { combos } => {
                for (i, c) in combos.iter().enumerate() {
                    if i > 0 {
                        self.check()?;
                    }
                    self.backend.keys(&c.keys, true)?;
                    let mut rev = c.keys.clone();
                    rev.reverse();
                    self.backend.keys(&rev, false)?;
                }
                self.touch();
                (format!("pressed {}", combos.iter().map(|c| c.text.as_str()).collect::<Vec<_>>().join(" ")), None)
            }
            Action::HoldKey { combo, duration } => {
                self.backend.keys(&combo.keys, true)?;
                self.touch();
                let held = self.sleep_checking(Duration::from_secs_f64(*duration));
                let mut rev = combo.keys.clone();
                rev.reverse();
                let released = self.backend.keys(&rev, false);
                held?;
                released?;
                (format!("held {} for {duration}s", combo.text), None)
            }
        })
    }

    pub fn act(&mut self, action: &Action) -> Map<String, Value> {
        let mut out = Map::new();
        let result = (|| -> R<()> {
            let (text, real) = self.perform(action)?;
            self.backend.sync()?;
            let mut shot = None;
            match action {
                Action::Wait { .. } => {}
                Action::Zoom { region } => shot = Some(self.zoom(*region)?),
                Action::Screenshot | Action::CursorPosition => shot = Some(self.screenshot()?),
                _ => {
                    std::thread::sleep(self.config.settle);
                    shot = Some(self.screenshot()?);
                }
            }
            out.insert("ok".into(), json!(true));
            out.insert("text".into(), json!(text));
            let m = self.mapping();
            let cursor = match self.backend.cursor()? {
                Some(p) => Some(p),
                None => self.left_at.map(|(p, _)| p),
            };
            if let Some((x, y)) = cursor {
                let (sx, sy) = m.to_scaled(x, y);
                out.insert("cursor".into(), json!({"x": sx, "y": sy}));
            }
            if let Some((x, y)) = real {
                out.insert("real".into(), json!({"x": x, "y": y}));
            }
            if let Some(s) = shot {
                out.insert(
                    "screenshot".into(),
                    json!({"mime_type": s.mime_type, "data": s.data, "width": s.width, "height": s.height}),
                );
            }
            Ok(())
        })();
        match result {
            Ok(()) => out,
            Err(f) => {
                let _ = self.backend.release_all().and_then(|()| self.backend.sync());
                let (error, interrupted) = match f {
                    Failure::Error(e) => (e, false),
                    Failure::Interrupted => (INTERRUPTED.to_string(), true),
                };
                let mut m = Map::new();
                m.insert("ok".into(), json!(false));
                m.insert("error".into(), json!(error));
                m.insert("interrupted".into(), json!(interrupted));
                m
            }
        }
    }

    /// Answers one request line.
    pub fn handle_line(&mut self, line: &str) -> Value {
        let req: Value = match serde_json::from_str(line) {
            Ok(v) => v,
            Err(e) => {
                return json!({"id": null, "ok": false, "error": format!("not JSON ({e}): send one object per line, like {{\"id\":1,\"op\":\"info\"}}"), "interrupted": false})
            }
        };
        let id = req.get("id").cloned().unwrap_or(Value::Null);
        let fail = |e: String| json!({"id": id, "ok": false, "error": e, "interrupted": false});
        let mut body = match req.get("op").and_then(Value::as_str) {
            Some("info") => {
                let mut m = Map::new();
                m.insert("ok".into(), json!(true));
                m.insert("info".into(), self.info());
                m
            }
            Some("act") => match req.get("action").map(Action::parse) {
                None => return fail("act needs \"action\": {\"action\":\"screenshot\"}, ...".into()),
                Some(Err(e)) => return fail(e),
                Some(Ok(action)) => self.act(&action),
            },
            Some(op @ ("fake_log" | "fake_user_move")) => {
                let Some(fake) = self.backend.as_fake() else {
                    return fail(format!("{op} is only for --backend fake"));
                };
                let mut m = Map::new();
                m.insert("ok".into(), json!(true));
                if op == "fake_log" {
                    m.insert("log".into(), json!(fake.log));
                } else {
                    let coord = |k: &str| req.get(k).and_then(Value::as_i64).unwrap_or(0) as i32;
                    fake.user_move(coord("x"), coord("y"));
                }
                m
            }
            Some(other) => return fail(format!("unknown op {other:?}: use info or act")),
            None => return fail("\"op\" is missing: info or act".into()),
        };
        body.insert("id".into(), id);
        Value::Object(body)
    }
}

impl Drop for Driver {
    fn drop(&mut self) {
        let _ = self.backend.release_all().and_then(|()| self.backend.sync());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fake::Fake;

    fn driver() -> Driver {
        let config = Config {
            limits: Limits { max_long_side: 320, max_pixels: 1_000_000 },
            format: Format::Png,
            settle: Duration::ZERO,
            watch: Duration::from_secs(30),
        };
        Driver::new(Box::new(Fake::new(640, 400)), config)
    }

    /// The reply without the image data, which is checked apart.
    fn ask(d: &mut Driver, line: &str) -> String {
        let mut v = d.handle_line(line);
        if let Some(s) = v.get_mut("screenshot") {
            let data = s["data"].as_str().unwrap().to_string();
            let bytes = base64::engine::general_purpose::STANDARD.decode(data).unwrap();
            let img = image::load_from_memory(&bytes).unwrap();
            assert_eq!(
                (img.width() as u64, img.height() as u64),
                (s["width"].as_u64().unwrap(), s["height"].as_u64().unwrap())
            );
            s["data"] = json!("…");
        }
        v.to_string()
    }

    #[test]
    fn protocol() {
        let mut d = driver();
        let lines = [
            r#"{"id":1,"op":"info"}"#,
            r#"{"id":2,"op":"act","action":{"action":"screenshot"}}"#,
            r#"{"id":3,"op":"act","action":{"action":"click","x":160,"y":100}}"#,
            r#"{"id":4,"op":"act","action":{"action":"click","x":10,"y":20,"button":"right","count":2,"keys":"ctrl"}}"#,
            r#"{"id":5,"op":"act","action":{"action":"type","text":"hello wörld"}}"#,
            r#"{"id":6,"op":"act","action":{"action":"key","combo":"ctrl+shift+t Return"}}"#,
            r#"{"id":7,"op":"act","action":{"action":"scroll","x":5,"y":5,"dy":3}}"#,
            r#"{"id":8,"op":"act","action":{"action":"drag","path":[[0,0],[50,50],[100,60]]}}"#,
            r#"{"id":9,"op":"act","action":{"action":"wait","seconds":0}}"#,
            r#"{"id":10,"op":"act","action":{"action":"zoom","region":[0,0,80,50]}}"#,
            r#"{"id":11,"op":"act","action":{"action":"cursor_position"}}"#,
            r#"{"id":12,"op":"act","action":{"action":"click","x":320,"y":0}}"#,
            r#"{"id":13,"op":"act","action":{"action":"fly"}}"#,
            r#"{"id":14,"op":"nope"}"#,
            r#"not json"#,
            r#"{"id":15,"op":"fake_log"}"#,
        ];
        // the expected output is Linux's
        let platform = format!("\"platform\":\"{}\"", std::env::consts::OS);
        let out: Vec<String> =
            lines.iter().map(|l| ask(&mut d, l).replace(&platform, "\"platform\":\"linux\"")).collect();
        let expected = [
            r#"{"id":1,"info":{"backend":"fake","detail":"in-memory framebuffer","height":400,"output":"FAKE-1","platform":"linux","scale":1.0,"scaled_height":200,"scaled_width":320,"width":640},"ok":true}"#,
            r#"{"cursor":{"x":160,"y":100},"id":2,"ok":true,"screenshot":{"data":"…","height":200,"mime_type":"image/png","width":320},"text":"screenshot"}"#,
            r#"{"cursor":{"x":160,"y":100},"id":3,"ok":true,"real":{"x":320,"y":200},"screenshot":{"data":"…","height":200,"mime_type":"image/png","width":320},"text":"clicked left at (160, 100)"}"#,
            r#"{"cursor":{"x":10,"y":20},"id":4,"ok":true,"real":{"x":20,"y":40},"screenshot":{"data":"…","height":200,"mime_type":"image/png","width":320},"text":"double-clicked right at (10, 20)"}"#,
            r#"{"cursor":{"x":10,"y":20},"id":5,"ok":true,"screenshot":{"data":"…","height":200,"mime_type":"image/png","width":320},"text":"typed 11 characters"}"#,
            r#"{"cursor":{"x":10,"y":20},"id":6,"ok":true,"screenshot":{"data":"…","height":200,"mime_type":"image/png","width":320},"text":"pressed ctrl+shift+t Return"}"#,
            r#"{"cursor":{"x":5,"y":5},"id":7,"ok":true,"real":{"x":10,"y":10},"screenshot":{"data":"…","height":200,"mime_type":"image/png","width":320},"text":"scrolled 0,3 at (5, 5)"}"#,
            r#"{"cursor":{"x":100,"y":60},"id":8,"ok":true,"real":{"x":200,"y":120},"screenshot":{"data":"…","height":200,"mime_type":"image/png","width":320},"text":"dragged left from (0, 0) to (100, 60)"}"#,
            r#"{"cursor":{"x":100,"y":60},"id":9,"ok":true,"text":"waited 0s"}"#,
            r#"{"cursor":{"x":100,"y":60},"id":10,"ok":true,"screenshot":{"data":"…","height":100,"mime_type":"image/png","width":160},"text":"zoomed into [0, 0, 80, 50]"}"#,
            r#"{"cursor":{"x":100,"y":60},"id":11,"ok":true,"screenshot":{"data":"…","height":200,"mime_type":"image/png","width":320},"text":"cursor position"}"#,
            r#"{"error":"(320, 0) is off the screen: x must be within 0..319 and y within 0..199 (the screenshot's size)","id":12,"interrupted":false,"ok":false}"#,
            r#"{"error":"unknown action \"fly\": use one of screenshot, click, move, drag, mouse_down, mouse_up, scroll, type, key, hold_key, wait, zoom, cursor_position","id":13,"interrupted":false,"ok":false}"#,
            r#"{"error":"unknown op \"nope\": use info or act","id":14,"interrupted":false,"ok":false}"#,
            r#"{"error":"not JSON (expected ident at line 1 column 2): send one object per line, like {\"id\":1,\"op\":\"info\"}","id":null,"interrupted":false,"ok":false}"#,
            r#"{"id":15,"log":["move 320,200","button left down","button left up","move 20,40","key Control_L down","button right down","button right up","button right down","button right up","key Control_L up","text hello wö","text rld","key Control_L+Shift_L+t down","key t+Shift_L+Control_L up","key Return down","key Return up","move 10,10","scroll 0,3","move 0,0","button left down","move 100,100","move 200,120","button left up"],"ok":true}"#,
        ];
        for (i, (o, e)) in out.iter().zip(expected.iter()).enumerate() {
            assert_eq!(o, e, "line {}", i + 1);
        }
    }

    #[test]
    fn kill_switch() {
        let mut d = driver();
        ask(&mut d, r#"{"id":1,"op":"act","action":{"action":"move","x":100,"y":100}}"#);
        // the pointer is where the helper left it: no interruption
        assert!(ask(&mut d, r#"{"id":2,"op":"act","action":{"action":"type","text":"a"}}"#).contains(r#""ok":true"#));
        assert_eq!(ask(&mut d, r#"{"id":3,"op":"fake_user_move","x":5,"y":5}"#), r#"{"id":3,"ok":true}"#);
        assert_eq!(
            ask(&mut d, r#"{"id":4,"op":"act","action":{"action":"type","text":"hello"}}"#),
            r#"{"error":"you moved the mouse: stopped (computer use paused until the next prompt)","id":4,"interrupted":true,"ok":false}"#
        );
        // nothing was typed after the user moved; the next action goes on
        assert!(ask(&mut d, r#"{"id":5,"op":"act","action":{"action":"screenshot"}}"#).contains(r#""ok":true"#));
        let log = ask(&mut d, r#"{"id":6,"op":"fake_log"}"#);
        assert!(log.ends_with(r#""text a","user moved to 5,5"],"ok":true}"#), "{log}");
    }

    #[test]
    fn kill_switch_only_while_recent() {
        let mut d = driver();
        d.config.watch = Duration::ZERO;
        ask(&mut d, r#"{"id":1,"op":"act","action":{"action":"move","x":100,"y":100}}"#);
        std::thread::sleep(Duration::from_millis(5));
        ask(&mut d, r#"{"id":2,"op":"fake_user_move","x":5,"y":5}"#);
        assert!(ask(&mut d, r#"{"id":3,"op":"act","action":{"action":"type","text":"x"}}"#).contains(r#""ok":true"#));
    }

    #[test]
    fn a_failure_releases_every_key() {
        let mut d = driver();
        d.backend.as_fake().unwrap().refuse = crate::keysym::of_name("t");
        assert_eq!(
            ask(&mut d, r#"{"id":1,"op":"act","action":{"action":"key","combo":"ctrl+shift+t"}}"#),
            r#"{"error":"the fake refuses t","id":1,"interrupted":false,"ok":false}"#
        );
        assert_eq!(
            ask(&mut d, r#"{"id":2,"op":"fake_log"}"#),
            r#"{"id":2,"log":["key Control_L+Shift_L+t down","released Shift_L+Control_L"],"ok":true}"#
        );
        // keys held during a click that fails
        d.backend.as_fake().unwrap().refuse = crate::keysym::of_name("shift");
        ask(&mut d, r#"{"id":3,"op":"act","action":{"action":"click","x":1,"y":1,"keys":"super+shift"}}"#);
        let fake = d.backend.as_fake().unwrap();
        assert_eq!(fake.log[2..], ["move 2,2", "key Super_L+Shift_L down", "released Super_L"]);
    }

    #[test]
    fn jpeg() {
        let mut d = driver();
        d.config.format = Format::Jpeg;
        assert!(ask(&mut d, r#"{"id":1,"op":"act","action":{"action":"screenshot"}}"#).contains("image/jpeg"));
    }
}
