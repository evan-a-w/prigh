//! Wayland desktops without the wlroots protocols (GNOME's Mutter, KDE's
//! KWin, ...) through xdg-desktop-portal: input with the RemoteDesktop
//! portal, the screen through a ScreenCast stream of the same session
//! ([crate::screencast], PipeWire), so the user approves once, in one
//! dialog. `persist_mode` 2 makes that approval last: the portal answers
//! with a restore token, kept in ~/.prigh ([TokenStore]), and the next
//! session that presents it starts without asking.
//!
//! The portal cannot tell where the pointer is; the stream's cursor
//! metadata can (when the compositor offers it), which keeps the kill
//! switch working.

use crate::action::Button;
use crate::backend::{draw_pointer, Backend, Screen};
use crate::keysym::{of_char, Keysym};
use image::RgbaImage;
use std::collections::HashMap;
use std::os::fd::OwnedFd;
use std::path::PathBuf;
use std::time::{Duration, Instant};
use zbus::blocking::{Connection, MessageIterator};
use zbus::zvariant::{OwnedObjectPath, OwnedValue, Value};

pub const DESTINATION: &str = "org.freedesktop.portal.Desktop";
pub const PATH: &str = "/org/freedesktop/portal/desktop";
pub const REMOTE_DESKTOP: &str = "org.freedesktop.portal.RemoteDesktop";
pub const SCREEN_CAST: &str = "org.freedesktop.portal.ScreenCast";
const REQUEST: &str = "org.freedesktop.portal.Request";

/// SelectDevices' device types.
pub const KEYBOARD: u32 = 1;
pub const POINTER: u32 = 2;
/// SelectSources' source type: a monitor.
pub const MONITOR: u32 = 1;

/// ScreenCast's cursor modes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CursorMode {
    Hidden = 1,
    Embedded = 2,
    /// Left out of the frames, its position (and image) sent alongside.
    Metadata = 4,
}

impl CursorMode {
    /// The best of the modes the portal offers (its AvailableCursorModes):
    /// metadata, for the kill switch, then drawn into the frames.
    pub fn pick(available: u32) -> CursorMode {
        if available & CursorMode::Metadata as u32 != 0 {
            CursorMode::Metadata
        } else if available & CursorMode::Embedded as u32 != 0 {
            CursorMode::Embedded
        } else {
            CursorMode::Hidden
        }
    }
}

fn err(what: &'static str) -> impl Fn(zbus::Error) -> String {
    move |e| format!("xdg-desktop-portal {what}: {e}")
}

/// The session bus at `address`, or the user's.
pub fn bus(address: Option<&str>) -> Result<Connection, String> {
    match address {
        Some(a) => zbus::blocking::connection::Builder::address(a)
            .and_then(|b| b.build())
            .map_err(|e| format!("cannot connect to the D-Bus session bus at {a}: {e}")),
        None => Connection::session().map_err(|e| format!("cannot connect to the D-Bus session bus: {e}")),
    }
}

fn property(conn: &Connection, iface: &str, name: &str) -> Result<OwnedValue, zbus::Error> {
    let reply =
        conn.call_method(Some(DESTINATION), PATH, Some("org.freedesktop.DBus.Properties"), "Get", &(iface, name))?;
    let v: OwnedValue = reply.body().deserialize()?;
    // Get answers a variant holding the value
    match &*v {
        Value::Value(inner) => Ok(OwnedValue::try_from(&**inner)?),
        _ => Ok(v),
    }
}

fn u32_property(conn: &Connection, iface: &str, name: &str) -> Result<u32, zbus::Error> {
    Ok(u32::try_from(property(conn, iface, name)?)?)
}

/// The RemoteDesktop portal's version, if the session bus has one that
/// can move a pointer and type: what detection asks.
pub fn available(conn: &Connection) -> Result<u32, String> {
    let version = u32_property(conn, REMOTE_DESKTOP, "version")
        .map_err(|e| format!("the RemoteDesktop portal is not available ({e})"))?;
    let types = u32_property(conn, REMOTE_DESKTOP, "AvailableDeviceTypes")
        .map_err(|e| format!("the RemoteDesktop portal is not available ({e})"))?;
    if types & (KEYBOARD | POINTER) != KEYBOARD | POINTER {
        return Err(format!(
            "the RemoteDesktop portal offers device types {types} (it needs a keyboard and a pointer: 3)"
        ));
    }
    let _ = u32_property(conn, SCREEN_CAST, "version")
        .map_err(|e| format!("the ScreenCast portal is not available ({e})"))?;
    Ok(version)
}

/// Where the restore token lives between sessions: one line, readable only
/// by the user (it lets whoever presents it drive the desktop without
/// asking).
pub struct TokenStore {
    pub path: PathBuf,
}

impl TokenStore {
    pub fn default_path() -> Option<PathBuf> {
        let home = std::env::var_os("HOME").filter(|h| !h.is_empty())?;
        Some(PathBuf::from(home).join(".prigh").join("computer-portal-token"))
    }

    pub fn load(&self) -> Option<String> {
        let s = std::fs::read_to_string(&self.path).ok()?;
        let s = s.trim();
        (!s.is_empty()).then(|| s.to_string())
    }

    pub fn save(&self, token: &str) -> Result<(), String> {
        use std::io::Write;
        use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};
        let fail = |e: std::io::Error| format!("saving the portal's restore token in {}: {e}", self.path.display());
        if let Some(dir) = self.path.parent() {
            std::fs::create_dir_all(dir).map_err(fail)?;
        }
        let tmp = self.path.with_extension("tmp");
        let mut f =
            std::fs::OpenOptions::new().write(true).create(true).truncate(true).mode(0o600).open(&tmp).map_err(fail)?;
        f.set_permissions(std::fs::Permissions::from_mode(0o600)).map_err(fail)?;
        writeln!(f, "{token}").map_err(fail)?;
        drop(f);
        std::fs::rename(&tmp, &self.path).map_err(fail)
    }

    pub fn forget(&self) {
        let _ = std::fs::remove_file(&self.path);
    }
}

/// A request's object path: the portal puts it under the caller's unique
/// name (":1.42" as "1_42") and the handle_token it was given, so the
/// Response can be listened for before the call.
pub fn request_path(unique_name: &str, token: &str) -> String {
    let sender = unique_name.trim_start_matches(':').replace('.', "_");
    format!("/org/freedesktop/portal/desktop/request/{sender}/{token}")
}

pub type Options<'a> = HashMap<&'static str, Value<'a>>;

/// SelectDevices' options: a keyboard and a pointer, approved for good
/// (persist_mode 2, from version 2 of the portal) or by the restore token.
pub fn select_devices_options<'a>(token: &'a str, version: u32, restore: Option<&'a str>) -> Options<'a> {
    let mut o = Options::new();
    o.insert("handle_token", Value::from(token));
    o.insert("types", Value::from(KEYBOARD | POINTER));
    if version >= 2 {
        o.insert("persist_mode", Value::from(2u32));
        if let Some(r) = restore {
            o.insert("restore_token", Value::from(r));
        }
    }
    o
}

/// SelectSources' options: one monitor. A remote desktop session persists
/// through SelectDevices, so no persist_mode here.
pub fn select_sources_options(token: &str, cursor: CursorMode) -> Options<'_> {
    let mut o = Options::new();
    o.insert("handle_token", Value::from(token));
    o.insert("types", Value::from(MONITOR));
    o.insert("multiple", Value::from(false));
    o.insert("cursor_mode", Value::from(cursor as u32));
    o
}

/// A stream Start answered with.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stream {
    pub node_id: u32,
    /// In the compositor's (logical) coordinates, which absolute pointer
    /// motion uses.
    pub position: Option<(i32, i32)>,
    pub size: Option<(i32, i32)>,
}

type Results = HashMap<String, OwnedValue>;

fn pair(v: &OwnedValue) -> Option<(i32, i32)> {
    <(i32, i32)>::try_from(v.try_clone().ok()?).ok()
}

/// Start's results: the streams (the first is the screen), the devices
/// granted and the restore token for next time.
pub fn parse_start(results: &Results) -> Result<(Stream, u32, Option<String>), String> {
    let devices = results.get("devices").and_then(|v| u32::try_from(v).ok()).unwrap_or(0);
    let token = results.get("restore_token").and_then(|v| <&str>::try_from(v).ok()).map(str::to_string);
    let streams = results.get("streams").ok_or("the portal started without a screen cast stream")?;
    let streams = <Vec<(u32, HashMap<String, OwnedValue>)>>::try_from(
        streams.try_clone().map_err(|e| format!("the portal's streams: {e}"))?,
    )
    .map_err(|e| format!("the portal's streams: {e}"))?;
    let (node_id, props) = streams.into_iter().next().ok_or("the portal started without a screen cast stream")?;
    let stream =
        Stream { node_id, position: props.get("position").and_then(pair), size: props.get("size").and_then(pair) };
    Ok((stream, devices, token))
}

/// The screen as the stream delivers it.
pub trait Capture {
    /// The frames' size, in pixels.
    fn size(&self) -> (u32, u32);
    /// The latest frame.
    fn frame(&mut self) -> Result<RgbaImage, String>;
    /// The pointer's position in the frame from the cursor metadata, if the
    /// stream has it (and the pointer is on this screen).
    fn cursor(&mut self) -> Option<(i32, i32)>;
}

/// Opens the stream's capture: PipeWire's remote (from
/// OpenPipeWireRemote), the stream and its cursor mode.
pub type OpenCapture = dyn FnOnce(OwnedFd, &Stream, CursorMode) -> Result<Box<dyn Capture>, String>;

pub struct Portal {
    conn: Connection,
    session: OwnedObjectPath,
    stream: Stream,
    capture: Box<dyn Capture>,
    cursor_mode: CursorMode,
    screen: Screen,
    /// Where the helper last sent the pointer (frame pixels), until the
    /// stream's metadata catches up with it.
    pending: Option<((i32, i32), Instant)>,
    /// Where the helper last sent the pointer, for the drawn arrow when
    /// there is no metadata.
    sent: Option<(i32, i32)>,
}

pub struct Connect {
    pub conn: Connection,
    pub tokens: Option<TokenStore>,
    pub detail: String,
    pub open_capture: Box<OpenCapture>,
    /// How long the user has to answer the portal's dialog.
    pub answer_within: Duration,
}

/// Calls a method of the portal that answers with a Request, and waits for
/// its Response.
fn request<B>(
    conn: &Connection,
    iface: &str,
    method: &str,
    token: &str,
    body: &B,
    within: Duration,
) -> Result<Results, String>
where
    B: serde::Serialize + zbus::zvariant::DynamicType,
{
    let unique = conn.unique_name().ok_or("the D-Bus connection has no name")?.to_string();
    let path = request_path(&unique, token);
    let rule = zbus::MatchRule::builder()
        .msg_type(zbus::message::Type::Signal)
        .interface(REQUEST)
        .and_then(|b| b.member("Response"))
        .and_then(|b| b.path(path.clone()))
        .map(|b| b.build())
        .map_err(err("Response"))?;
    let mut responses = MessageIterator::for_match_rule(rule, conn, Some(4)).map_err(err("Response"))?;
    conn.call_method(Some(DESTINATION), PATH, Some(iface), method, body)
        .map_err(|e| format!("xdg-desktop-portal {method}: {e}"))?;
    let (tx, rx) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        let _ = tx.send(responses.next());
    });
    let msg = match rx.recv_timeout(within) {
        Ok(Some(Ok(m))) => m,
        Ok(Some(Err(e))) => return Err(format!("xdg-desktop-portal {method}: {e}")),
        Ok(None) => return Err(format!("xdg-desktop-portal {method}: the bus closed")),
        Err(_) => {
            return Err(format!(
                "xdg-desktop-portal {method}: no answer within {}s (approve the screen sharing and remote control \
                 dialog, then try again)",
                within.as_secs()
            ))
        }
    };
    let (code, results): (u32, Results) = msg.body().deserialize().map_err(err("Response"))?;
    match code {
        0 => Ok(results),
        1 => Err(format!(
            "xdg-desktop-portal {method}: the request was cancelled (the screen sharing and remote control dialog was \
             dismissed): allow it to use computer use"
        )),
        _ => Err(format!("xdg-desktop-portal {method} failed (response {code})")),
    }
}

/// The portal of this session's bus, with the token in ~/.prigh and the
/// screen over PipeWire.
pub fn open() -> Result<Portal, String> {
    let desktop = std::env::var("XDG_CURRENT_DESKTOP").ok().filter(|d| !d.is_empty());
    Portal::connect(Connect {
        conn: bus(None)?,
        tokens: TokenStore::default_path().map(|path| TokenStore { path }),
        detail: format!("{} (xdg-desktop-portal)", desktop.as_deref().unwrap_or("Wayland")),
        open_capture: Box::new(|fd, stream, cursor| {
            Ok(Box::new(crate::screencast::ScreenCast::open(fd, stream, cursor)?) as Box<dyn Capture>)
        }),
        answer_within: Duration::from_secs(300),
    })
}

impl Portal {
    pub fn connect(c: Connect) -> Result<Portal, String> {
        let conn = c.conn;
        let version = available(&conn).map_err(|e| format!("{e}: {}", crate::detect::PORTAL_INSTALL))?;
        let modes = u32_property(&conn, SCREEN_CAST, "AvailableCursorModes").unwrap_or(0);
        let cursor_mode = CursorMode::pick(modes);
        let pid = std::process::id();
        let token = |what: &str| format!("prigh_{pid}_{what}");

        let mut o = Options::new();
        let (t, st) = (token("create"), token("session"));
        o.insert("handle_token", Value::from(t.as_str()));
        o.insert("session_handle_token", Value::from(st.as_str()));
        let results = request(&conn, REMOTE_DESKTOP, "CreateSession", &t, &(o,), c.answer_within)?;
        let session = results
            .get("session_handle")
            .and_then(|v| match &**v {
                Value::Str(s) => Some(s.to_string()),
                Value::ObjectPath(p) => Some(p.to_string()),
                _ => None,
            })
            .ok_or("xdg-desktop-portal CreateSession: no session_handle")?;
        let session = OwnedObjectPath::try_from(session).map_err(|e| format!("xdg-desktop-portal session: {e}"))?;

        let restore = c.tokens.as_ref().and_then(TokenStore::load);
        let t = token("devices");
        let o = select_devices_options(&t, version, restore.as_deref());
        request(&conn, REMOTE_DESKTOP, "SelectDevices", &t, &(&session, o), c.answer_within)?;

        let t = token("sources");
        let o = select_sources_options(&t, cursor_mode);
        request(&conn, SCREEN_CAST, "SelectSources", &t, &(&session, o), c.answer_within)?;

        let t = token("start");
        let mut o = Options::new();
        o.insert("handle_token", Value::from(t.as_str()));
        let results = request(&conn, REMOTE_DESKTOP, "Start", &t, &(&session, "", o), c.answer_within)?;
        let (stream, devices, new_token) = parse_start(&results)?;
        if devices & (KEYBOARD | POINTER) != KEYBOARD | POINTER {
            return Err(format!(
                "xdg-desktop-portal granted devices {devices}: computer use needs the keyboard and the pointer (allow \
                 both in the dialog)"
            ));
        }
        if let Some(store) = &c.tokens {
            match &new_token {
                Some(t) => store.save(t)?,
                None => store.forget(),
            }
        }

        let reply = conn
            .call_method(Some(DESTINATION), PATH, Some(SCREEN_CAST), "OpenPipeWireRemote", &(&session, Options::new()))
            .map_err(err("OpenPipeWireRemote"))?;
        let fd: zbus::zvariant::OwnedFd = reply.body().deserialize().map_err(err("OpenPipeWireRemote"))?;
        let capture = (c.open_capture)(fd.into(), &stream, cursor_mode)?;
        let (width, height) = capture.size();
        let scale = match stream.size {
            Some((w, _)) if w > 0 => width as f64 / w as f64,
            _ => 1.0,
        };
        let screen = Screen { backend: "portal", detail: c.detail, output: None, width, height, scale };
        Ok(Portal { conn, session, stream, capture, cursor_mode, screen, pending: None, sent: None })
    }

    fn notify<B>(&self, method: &str, body: &B) -> Result<(), String>
    where
        B: serde::Serialize + zbus::zvariant::DynamicType,
    {
        self.conn.call_method(Some(DESTINATION), PATH, Some(REMOTE_DESKTOP), method, body).map_err(|e| {
            format!(
                "xdg-desktop-portal {method}: {e} (if the sharing was stopped, computer use needs a new session: \
                 restart the tool host)"
            )
        })?;
        Ok(())
    }

    fn keysym(&self, k: Keysym, down: bool) -> Result<(), String> {
        self.notify("NotifyKeyboardKeysym", &(&self.session, Options::new(), k as i32, down as u32))
    }

    /// A frame pixel in the stream's (logical) coordinates.
    pub fn to_stream(&self, x: i32, y: i32) -> (f64, f64) {
        match self.stream.size {
            Some((w, h)) if w > 0 && h > 0 => {
                (x as f64 * w as f64 / self.screen.width as f64, y as f64 * h as f64 / self.screen.height as f64)
            }
            _ => (x as f64, y as f64),
        }
    }
}

impl Drop for Portal {
    fn drop(&mut self) {
        let _ = self.conn.call_method(
            Some(DESTINATION),
            self.session.as_ref(),
            Some("org.freedesktop.portal.Session"),
            "Close",
            &(),
        );
    }
}

/// How long a pointer move may take to show in the stream's metadata.
const CATCH_UP: Duration = Duration::from_millis(500);

impl Backend for Portal {
    fn screen(&self) -> Screen {
        self.screen.clone()
    }

    fn capture(&mut self) -> Result<RgbaImage, String> {
        let mut img = self.capture.frame()?;
        if self.cursor_mode != CursorMode::Embedded {
            if let Some((x, y)) = self.cursor()?.or(self.sent) {
                draw_pointer(&mut img, x, y);
            }
        }
        Ok(img)
    }

    fn move_to(&mut self, x: i32, y: i32) -> Result<(), String> {
        let (sx, sy) = self.to_stream(x, y);
        self.notify("NotifyPointerMotionAbsolute", &(&self.session, Options::new(), self.stream.node_id, sx, sy))?;
        self.pending = Some(((x, y), Instant::now()));
        self.sent = Some((x, y));
        Ok(())
    }

    fn button(&mut self, button: Button, down: bool) -> Result<(), String> {
        self.notify("NotifyPointerButton", &(&self.session, Options::new(), button.evdev() as i32, down as u32))
    }

    fn scroll(&mut self, dx: i32, dy: i32) -> Result<(), String> {
        for (axis, steps) in [(0u32, dy), (1u32, dx)] {
            if steps != 0 {
                self.notify("NotifyPointerAxisDiscrete", &(&self.session, Options::new(), axis, steps))?;
            }
        }
        Ok(())
    }

    fn keys(&mut self, keys: &[Keysym], down: bool) -> Result<(), String> {
        keys.iter().try_for_each(|k| self.keysym(*k, down))
    }

    fn type_text(&mut self, text: &str) -> Result<(), String> {
        for c in text.chars() {
            let k = of_char(c);
            self.keysym(k, true)?;
            self.keysym(k, false)?;
        }
        Ok(())
    }

    fn cursor(&mut self) -> Result<Option<(i32, i32)>, String> {
        if self.cursor_mode != CursorMode::Metadata {
            return Ok(None);
        }
        let seen = self.capture.cursor();
        if let Some((target, at)) = self.pending {
            let caught_up = seen.is_some_and(|(x, y)| (x - target.0).abs() <= 2 && (y - target.1).abs() <= 2);
            if !caught_up && at.elapsed() < CATCH_UP {
                return Ok(Some(target));
            }
            self.pending = None;
        }
        Ok(seen)
    }

    fn sync(&mut self) -> Result<(), String> {
        // a round trip: the portal has handled what came before
        let _ = u32_property(&self.conn, REMOTE_DESKTOP, "version").map_err(err("sync"))?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn show(o: &Options) -> String {
        let mut keys: Vec<_> = o.iter().map(|(k, v)| format!("{k}={v}")).collect();
        keys.sort();
        keys.join(" ")
    }

    #[test]
    fn builds_requests() {
        assert_eq!(
            request_path(":1.42", "prigh_7_start"),
            "/org/freedesktop/portal/desktop/request/1_42/prigh_7_start"
        );
        assert_eq!(
            show(&select_devices_options("t", 2, None)),
            "handle_token=\"t\" persist_mode=uint32 2 types=uint32 3"
        );
        assert_eq!(
            show(&select_devices_options("t", 2, Some("abc"))),
            "handle_token=\"t\" persist_mode=uint32 2 restore_token=\"abc\" types=uint32 3"
        );
        // version 1 has no persistence
        assert_eq!(show(&select_devices_options("t", 1, Some("abc"))), "handle_token=\"t\" types=uint32 3");
        assert_eq!(
            show(&select_sources_options("t", CursorMode::Metadata)),
            "cursor_mode=uint32 4 handle_token=\"t\" multiple=false types=uint32 1"
        );
        assert_eq!(CursorMode::pick(7), CursorMode::Metadata);
        assert_eq!(CursorMode::pick(3), CursorMode::Embedded);
        assert_eq!(CursorMode::pick(0), CursorMode::Hidden);
    }

    #[test]
    fn parses_start() {
        let mut props: HashMap<String, OwnedValue> = HashMap::new();
        props.insert("size".into(), OwnedValue::try_from(Value::from((1280i32, 720i32))).unwrap());
        props.insert("position".into(), OwnedValue::try_from(Value::from((0i32, 0i32))).unwrap());
        let streams = vec![(57u32, props)];
        let mut results: Results = HashMap::new();
        results.insert("devices".into(), OwnedValue::from(3u32));
        results.insert("streams".into(), OwnedValue::try_from(Value::from(streams)).unwrap());
        results.insert("restore_token".into(), OwnedValue::try_from(Value::from("tok")).unwrap());
        let (s, devices, token) = parse_start(&results).unwrap();
        assert_eq!(s, Stream { node_id: 57, position: Some((0, 0)), size: Some((1280, 720)) });
        assert_eq!(devices, 3);
        assert_eq!(token.as_deref(), Some("tok"));
        results.remove("streams");
        assert!(parse_start(&results).unwrap_err().contains("without a screen cast stream"));
    }

    #[test]
    fn stores_tokens() {
        let dir = std::env::temp_dir().join(format!("prigh-computer-token-{}", std::process::id()));
        let store = TokenStore { path: dir.join("sub").join("token") };
        assert_eq!(store.load(), None);
        store.save("abc-123").unwrap();
        assert_eq!(store.load().as_deref(), Some("abc-123"));
        use std::os::unix::fs::PermissionsExt;
        let mode = std::fs::metadata(&store.path).unwrap().permissions().mode();
        assert_eq!(mode & 0o777, 0o600);
        store.forget();
        assert_eq!(store.load(), None);
        let _ = std::fs::remove_dir_all(dir);
    }
}
