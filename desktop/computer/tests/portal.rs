#![cfg(target_os = "linux")]

//! The portal backend against a fake xdg-desktop-portal on a private
//! dbus-daemon: the fake answers as the real one does (Request objects and
//! their Response signals, the session's streams, a restore token) and
//! records every call, which the test prints. The capture is a fake too:
//! the PipeWire side is not exercised here (see the README).

use image::RgbaImage;
use prigh_computer::driver::{Config, Driver, Format};
use prigh_computer::portal::{self, Capture, Connect, CursorMode, Portal, Stream, TokenStore};
use prigh_computer::scale::Limits;
use serde_json::Value as Json;
use std::collections::HashMap;
use std::io::BufRead;
use std::process::{Child, Command, Stdio};
use std::sync::{Arc, Mutex};
use std::time::Duration;
use zbus::blocking::{Connection, MessageIterator};
use zbus::message::Type;
use zbus::zvariant::{OwnedObjectPath, OwnedValue, Value};

struct Bus {
    daemon: Child,
    address: String,
}

impl Drop for Bus {
    fn drop(&mut self) {
        let _ = self.daemon.kill();
        let _ = self.daemon.wait();
    }
}

fn bus() -> Option<Bus> {
    let mut daemon = match Command::new("dbus-daemon")
        .args(["--session", "--nofork", "--nopidfile", "--print-address=1"])
        .stdout(Stdio::piped())
        .spawn()
    {
        Ok(d) => d,
        Err(e) if std::env::var_os("PRIGH_REQUIRE_DBUS").is_none() => {
            eprintln!("skipped: no dbus-daemon ({e})");
            return None;
        }
        Err(e) => panic!("dbus-daemon: {e}"),
    };
    let mut line = String::new();
    std::io::BufReader::new(daemon.stdout.take().unwrap()).read_line(&mut line).unwrap();
    Some(Bus { daemon, address: line.trim().to_string() })
}

#[derive(Default)]
struct Fake {
    log: Vec<String>,
    /// The pointer as the compositor has it (frame pixels), for the cursor
    /// metadata.
    cursor: Option<(i32, i32)>,
    /// Answers Start with "cancelled".
    deny: bool,
    starts: u32,
}

type Shared = Arc<Mutex<Fake>>;

/// a{sv} with the handle tokens (which have the pid) left out.
fn show(o: &HashMap<String, OwnedValue>) -> String {
    let mut v: Vec<String> = o
        .iter()
        .map(|(k, v)| if k.ends_with("handle_token") { format!("{k}=…") } else { format!("{k}={}", &**v) })
        .collect();
    v.sort();
    format!("{{{}}}", v.join(" "))
}

const SESSION: &str = "/org/freedesktop/portal/desktop/session/1/prigh";

/// The fake portal: serves until the bus goes away.
fn serve(address: &str, fake: Shared) {
    let conn = portal::bus(Some(address)).unwrap();
    conn.request_name(portal::DESTINATION).unwrap();
    std::thread::spawn(move || {
        for msg in MessageIterator::from(&conn) {
            let Ok(msg) = msg else { break };
            let hdr = msg.header();
            if hdr.message_type() != Type::MethodCall {
                continue;
            }
            let member = hdr.member().map(|m| m.to_string()).unwrap_or_default();
            let sender = hdr.sender().map(|s| s.to_string()).unwrap_or_default();
            let body = msg.body();
            let mut f = fake.lock().unwrap();
            // answers a request: the Response, then the request's handle
            let respond = |token: &str, code: u32, results: HashMap<&str, Value>| {
                let path = portal::request_path(&sender, token);
                conn.emit_signal(
                    Some(sender.as_str()),
                    path.as_str(),
                    "org.freedesktop.portal.Request",
                    "Response",
                    &(code, results),
                )
                .unwrap();
                conn.reply(&hdr, &(OwnedObjectPath::try_from(path).unwrap(),)).unwrap();
            };
            let token = |o: &HashMap<String, OwnedValue>| -> String {
                o.get("handle_token").and_then(|v| <&str>::try_from(v).ok()).unwrap_or("").to_string()
            };
            match member.as_str() {
                "Get" => {
                    let (iface, name): (String, String) = body.deserialize().unwrap();
                    let v: u32 = match (iface.as_str(), name.as_str()) {
                        (portal::REMOTE_DESKTOP, "version") => 2,
                        (portal::REMOTE_DESKTOP, "AvailableDeviceTypes") => 7,
                        (portal::SCREEN_CAST, "version") => 5,
                        (portal::SCREEN_CAST, "AvailableCursorModes") => 7,
                        _ => {
                            conn.reply_error(&hdr, "org.freedesktop.DBus.Error.UnknownProperty", &(name,)).unwrap();
                            continue;
                        }
                    };
                    conn.reply(&hdr, &(Value::from(v),)).unwrap();
                }
                "CreateSession" => {
                    let (o,): (HashMap<String, OwnedValue>,) = body.deserialize().unwrap();
                    f.log.push(format!("CreateSession {}", show(&o)));
                    let mut r = HashMap::new();
                    r.insert("session_handle", Value::from(SESSION));
                    respond(&token(&o), 0, r);
                }
                "SelectDevices" | "SelectSources" => {
                    let (s, o): (OwnedObjectPath, HashMap<String, OwnedValue>) = body.deserialize().unwrap();
                    f.log.push(format!("{member} {} {}", s.as_str(), show(&o)));
                    respond(&token(&o), 0, HashMap::new());
                }
                "Start" => {
                    let (s, parent, o): (OwnedObjectPath, String, HashMap<String, OwnedValue>) =
                        body.deserialize().unwrap();
                    f.log.push(format!("Start {} {parent:?} {}", s.as_str(), show(&o)));
                    if f.deny {
                        respond(&token(&o), 1, HashMap::new());
                        continue;
                    }
                    f.starts += 1;
                    let mut props: HashMap<&str, Value> = HashMap::new();
                    props.insert("size", Value::from((640i32, 400i32)));
                    props.insert("position", Value::from((0i32, 0i32)));
                    let mut r: HashMap<&str, Value> = HashMap::new();
                    r.insert("devices", Value::from(3u32));
                    r.insert("streams", Value::from(vec![(42u32, props)]));
                    r.insert("restore_token", Value::from(format!("token-{}", f.starts)));
                    f.cursor = Some((640, 400));
                    respond(&token(&o), 0, r);
                }
                "OpenPipeWireRemote" => {
                    let (s, o): (OwnedObjectPath, HashMap<String, OwnedValue>) = body.deserialize().unwrap();
                    f.log.push(format!("OpenPipeWireRemote {} {}", s.as_str(), show(&o)));
                    let file = std::fs::File::open("/dev/null").unwrap();
                    conn.reply(&hdr, &(zbus::zvariant::Fd::from(&file),)).unwrap();
                }
                "NotifyPointerMotionAbsolute" => {
                    let (_, _, node, x, y): (OwnedObjectPath, HashMap<String, OwnedValue>, u32, f64, f64) =
                        body.deserialize().unwrap();
                    f.log.push(format!("motion stream {node} {x},{y}"));
                    // the stream is twice the logical size
                    f.cursor = Some(((x * 2.0) as i32, (y * 2.0) as i32));
                    conn.reply(&hdr, &()).unwrap();
                }
                "NotifyPointerButton" => {
                    let (_, _, b, s): (OwnedObjectPath, HashMap<String, OwnedValue>, i32, u32) =
                        body.deserialize().unwrap();
                    f.log.push(format!("button 0x{b:x} {s}"));
                    conn.reply(&hdr, &()).unwrap();
                }
                "NotifyPointerAxisDiscrete" => {
                    let (_, _, axis, steps): (OwnedObjectPath, HashMap<String, OwnedValue>, u32, i32) =
                        body.deserialize().unwrap();
                    f.log.push(format!("axis {axis} {steps}"));
                    conn.reply(&hdr, &()).unwrap();
                }
                "NotifyKeyboardKeysym" => {
                    let (_, _, k, s): (OwnedObjectPath, HashMap<String, OwnedValue>, i32, u32) =
                        body.deserialize().unwrap();
                    f.log.push(format!("keysym 0x{k:x} {s}"));
                    conn.reply(&hdr, &()).unwrap();
                }
                "Close" => {
                    f.log.push(format!("Close {}", hdr.path().map(|p| p.as_str()).unwrap_or("")));
                    conn.reply(&hdr, &()).unwrap();
                }
                other => {
                    f.log.push(format!("unexpected {other}"));
                    conn.reply_error(&hdr, "org.freedesktop.DBus.Error.UnknownMethod", &(other,)).unwrap();
                }
            }
        }
    });
}

/// The stream: a grey 1280x800 frame (twice the logical 640x400), and the
/// fake's pointer as metadata.
struct FakeCapture(Shared);

impl Capture for FakeCapture {
    fn size(&self) -> (u32, u32) {
        (1280, 800)
    }
    fn frame(&mut self) -> Result<RgbaImage, String> {
        Ok(RgbaImage::from_pixel(1280, 800, image::Rgba([90, 110, 130, 255])))
    }
    fn cursor(&mut self) -> Option<(i32, i32)> {
        self.0.lock().unwrap().cursor
    }
}

fn connect(address: &str, fake: &Shared, tokens: &std::path::Path) -> Result<Portal, String> {
    let opened = fake.clone();
    let f2 = fake.clone();
    Portal::connect(Connect {
        conn: portal::bus(Some(address))?,
        tokens: Some(TokenStore { path: tokens.to_path_buf() }),
        detail: "fake (xdg-desktop-portal)".into(),
        open_capture: Box::new(move |_fd, stream: &Stream, cursor: CursorMode| {
            opened.lock().unwrap().log.push(format!("capture {stream:?} {cursor:?}"));
            Ok(Box::new(FakeCapture(f2)) as Box<dyn Capture>)
        }),
        answer_within: Duration::from_secs(10),
    })
}

fn act(d: &mut Driver, action: &str) -> String {
    let reply: Json = d.handle_line(&format!(r#"{{"id":1,"op":"act","action":{action}}}"#));
    let mut m = reply.as_object().unwrap().clone();
    m.remove("screenshot");
    m.remove("id");
    Json::Object(m).to_string()
}

fn take_log(fake: &Shared) -> String {
    std::mem::take(&mut fake.lock().unwrap().log).join("\n")
}

#[test]
fn portal_session() {
    let Some(bus) = bus() else { return };
    let fake: Shared = Arc::default();
    serve(&bus.address, fake.clone());
    let dir = std::env::temp_dir().join(format!("prigh-computer-portal-{}", std::process::id()));
    let tokens = dir.join("computer-portal-token");

    let portal = connect(&bus.address, &fake, &tokens).unwrap();
    assert_eq!(
        take_log(&fake),
        format!(
            "CreateSession {{handle_token=… session_handle_token=…}}
SelectDevices {SESSION} {{handle_token=… persist_mode=uint32 2 types=uint32 3}}
SelectSources {SESSION} {{cursor_mode=uint32 4 handle_token=… multiple=false types=uint32 1}}
Start {SESSION} \"\" {{handle_token=…}}
OpenPipeWireRemote {SESSION} {{}}
capture Stream {{ node_id: 42, position: Some((0, 0)), size: Some((640, 400)) }} Metadata"
        )
    );
    assert_eq!(std::fs::read_to_string(&tokens).unwrap(), "token-1\n");

    let config = Config {
        limits: Limits { max_long_side: 640, max_pixels: 1_000_000 },
        format: Format::Png,
        settle: Duration::ZERO,
        watch: Duration::from_secs(30),
    };
    let mut d = Driver::new(Box::new(portal), config);
    assert_eq!(
        d.info().to_string(),
        r#"{"backend":"portal","detail":"fake (xdg-desktop-portal)","height":800,"output":null,"platform":"linux","scale":2.0,"scaled_height":400,"scaled_width":640,"width":1280}"#
    );
    let replies = [
        act(&mut d, r#"{"action":"click","x":320,"y":100}"#),
        act(&mut d, r#"{"action":"type","text":"Hé→"}"#),
        act(&mut d, r#"{"action":"key","combo":"ctrl+a"}"#),
        act(&mut d, r#"{"action":"scroll","dy":-2,"dx":1}"#),
    ];
    assert_eq!(
        replies.join("\n"),
        r#"{"cursor":{"x":320,"y":100},"ok":true,"real":{"x":640,"y":200},"text":"clicked left at (320, 100)"}
{"cursor":{"x":320,"y":100},"ok":true,"text":"typed 3 characters"}
{"cursor":{"x":320,"y":100},"ok":true,"text":"pressed ctrl+a"}
{"cursor":{"x":320,"y":100},"ok":true,"text":"scrolled 1,-2"}"#
    );
    // motion is in the stream's logical coordinates: half the frame's
    assert_eq!(
        take_log(&fake),
        "motion stream 42 320,100
button 0x110 1
button 0x110 0
keysym 0x48 1
keysym 0x48 0
keysym 0xe9 1
keysym 0xe9 0
keysym 0x1002192 1
keysym 0x1002192 0
keysym 0xffe3 1
keysym 0x61 1
keysym 0x61 0
keysym 0xffe3 0
axis 0 -2
axis 1 1"
    );

    // the user takes the pointer: the kill switch sees it in the metadata
    fake.lock().unwrap().cursor = Some((100, 100));
    assert_eq!(
        act(&mut d, r#"{"action":"click","x":10,"y":10}"#),
        r#"{"error":"you moved the mouse: stopped (computer use paused until the next prompt)","interrupted":true,"ok":false}"#
    );
    assert_eq!(take_log(&fake), "");
    drop(d);
    assert_eq!(take_log(&fake), format!("Close {SESSION}"));

    // the next session presents the token, and keeps the new one
    let portal = connect(&bus.address, &fake, &tokens).unwrap();
    let log = take_log(&fake);
    assert!(log.contains("SelectDevices /org/freedesktop/portal/desktop/session/1/prigh {handle_token=… persist_mode=uint32 2 restore_token=\"token-1\" types=uint32 3}"), "{log}");
    assert_eq!(std::fs::read_to_string(&tokens).unwrap(), "token-2\n");
    drop(portal);
    take_log(&fake);

    // a dismissed dialog says what to do
    fake.lock().unwrap().deny = true;
    let e = connect(&bus.address, &fake, &tokens).err().unwrap();
    assert_eq!(
        e,
        "xdg-desktop-portal Start: the request was cancelled (the screen sharing and remote control dialog was \
         dismissed): allow it to use computer use"
    );
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn portal_missing() {
    let Some(bus) = bus() else { return };
    let conn: Connection = portal::bus(Some(&bus.address)).unwrap();
    let e = portal::available(&conn).unwrap_err();
    assert!(e.starts_with("the RemoteDesktop portal is not available ("), "{e}");
    assert!(e.contains("org.freedesktop.portal.Desktop"), "{e}");
}
