//! wlroots compositors (Hyprland, sway, river, ...): screenshots through
//! `wlr-screencopy-unstable-v1`, the pointer through
//! `zwlr_virtual_pointer_v1` bound to the controlled output, and keys
//! through `zwp_virtual_keyboard_v1` with a keymap of its own
//! ([crate::keymap]), so any character types whatever the user's layout.
//! No permission prompts: these protocols are open to any client.
//!
//! The output is the focused one (Hyprland and sway say which), else the
//! first. Where the pointer is can be asked only of Hyprland (its IPC):
//! elsewhere the kill switch has nothing to compare and stays off.

use crate::action::Button;
use crate::backend::{Backend, Screen};
use crate::keymap::{modifier_mask, Keymap};
use crate::keysym::{is_modifier, of_char, of_name, Keysym};
use image::RgbaImage;
use serde_json::Value;
use std::io::{Read, Write};
use std::os::fd::{AsFd, FromRawFd, OwnedFd};
use std::os::unix::net::UnixStream;
use std::path::PathBuf;
use std::time::Instant;
use wayland_client::globals::{registry_queue_init, GlobalListContents};
use wayland_client::protocol::{wl_buffer, wl_output, wl_registry, wl_seat, wl_shm, wl_shm_pool};
use wayland_client::{delegate_noop, Connection, Dispatch, EventQueue, Proxy, QueueHandle, WEnum};
use wayland_protocols_misc::zwp_virtual_keyboard_v1::client::{
    zwp_virtual_keyboard_manager_v1, zwp_virtual_keyboard_v1,
};
use wayland_protocols_wlr::screencopy::v1::client::{zwlr_screencopy_frame_v1, zwlr_screencopy_manager_v1};
use wayland_protocols_wlr::virtual_pointer::v1::client::{zwlr_virtual_pointer_manager_v1, zwlr_virtual_pointer_v1};

const NEEDED: [&str; 3] =
    ["zwlr_virtual_pointer_manager_v1", "zwlr_screencopy_manager_v1", "zwp_virtual_keyboard_manager_v1"];

/// Whether the compositor at $WAYLAND_DISPLAY offers what this backend
/// needs.
pub fn has_globals() -> Result<bool, String> {
    let conn = Connection::connect_to_env().map_err(|e| e.to_string())?;
    let (globals, _queue) = registry_queue_init::<Probe>(&conn).map_err(|e| e.to_string())?;
    let list = globals.contents().clone_list();
    Ok(NEEDED.iter().all(|n| list.iter().any(|g| g.interface == *n)))
}

struct Probe;
impl Dispatch<wl_registry::WlRegistry, GlobalListContents> for Probe {
    fn event(
        _: &mut Self,
        _: &wl_registry::WlRegistry,
        _: wl_registry::Event,
        _: &GlobalListContents,
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
    }
}

#[derive(Debug, Default, Clone)]
struct OutputInfo {
    name: Option<String>,
    width: i32,
    height: i32,
    scale: i32,
}

#[derive(Debug, Default)]
struct Frame {
    buffer: Option<(wl_shm::Format, u32, u32, u32)>,
    buffer_done: bool,
    flags: u32,
    ready: bool,
    failed: bool,
}

#[derive(Default)]
struct State {
    outputs: Vec<(wl_output::WlOutput, OutputInfo)>,
    frame: Frame,
}

impl Dispatch<wl_registry::WlRegistry, GlobalListContents> for State {
    fn event(
        _: &mut Self,
        _: &wl_registry::WlRegistry,
        _: wl_registry::Event,
        _: &GlobalListContents,
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
    }
}

impl Dispatch<wl_output::WlOutput, ()> for State {
    fn event(
        state: &mut Self,
        output: &wl_output::WlOutput,
        event: wl_output::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        let Some((_, info)) = state.outputs.iter_mut().find(|(o, _)| o == output) else { return };
        match event {
            wl_output::Event::Name { name } => info.name = Some(name),
            wl_output::Event::Scale { factor } => info.scale = factor,
            wl_output::Event::Mode { flags: WEnum::Value(flags), width, height, .. }
                if flags.contains(wl_output::Mode::Current) =>
            {
                info.width = width;
                info.height = height;
            }
            _ => {}
        }
    }
}

impl Dispatch<zwlr_screencopy_frame_v1::ZwlrScreencopyFrameV1, ()> for State {
    fn event(
        state: &mut Self,
        _: &zwlr_screencopy_frame_v1::ZwlrScreencopyFrameV1,
        event: zwlr_screencopy_frame_v1::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        use zwlr_screencopy_frame_v1::Event;
        match event {
            Event::Buffer { format: WEnum::Value(format), width, height, stride } => {
                state.frame.buffer = Some((format, width, height, stride))
            }
            Event::BufferDone => state.frame.buffer_done = true,
            Event::Flags { flags } => state.frame.flags = u32::from(flags),
            Event::Ready { .. } => state.frame.ready = true,
            Event::Failed => state.frame.failed = true,
            _ => {}
        }
    }
}

delegate_noop!(State: ignore wl_shm::WlShm);
delegate_noop!(State: ignore wl_seat::WlSeat);
delegate_noop!(State: ignore wl_buffer::WlBuffer);
delegate_noop!(State: wl_shm_pool::WlShmPool);
delegate_noop!(State: zwlr_screencopy_manager_v1::ZwlrScreencopyManagerV1);
delegate_noop!(State: zwlr_virtual_pointer_manager_v1::ZwlrVirtualPointerManagerV1);
delegate_noop!(State: zwlr_virtual_pointer_v1::ZwlrVirtualPointerV1);
delegate_noop!(State: zwp_virtual_keyboard_manager_v1::ZwpVirtualKeyboardManagerV1);
delegate_noop!(State: zwp_virtual_keyboard_v1::ZwpVirtualKeyboardV1);

/// A shared-memory file of `size` bytes.
fn memfd(size: usize) -> Result<OwnedFd, String> {
    let fd = unsafe { libc::memfd_create(c"prigh-computer".as_ptr(), libc::MFD_CLOEXEC) };
    if fd < 0 {
        return Err(format!("memfd_create: {}", std::io::Error::last_os_error()));
    }
    let fd = unsafe { OwnedFd::from_raw_fd(fd) };
    let file = std::fs::File::from(fd.try_clone().map_err(|e| e.to_string())?);
    file.set_len(size as u64).map_err(|e| format!("sizing shared memory: {e}"))?;
    Ok(fd)
}

/// Hyprland's IPC socket, when this is Hyprland.
fn hyprland_socket() -> Option<PathBuf> {
    let sig = std::env::var("HYPRLAND_INSTANCE_SIGNATURE").ok()?;
    let runtime = std::env::var("XDG_RUNTIME_DIR").unwrap_or_default();
    [
        PathBuf::from(&runtime).join("hypr").join(&sig).join(".socket.sock"),
        PathBuf::from("/tmp/hypr").join(&sig).join(".socket.sock"),
    ]
    .into_iter()
    .find(|p| p.exists())
}

fn hyprland_ipc(socket: &PathBuf, request: &str) -> Result<Value, String> {
    let mut s = UnixStream::connect(socket).map_err(|e| format!("Hyprland IPC: {e}"))?;
    s.write_all(request.as_bytes()).map_err(|e| format!("Hyprland IPC: {e}"))?;
    let mut out = String::new();
    s.read_to_string(&mut out).map_err(|e| format!("Hyprland IPC: {e}"))?;
    serde_json::from_str(&out).map_err(|e| format!("Hyprland IPC {request}: {e}"))
}

/// i3/sway IPC: one message of `kind`, its JSON reply.
fn sway(kind: u32) -> Result<Value, String> {
    let path = std::env::var("SWAYSOCK").map_err(|_| "no $SWAYSOCK".to_string())?;
    let mut s = UnixStream::connect(path).map_err(|e| format!("sway IPC: {e}"))?;
    let mut msg = b"i3-ipc".to_vec();
    msg.extend_from_slice(&0u32.to_le_bytes());
    msg.extend_from_slice(&kind.to_le_bytes());
    s.write_all(&msg).map_err(|e| format!("sway IPC: {e}"))?;
    let mut header = [0u8; 14];
    s.read_exact(&mut header).map_err(|e| format!("sway IPC: {e}"))?;
    let len = u32::from_le_bytes(header[6..10].try_into().unwrap()) as usize;
    let mut body = vec![0u8; len];
    s.read_exact(&mut body).map_err(|e| format!("sway IPC: {e}"))?;
    serde_json::from_slice(&body).map_err(|e| format!("sway IPC: {e}"))
}

fn focused_name(outputs: &Value) -> Option<String> {
    outputs.as_array()?.iter().find(|o| o["focused"].as_bool() == Some(true))?["name"].as_str().map(str::to_string)
}

pub struct Wlroots {
    queue: EventQueue<State>,
    qh: QueueHandle<State>,
    state: State,
    shm: wl_shm::WlShm,
    screencopy: zwlr_screencopy_manager_v1::ZwlrScreencopyManagerV1,
    pointer: zwlr_virtual_pointer_v1::ZwlrVirtualPointerV1,
    keyboard: zwp_virtual_keyboard_v1::ZwpVirtualKeyboardV1,
    output: wl_output::WlOutput,
    screen: Screen,
    keymap: Keymap,
    mods: u32,
    start: Instant,
    hyprland: Option<PathBuf>,
}

impl Wlroots {
    pub fn connect() -> Result<Wlroots, String> {
        let conn =
            Connection::connect_to_env().map_err(|e| format!("cannot connect to the Wayland compositor: {e}"))?;
        let (globals, mut queue) = registry_queue_init::<State>(&conn).map_err(|e| format!("Wayland registry: {e}"))?;
        let qh = queue.handle();
        let missing = |what: &str, e: wayland_client::globals::BindError| {
            format!("the compositor lacks {what} ({e}): it is not a wlroots compositor (Hyprland, sway, ...)")
        };
        let shm: wl_shm::WlShm = globals.bind(&qh, 1..=1, ()).map_err(|e| missing("wl_shm", e))?;
        let seat: wl_seat::WlSeat = globals.bind(&qh, 1..=1, ()).map_err(|e| missing("wl_seat", e))?;
        let screencopy: zwlr_screencopy_manager_v1::ZwlrScreencopyManagerV1 =
            globals.bind(&qh, 1..=3, ()).map_err(|e| missing("wlr-screencopy", e))?;
        let pointers: zwlr_virtual_pointer_manager_v1::ZwlrVirtualPointerManagerV1 =
            globals.bind(&qh, 1..=2, ()).map_err(|e| missing("the wlr virtual pointer", e))?;
        let keyboards: zwp_virtual_keyboard_manager_v1::ZwpVirtualKeyboardManagerV1 =
            globals.bind(&qh, 1..=1, ()).map_err(|e| missing("the virtual keyboard", e))?;
        let mut state = State::default();
        for g in globals.contents().clone_list() {
            if g.interface == "wl_output" {
                let o: wl_output::WlOutput = globals.registry().bind(g.name, g.version.min(4), &qh, ());
                state.outputs.push((o, OutputInfo { scale: 1, ..Default::default() }));
            }
        }
        queue.roundtrip(&mut state).map_err(|e| e.to_string())?;
        queue.roundtrip(&mut state).map_err(|e| e.to_string())?;
        if state.outputs.is_empty() {
            return Err("the compositor has no outputs (no screen to control)".into());
        }
        let hyprland = hyprland_socket();
        let (detail, focused) = if let Some(socket) = &hyprland {
            ("Hyprland".to_string(), hyprland_ipc(socket, "j/monitors").ok().as_ref().and_then(focused_name))
        } else if std::env::var("SWAYSOCK").is_ok() {
            ("sway".to_string(), sway(3).ok().as_ref().and_then(focused_name))
        } else {
            (std::env::var("XDG_CURRENT_DESKTOP").unwrap_or_else(|_| "wlroots".into()), None)
        };
        let index = focused
            .and_then(|name| state.outputs.iter().position(|(_, i)| i.name.as_deref() == Some(name.as_str())))
            .unwrap_or(0);
        let (output, info) = state.outputs[index].clone();
        let pointer = if pointers.version() >= 2 {
            pointers.create_virtual_pointer_with_output(Some(&seat), Some(&output), &qh, ())
        } else {
            pointers.create_virtual_pointer(Some(&seat), &qh, ())
        };
        let keyboard = keyboards.create_virtual_keyboard(&seat, &qh, ());
        let mut me = Wlroots {
            queue,
            qh,
            state,
            shm,
            screencopy,
            pointer,
            keyboard,
            output,
            screen: Screen {
                backend: "wlroots",
                detail,
                output: info.name.clone(),
                width: info.width.max(1) as u32,
                height: info.height.max(1) as u32,
                scale: info.scale.max(1) as f64,
            },
            keymap: Keymap::default(),
            mods: 0,
            start: Instant::now(),
            hyprland,
        };
        me.keymap.keycode(of_name("Return").unwrap());
        me.upload_keymap()?;
        // The real size is what screencopy gives (a rotated output's too).
        let first = me.capture()?;
        me.screen.width = first.width();
        me.screen.height = first.height();
        Ok(me)
    }

    fn time(&self) -> u32 {
        self.start.elapsed().as_millis() as u32
    }

    fn flush(&mut self) -> Result<(), String> {
        self.queue.flush().map_err(|e| format!("Wayland: {e}"))
    }

    fn upload_keymap(&mut self) -> Result<(), String> {
        let mut text = self.keymap.text().into_bytes();
        text.push(0);
        let fd = memfd(text.len())?;
        let mut file = std::fs::File::from(fd.try_clone().map_err(|e| e.to_string())?);
        file.write_all(&text).map_err(|e| format!("writing the keymap: {e}"))?;
        self.keyboard.keymap(1, fd.as_fd(), text.len() as u32);
        self.flush()
    }

    fn key(&mut self, keycode: u32, down: bool) {
        let t = self.time();
        self.keyboard.key(t, Keymap::evdev(keycode), down as u32);
    }

    fn pointer_frame(&mut self) -> Result<(), String> {
        self.pointer.frame();
        self.flush()
    }
}

impl Backend for Wlroots {
    fn screen(&self) -> Screen {
        self.screen.clone()
    }

    fn capture(&mut self) -> Result<RgbaImage, String> {
        self.state.frame = Frame::default();
        let frame = self.screencopy.capture_output(1, &self.output, &self.qh, ());
        let v3 = frame.version() >= 3;
        while self.state.frame.buffer.is_none() || (v3 && !self.state.frame.buffer_done) {
            if self.state.frame.failed {
                frame.destroy();
                return Err("the compositor refused the screenshot (screencopy failed)".into());
            }
            self.queue.blocking_dispatch(&mut self.state).map_err(|e| format!("Wayland: {e}"))?;
        }
        let (format, w, h, stride) = self.state.frame.buffer.unwrap();
        let size = (stride * h) as usize;
        let fd = memfd(size)?;
        let pool = self.shm.create_pool(fd.as_fd(), size as i32, &self.qh, ());
        let buffer = pool.create_buffer(0, w as i32, h as i32, stride as i32, format, &self.qh, ());
        frame.copy(&buffer);
        let result = (|| {
            while !self.state.frame.ready {
                if self.state.frame.failed {
                    return Err("the compositor could not copy the screen (screencopy failed)".to_string());
                }
                self.queue.blocking_dispatch(&mut self.state).map_err(|e| format!("Wayland: {e}"))?;
            }
            let file = std::fs::File::from(fd.try_clone().map_err(|e| e.to_string())?);
            let map = unsafe { memmap2::Mmap::map(&file) }.map_err(|e| format!("mapping the screenshot: {e}"))?;
            // wl_shm formats are little-endian: ARGB8888 is B, G, R, A in memory.
            let order: [usize; 3] = match format {
                wl_shm::Format::Argb8888 | wl_shm::Format::Xrgb8888 => [2, 1, 0],
                wl_shm::Format::Abgr8888 | wl_shm::Format::Xbgr8888 => [0, 1, 2],
                other => return Err(format!("unsupported screencopy format {other:?}")),
            };
            let y_invert = self.state.frame.flags & 1 != 0;
            Ok(RgbaImage::from_fn(w, h, |x, y| {
                let row = if y_invert { h - 1 - y } else { y };
                let p = (row * stride + x * 4) as usize;
                image::Rgba([map[p + order[0]], map[p + order[1]], map[p + order[2]], 255])
            }))
        })();
        buffer.destroy();
        pool.destroy();
        frame.destroy();
        self.flush()?;
        result
    }

    fn move_to(&mut self, x: i32, y: i32) -> Result<(), String> {
        let t = self.time();
        self.pointer.motion_absolute(t, x.max(0) as u32, y.max(0) as u32, self.screen.width, self.screen.height);
        self.pointer_frame()
    }

    fn button(&mut self, button: Button, down: bool) -> Result<(), String> {
        let t = self.time();
        let state = if down { wl_pointer_state::Pressed } else { wl_pointer_state::Released };
        self.pointer.button(t, button.evdev(), state);
        self.pointer_frame()
    }

    fn scroll(&mut self, dx: i32, dy: i32) -> Result<(), String> {
        use wayland_client::protocol::wl_pointer::{Axis, AxisSource};
        let t = self.time();
        self.pointer.axis_source(AxisSource::Wheel);
        if dy != 0 {
            self.pointer.axis_discrete(t, Axis::VerticalScroll, 15.0 * dy as f64, dy);
        }
        if dx != 0 {
            self.pointer.axis_discrete(t, Axis::HorizontalScroll, 15.0 * dx as f64, dx);
        }
        self.pointer_frame()
    }

    fn keys(&mut self, keys: &[Keysym], down: bool) -> Result<(), String> {
        let mut added = false;
        let codes: Vec<Option<u32>> = keys
            .iter()
            .map(|k| {
                if is_modifier(*k) {
                    None
                } else {
                    let (c, a) = self.keymap.keycode(*k);
                    added |= a;
                    Some(c)
                }
            })
            .collect();
        if added {
            self.upload_keymap()?;
        }
        for (k, code) in keys.iter().zip(codes) {
            match code {
                None => {
                    let m = modifier_mask(*k);
                    self.mods = if down { self.mods | m } else { self.mods & !m };
                    self.keyboard.modifiers(self.mods, 0, 0, 0);
                }
                Some(c) => self.key(c, down),
            }
        }
        self.flush()
    }

    fn type_text(&mut self, text: &str) -> Result<(), String> {
        let syms: Vec<Keysym> = text.chars().map(of_char).collect();
        if self.keymap.len() + syms.len() > 240 {
            self.keymap = Keymap::default();
        }
        let mut added = false;
        let codes: Vec<u32> = syms
            .iter()
            .map(|k| {
                let (c, a) = self.keymap.keycode(*k);
                added |= a;
                c
            })
            .collect();
        if added {
            self.upload_keymap()?;
        }
        for c in codes {
            self.key(c, true);
            self.key(c, false);
            self.flush()?;
            std::thread::sleep(std::time::Duration::from_millis(2));
        }
        Ok(())
    }

    fn cursor(&mut self) -> Result<Option<(i32, i32)>, String> {
        let Some(socket) = &self.hyprland else { return Ok(None) };
        let pos = hyprland_ipc(socket, "j/cursorpos")?;
        let monitors = hyprland_ipc(socket, "j/monitors")?;
        let name = self.screen.output.clone().unwrap_or_default();
        let Some(m) = monitors.as_array().and_then(|ms| ms.iter().find(|m| m["name"].as_str() == Some(&name))) else {
            return Ok(None);
        };
        let f = |v: &Value| v.as_f64().unwrap_or(0.0);
        let scale = f(&m["scale"]).max(0.1);
        let x = (f(&pos["x"]) - f(&m["x"])) * scale;
        let y = (f(&pos["y"]) - f(&m["y"])) * scale;
        Ok(Some((x.round() as i32, y.round() as i32)))
    }

    fn sync(&mut self) -> Result<(), String> {
        self.queue.roundtrip(&mut self.state).map(|_| ()).map_err(|e| format!("Wayland: {e}"))
    }
}

use wayland_client::protocol::wl_pointer::ButtonState as wl_pointer_state;
