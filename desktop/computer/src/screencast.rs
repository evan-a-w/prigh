//! A ScreenCast stream's frames ([crate::portal::Capture]) over PipeWire:
//! a thread runs the PipeWire loop, holds on to the newest frame (a
//! compositor sends one only when the screen changes, so the last one is
//! the screen now) and the cursor metadata, and copies the frame out when
//! asked.

use crate::portal::{Capture, CursorMode, Stream};
use image::RgbaImage;
use pipewire as pw;
use pw::spa;
use pw::spa::pod::{ChoiceValue, Object, Pod, Property, PropertyFlags, Value};
use pw::spa::utils::{Choice, ChoiceEnum, ChoiceFlags, Fraction, Id, Rectangle};
use std::cell::RefCell;
use std::os::fd::OwnedFd;
use std::rc::Rc;
use std::sync::{mpsc, Arc, Mutex};
use std::time::Duration;

/// Where a format's red, green and blue bytes are in each 4-byte pixel.
pub fn channel_order(format: spa::param::video::VideoFormat) -> Option<[usize; 3]> {
    use spa::param::video::VideoFormat as F;
    match format {
        F::BGRx | F::BGRA => Some([2, 1, 0]),
        F::RGBx | F::RGBA => Some([0, 1, 2]),
        F::xRGB | F::ARGB => Some([1, 2, 3]),
        F::xBGR | F::ABGR => Some([3, 2, 1]),
        _ => None,
    }
}

/// A frame's bytes (rows `stride` apart) as an image.
pub fn to_image(data: &[u8], width: u32, height: u32, stride: usize, order: [usize; 3]) -> Result<RgbaImage, String> {
    if stride < width as usize * 4 || data.len() < stride * (height as usize - 1) + width as usize * 4 {
        return Err(format!("a {width}x{height} frame of {} bytes with stride {stride} is truncated", data.len()));
    }
    Ok(RgbaImage::from_fn(width, height, |x, y| {
        let p = y as usize * stride + x as usize * 4;
        image::Rgba([data[p + order[0]], data[p + order[1]], data[p + order[2]], 255])
    }))
}

enum Ask {
    Frame,
    Quit,
}

#[derive(Default)]
struct Shared {
    cursor: Option<(i32, i32)>,
}

pub struct ScreenCast {
    size: (u32, u32),
    shared: Arc<Mutex<Shared>>,
    ask: pw::channel::Sender<Ask>,
    frames: mpsc::Receiver<Result<RgbaImage, String>>,
    thread: Option<std::thread::JoinHandle<()>>,
}

/// How long to wait for the stream's first frame.
const FIRST_FRAME: Duration = Duration::from_secs(10);

impl ScreenCast {
    pub fn open(fd: OwnedFd, stream: &Stream, cursor: CursorMode) -> Result<ScreenCast, String> {
        let shared = Arc::new(Mutex::new(Shared::default()));
        let (ask, asked) = pw::channel::channel();
        let (ready_tx, ready) = mpsc::channel();
        let (frames_tx, frames) = mpsc::channel();
        let node = stream.node_id;
        let metadata = cursor == CursorMode::Metadata;
        let thread = {
            let shared = shared.clone();
            std::thread::Builder::new()
                .name("pipewire".into())
                .spawn(move || {
                    if let Err(e) = run(fd, node, metadata, shared, asked, ready_tx.clone(), frames_tx) {
                        let _ = ready_tx.send(Err(e));
                    }
                })
                .map_err(|e| format!("starting the PipeWire thread: {e}"))?
        };
        let size = match ready.recv_timeout(FIRST_FRAME) {
            Ok(Ok(size)) => size,
            Ok(Err(e)) => return Err(e),
            Err(_) => {
                let _ = ask.send(Ask::Quit);
                return Err(format!(
                    "the screen cast sent no frame within {}s (PipeWire node {node})",
                    FIRST_FRAME.as_secs()
                ));
            }
        };
        Ok(ScreenCast { size, shared, ask, frames, thread: Some(thread) })
    }
}

impl Drop for ScreenCast {
    fn drop(&mut self) {
        let _ = self.ask.send(Ask::Quit);
        if let Some(t) = self.thread.take() {
            let _ = t.join();
        }
    }
}

impl Capture for ScreenCast {
    fn size(&self) -> (u32, u32) {
        self.size
    }

    fn frame(&mut self) -> Result<RgbaImage, String> {
        self.ask.send(Ask::Frame).map_err(|_| "the screen cast has stopped".to_string())?;
        match self.frames.recv_timeout(Duration::from_secs(5)) {
            Ok(r) => r,
            Err(_) => Err("the screen cast has stopped (no frame within 5s)".into()),
        }
    }

    fn cursor(&mut self) -> Option<(i32, i32)> {
        self.shared.lock().unwrap().cursor
    }
}

fn pod(value: Value) -> Result<Vec<u8>, String> {
    Ok(spa::pod::serialize::PodSerializer::serialize(std::io::Cursor::new(Vec::new()), &value)
        .map_err(|e| format!("PipeWire parameters: {e:?}"))?
        .0
        .into_inner())
}

fn object(type_: spa::utils::SpaTypes, id: spa::param::ParamType, properties: Vec<Property>) -> Value {
    Value::Object(Object { type_: type_.as_raw(), id: id.as_raw(), properties })
}

fn int_range(default: i32, min: i32, max: i32) -> Value {
    Value::Choice(ChoiceValue::Int(Choice(ChoiceFlags::empty(), ChoiceEnum::Range { default, min, max })))
}

/// The formats this end reads: 32-bit RGB in any order, in memory (no
/// DMA-BUF: without modifiers in the offer, compositors send memfds).
fn enum_format() -> Result<Vec<u8>, String> {
    use spa::param::format::{FormatProperties as P, MediaSubtype, MediaType};
    use spa::param::video::VideoFormat as F;
    let formats = [F::BGRx, F::BGRA, F::RGBx, F::RGBA, F::xRGB, F::ARGB, F::xBGR, F::ABGR];
    let prop = |key: P, value| Property { key: key.as_raw(), flags: PropertyFlags::empty(), value };
    pod(object(
        spa::utils::SpaTypes::ObjectParamFormat,
        spa::param::ParamType::EnumFormat,
        vec![
            prop(P::MediaType, Value::Id(Id(MediaType::Video.as_raw()))),
            prop(P::MediaSubtype, Value::Id(Id(MediaSubtype::Raw.as_raw()))),
            prop(
                P::VideoFormat,
                Value::Choice(ChoiceValue::Id(Choice(
                    ChoiceFlags::empty(),
                    ChoiceEnum::Enum {
                        default: Id(formats[0].as_raw()),
                        alternatives: formats.iter().map(|f| Id(f.as_raw())).collect(),
                    },
                ))),
            ),
            prop(
                P::VideoSize,
                Value::Choice(ChoiceValue::Rectangle(Choice(
                    ChoiceFlags::empty(),
                    ChoiceEnum::Range {
                        default: Rectangle { width: 1920, height: 1080 },
                        min: Rectangle { width: 1, height: 1 },
                        max: Rectangle { width: 16384, height: 16384 },
                    },
                ))),
            ),
            prop(
                P::VideoFramerate,
                Value::Choice(ChoiceValue::Fraction(Choice(
                    ChoiceFlags::empty(),
                    ChoiceEnum::Range {
                        default: Fraction { num: 30, denom: 1 },
                        min: Fraction { num: 0, denom: 1 },
                        max: Fraction { num: 1000, denom: 1 },
                    },
                ))),
            ),
        ],
    ))
}

/// Once the format is fixed: buffers in memory, and room for the cursor
/// metadata (with a bitmap of up to 1024x1024, which it is never read).
fn buffer_params(metadata: bool) -> Result<Vec<Vec<u8>>, String> {
    use spa::sys::*;
    let mut out = vec![pod(object(
        spa::utils::SpaTypes::ObjectParamBuffers,
        spa::param::ParamType::Buffers,
        vec![Property::new(SPA_PARAM_BUFFERS_dataType, Value::Int((1 << SPA_DATA_MemFd) | (1 << SPA_DATA_MemPtr)))],
    ))?];
    if metadata {
        let size = |w: i32, h: i32| {
            (std::mem::size_of::<spa_meta_cursor>() + std::mem::size_of::<spa_meta_bitmap>()) as i32 + w * h * 4
        };
        out.push(pod(object(
            spa::utils::SpaTypes::ObjectParamMeta,
            spa::param::ParamType::Meta,
            vec![
                Property::new(SPA_PARAM_META_type, Value::Id(Id(SPA_META_Cursor))),
                Property::new(SPA_PARAM_META_size, int_range(size(64, 64), size(1, 1), size(1024, 1024))),
            ],
        ))?);
    }
    Ok(out)
}

struct State {
    format: spa::param::video::VideoInfoRaw,
    /// The newest buffer with a frame, kept from the compositor until a
    /// newer one arrives.
    held: *mut pw::sys::pw_buffer,
    ready: Option<mpsc::Sender<Result<(u32, u32), String>>>,
}

/// The cursor metadata's position, if this buffer has some.
unsafe fn cursor_of(buf: *mut spa::sys::spa_buffer) -> Option<(i32, i32)> {
    let b = &*buf;
    for i in 0..b.n_metas as usize {
        let m = &*b.metas.add(i);
        if m.type_ == spa::sys::SPA_META_Cursor
            && m.size as usize >= std::mem::size_of::<spa::sys::spa_meta_cursor>()
            && !m.data.is_null()
        {
            let c = &*(m.data as *const spa::sys::spa_meta_cursor);
            if c.id != 0 {
                return Some((c.position.x, c.position.y));
            }
        }
    }
    None
}

/// Whether the buffer carries a whole frame (not only cursor metadata).
unsafe fn has_frame(buf: *mut spa::sys::spa_buffer) -> bool {
    let b = &*buf;
    if b.n_datas == 0 {
        return false;
    }
    let d = &*b.datas;
    if d.chunk.is_null() {
        return false;
    }
    let c = &*d.chunk;
    c.size > 0 && c.flags & spa::sys::SPA_CHUNK_FLAG_CORRUPTED as i32 == 0
}

unsafe fn copy_frame(state: &State) -> Result<RgbaImage, String> {
    if state.held.is_null() {
        return Err("the screen cast has sent no frame yet".into());
    }
    let b = &*(*state.held).buffer;
    let d = &*b.datas;
    if d.data.is_null() {
        return Err(format!(
            "the screen cast's frames are not in memory (data type {}): prigh-computer reads memfd and shared \
             memory frames only",
            d.type_
        ));
    }
    let c = &*d.chunk;
    let size = state.format.size();
    let order = channel_order(state.format.format())
        .ok_or_else(|| format!("the screen cast's pixel format {:?} is not supported", state.format.format()))?;
    let stride = if c.stride > 0 { c.stride as usize } else { size.width as usize * 4 };
    let offset = (c.offset % d.maxsize.max(1)) as usize;
    let len = (d.maxsize as usize).saturating_sub(offset);
    let bytes = std::slice::from_raw_parts((d.data as *const u8).add(offset), len);
    to_image(bytes, size.width, size.height, stride, order)
}

fn run(
    fd: OwnedFd,
    node: u32,
    metadata: bool,
    shared: Arc<Mutex<Shared>>,
    asked: pw::channel::Receiver<Ask>,
    ready: mpsc::Sender<Result<(u32, u32), String>>,
    frames: mpsc::Sender<Result<RgbaImage, String>>,
) -> Result<(), String> {
    pw::init();
    let fail = |what: &'static str| move |e: pw::Error| format!("PipeWire {what}: {e}");
    let mainloop = pw::main_loop::MainLoopRc::new(None).map_err(fail("main loop"))?;
    let context = pw::context::ContextRc::new(&mainloop, None).map_err(fail("context"))?;
    let core = context.connect_fd_rc(fd, None).map_err(fail("connecting to the portal's remote"))?;
    let stream = pw::stream::StreamRc::new(
        core,
        "prigh-computer",
        pw::properties::properties! {
            *pw::keys::MEDIA_TYPE => "Video",
            *pw::keys::MEDIA_CATEGORY => "Capture",
            *pw::keys::MEDIA_ROLE => "Screen",
        },
    )
    .map_err(fail("stream"))?;
    let state =
        Rc::new(RefCell::new(State { format: Default::default(), held: std::ptr::null_mut(), ready: Some(ready) }));

    let _listener = stream
        .add_local_listener_with_user_data(())
        .state_changed({
            let state = state.clone();
            move |_, _, _, new| {
                if let pw::stream::StreamState::Error(e) = new {
                    if let Some(r) = state.borrow_mut().ready.take() {
                        let _ = r.send(Err(format!("the screen cast stream failed: {e}")));
                    }
                }
            }
        })
        .param_changed({
            let state = state.clone();
            move |stream, _, id, param| {
                let Some(param) = param else { return };
                if id != spa::param::ParamType::Format.as_raw() {
                    return;
                }
                if state.borrow_mut().format.parse(param).is_err() {
                    return;
                }
                let Ok(params) = buffer_params(metadata) else { return };
                let mut pods: Vec<&Pod> = params.iter().filter_map(|p| Pod::from_bytes(p)).collect();
                let _ = stream.update_params(&mut pods);
            }
        })
        .process({
            let state = state.clone();
            let shared = shared.clone();
            move |stream, _| {
                let mut st = state.borrow_mut();
                loop {
                    let b = unsafe { stream.dequeue_raw_buffer() };
                    if b.is_null() {
                        break;
                    }
                    unsafe {
                        let buf = (*b).buffer;
                        if let Some(p) = cursor_of(buf) {
                            shared.lock().unwrap().cursor = Some(p);
                        }
                        if has_frame(buf) {
                            if !st.held.is_null() {
                                stream.queue_raw_buffer(st.held);
                            }
                            st.held = b;
                        } else {
                            stream.queue_raw_buffer(b);
                        }
                    }
                }
                if !st.held.is_null() {
                    let size = st.format.size();
                    if let Some(r) = st.ready.take() {
                        let _ = r.send(Ok((size.width, size.height)));
                    }
                }
            }
        })
        .register()
        .map_err(fail("stream listener"))?;

    let format = enum_format()?;
    let mut params = [Pod::from_bytes(&format).ok_or("PipeWire parameters: a bad pod")?];
    stream
        .connect(
            spa::utils::Direction::Input,
            Some(node),
            pw::stream::StreamFlags::AUTOCONNECT | pw::stream::StreamFlags::MAP_BUFFERS,
            &mut params,
        )
        .map_err(fail("connecting the stream"))?;

    let _asked = asked.attach(mainloop.loop_(), {
        let mainloop = mainloop.clone();
        let state = state.clone();
        move |ask| match ask {
            Ask::Frame => {
                let _ = frames.send(unsafe { copy_frame(&state.borrow()) });
            }
            Ask::Quit => mainloop.quit(),
        }
    });
    mainloop.run();
    let _ = stream.disconnect();
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use spa::param::video::VideoFormat as F;

    #[test]
    fn converts_frames() {
        // 2x2, BGRx with a padded stride of 12
        let data = [
            1, 2, 3, 0, 4, 5, 6, 0, 9, 9, 9, 9, //
            7, 8, 9, 0, 10, 11, 12, 0, 9, 9, 9, 9,
        ];
        let img = to_image(&data, 2, 2, 12, channel_order(F::BGRx).unwrap()).unwrap();
        assert_eq!(img.get_pixel(0, 0).0, [3, 2, 1, 255]);
        assert_eq!(img.get_pixel(1, 1).0, [12, 11, 10, 255]);
        let img = to_image(&data, 2, 2, 12, channel_order(F::xRGB).unwrap()).unwrap();
        assert_eq!(img.get_pixel(1, 0).0, [5, 6, 0, 255]);
        assert!(to_image(&data[..19], 2, 2, 12, [0, 1, 2]).unwrap_err().contains("truncated"));
        assert_eq!(channel_order(F::I420), None);
        assert!(enum_format().is_ok());
        assert_eq!(buffer_params(true).unwrap().len(), 2);
        assert_eq!(buffer_params(false).unwrap().len(), 1);
    }
}
