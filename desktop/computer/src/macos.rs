//! macOS: screenshots with ScreenCaptureKit (SCScreenshotManager, macOS 14
//! and later; CGDisplayCreateImage before), input as Quartz events posted
//! to the HID event tap, the pointer from a new event's location (so the
//! kill switch works). It controls the main display, in its pixels (twice
//! its points on Retina): [quartz::Display] converts.
//!
//! macOS asks the user to allow Screen Recording and Accessibility, for the
//! app that started the helper (the prigh app, or the terminal): until they
//! are given, capture and input fail with where to give them
//! ([quartz::permission_error]).
//!
//! What is sent is decided in [crate::quartz], tested on every platform;
//! this is only the calls.

use crate::action::Button;
use crate::backend::{draw_pointer, Backend, Screen};
use crate::keysym::Keysym;
use crate::quartz::{self, Clicks, Display, TextEvent};
use block2::RcBlock;
use image::RgbaImage;
use objc2::msg_send;
use objc2::rc::{autoreleasepool, Allocated, Retained};
use objc2::runtime::{AnyClass, AnyObject};
use std::ffi::{c_char, c_void, CStr};
use std::sync::mpsc;
use std::time::{Duration, Instant};

type CFTypeRef = *const c_void;
type CGEventRef = *mut c_void;
type CGImageRef = *mut c_void;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct CGPoint {
    x: f64,
    y: f64,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct CGSize {
    width: f64,
    height: f64,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct CGRect {
    origin: CGPoint,
    size: CGSize,
}

const HID_SYSTEM_STATE: i32 = 1;
const HID_EVENT_TAP: u32 = 0;
const SCROLL_UNIT_LINE: u32 = 1;
const MOUSE_EVENT_CLICK_STATE: u32 = 1;
const MOUSE_EVENT_BUTTON_NUMBER: u32 = 3;
const IMAGE_ALPHA_PREMULTIPLIED_LAST: u32 = 1;
const BITMAP_BYTE_ORDER_32_BIG: u32 = 4 << 12;

#[link(name = "CoreFoundation", kind = "framework")]
extern "C" {
    fn CFRelease(cf: CFTypeRef);
    fn CFRetain(cf: CFTypeRef) -> CFTypeRef;
    fn CFDictionaryCreate(
        allocator: CFTypeRef,
        keys: *const CFTypeRef,
        values: *const CFTypeRef,
        count: isize,
        key_callbacks: *const c_void,
        value_callbacks: *const c_void,
    ) -> CFTypeRef;
    static kCFTypeDictionaryKeyCallBacks: c_void;
    static kCFTypeDictionaryValueCallBacks: c_void;
    static kCFBooleanTrue: CFTypeRef;
    static kCFBooleanFalse: CFTypeRef;
}

#[link(name = "CoreGraphics", kind = "framework")]
extern "C" {
    fn CGMainDisplayID() -> u32;
    fn CGDisplayBounds(display: u32) -> CGRect;
    fn CGDisplayCopyDisplayMode(display: u32) -> *mut c_void;
    fn CGDisplayModeGetPixelWidth(mode: *mut c_void) -> usize;
    fn CGDisplayModeGetPixelHeight(mode: *mut c_void) -> usize;
    fn CGDisplayModeRelease(mode: *mut c_void);
    fn CGDisplayCreateImage(display: u32) -> CGImageRef;
    fn CGPreflightScreenCaptureAccess() -> bool;
    fn CGRequestScreenCaptureAccess() -> bool;

    fn CGEventSourceCreate(state: i32) -> *mut c_void;
    fn CGEventSourceSetLocalEventsSuppressionInterval(source: *mut c_void, seconds: f64);
    fn CGEventCreate(source: *mut c_void) -> CGEventRef;
    fn CGEventGetLocation(event: CGEventRef) -> CGPoint;
    fn CGEventCreateMouseEvent(source: *mut c_void, kind: u32, at: CGPoint, button: u32) -> CGEventRef;
    fn CGEventCreateKeyboardEvent(source: *mut c_void, key: u16, down: bool) -> CGEventRef;
    fn CGEventCreateScrollWheelEvent2(
        source: *mut c_void,
        units: u32,
        wheels: u32,
        wheel1: i32,
        wheel2: i32,
        wheel3: i32,
    ) -> CGEventRef;
    fn CGEventKeyboardSetUnicodeString(event: CGEventRef, length: usize, string: *const u16);
    fn CGEventSetIntegerValueField(event: CGEventRef, field: u32, value: i64);
    fn CGEventSetFlags(event: CGEventRef, flags: u64);
    fn CGEventPost(tap: u32, event: CGEventRef);

    fn CGImageGetWidth(image: CGImageRef) -> usize;
    fn CGImageGetHeight(image: CGImageRef) -> usize;
    fn CGImageRelease(image: CGImageRef);
    fn CGColorSpaceCreateWithName(name: CFTypeRef) -> *mut c_void;
    fn CGColorSpaceRelease(space: *mut c_void);
    static kCGColorSpaceSRGB: CFTypeRef;
    fn CGBitmapContextCreate(
        data: *mut c_void,
        width: usize,
        height: usize,
        bits_per_component: usize,
        bytes_per_row: usize,
        space: *mut c_void,
        info: u32,
    ) -> *mut c_void;
    fn CGContextDrawImage(context: *mut c_void, rect: CGRect, image: CGImageRef);
    fn CGContextRelease(context: *mut c_void);
}

#[link(name = "ApplicationServices", kind = "framework")]
extern "C" {
    fn AXIsProcessTrustedWithOptions(options: CFTypeRef) -> bool;
    static kAXTrustedCheckOptionPrompt: CFTypeRef;
}

extern "C" {
    fn dlopen(path: *const c_char, mode: i32) -> *mut c_void;
}

/// Whether this process may post events, asking (once: macOS asks only
/// the first time) if `prompt`.
fn accessibility(prompt: bool) -> bool {
    unsafe {
        let key = kAXTrustedCheckOptionPrompt;
        let value = if prompt { kCFBooleanTrue } else { kCFBooleanFalse };
        let options = CFDictionaryCreate(
            std::ptr::null(),
            &key,
            &value,
            1,
            &kCFTypeDictionaryKeyCallBacks as *const c_void,
            &kCFTypeDictionaryValueCallBacks as *const c_void,
        );
        let trusted = AXIsProcessTrustedWithOptions(options);
        if !options.is_null() {
            CFRelease(options);
        }
        trusted
    }
}

fn responsible_app() -> String {
    let var = |k: &str| std::env::var(k).ok();
    quartz::responsible_app(var("__CFBundleIdentifier").as_deref(), var("TERM_PROGRAM").as_deref())
}

/// An object handed from a completion handler's queue to the waiting thread.
struct Sent(*mut c_void);
unsafe impl Send for Sent {}

fn description(error: *mut AnyObject) -> String {
    match unsafe { error.as_ref() } {
        None => "no reason given".into(),
        Some(error) => unsafe {
            let text: Option<Retained<AnyObject>> = msg_send![error, localizedDescription];
            text.map_or_else(
                || "no reason given".into(),
                |t| {
                    let p: *const c_char = msg_send![&*t, UTF8String];
                    if p.is_null() {
                        "no reason given".into()
                    } else {
                        CStr::from_ptr(p).to_string_lossy().into_owned()
                    }
                },
            )
        },
    }
}

fn class(name: &CStr) -> Result<&'static AnyClass, String> {
    AnyClass::get(name).ok_or_else(|| format!("ScreenCaptureKit lacks {}", name.to_string_lossy()))
}

const TIMEOUT: Duration = Duration::from_secs(10);

/// ScreenCaptureKit's screenshot of `display` at `w`×`h` pixels, pointer
/// included; `None` before macOS 14 (no SCScreenshotManager).
fn sck_capture(display: u32, w: usize, h: usize) -> Result<Option<CGImageRef>, String> {
    // Loaded, not linked: the framework is missing before macOS 12.3.
    let loaded =
        unsafe { dlopen(c"/System/Library/Frameworks/ScreenCaptureKit.framework/ScreenCaptureKit".as_ptr(), 1) };
    if loaded.is_null() {
        return Ok(None);
    }
    let Some(manager) = AnyClass::get(c"SCScreenshotManager") else { return Ok(None) };
    autoreleasepool(|_| unsafe {
        let (tx, rx) = mpsc::channel::<Result<Sent, String>>();
        let block: RcBlock<dyn Fn(*mut AnyObject, *mut AnyObject)> =
            RcBlock::new(move |content: *mut AnyObject, error: *mut AnyObject| {
                let r = match Retained::retain(content) {
                    Some(c) => Ok(Sent(Retained::into_raw(c) as *mut c_void)),
                    None => Err(description(error)),
                };
                let _ = tx.send(r);
            });
        let _: () = msg_send![class(c"SCShareableContent")?, getShareableContentWithCompletionHandler: &*block];
        let content = rx
            .recv_timeout(TIMEOUT)
            .map_err(|_| "ScreenCaptureKit did not list the displays in time".to_string())?
            .map_err(|e| format!("ScreenCaptureKit cannot list the displays: {e}"))?;
        let content: Retained<AnyObject> = Retained::from_raw(content.0 as *mut AnyObject).unwrap();
        let displays: Retained<AnyObject> = msg_send![&*content, displays];
        let n: usize = msg_send![&*displays, count];
        let mut found = None;
        for i in 0..n {
            let d: Retained<AnyObject> = msg_send![&*displays, objectAtIndex: i];
            let id: u32 = msg_send![&*d, displayID];
            if id == display {
                found = Some(d);
            }
        }
        let display = found.ok_or("ScreenCaptureKit does not list the main display")?;
        let empty: Retained<AnyObject> = msg_send![class(c"NSArray")?, array];
        let filter: Allocated<AnyObject> = msg_send![class(c"SCContentFilter")?, alloc];
        let filter: Retained<AnyObject> = msg_send![filter, initWithDisplay: &*display, excludingWindows: &*empty];
        let config: Retained<AnyObject> = msg_send![class(c"SCStreamConfiguration")?, new];
        let _: () = msg_send![&*config, setWidth: w];
        let _: () = msg_send![&*config, setHeight: h];
        let _: () = msg_send![&*config, setShowsCursor: true];

        let (tx, rx) = mpsc::channel::<Result<Sent, String>>();
        let block: RcBlock<dyn Fn(*mut c_void, *mut AnyObject)> =
            RcBlock::new(move |image: *mut c_void, error: *mut AnyObject| {
                let r = if image.is_null() {
                    Err(description(error))
                } else {
                    CFRetain(image);
                    Ok(Sent(image))
                };
                let _ = tx.send(r);
            });
        let _: () = msg_send![
            manager,
            captureImageWithFilter: &*filter,
            configuration: &*config,
            completionHandler: &*block
        ];
        let image = rx
            .recv_timeout(TIMEOUT)
            .map_err(|_| "ScreenCaptureKit took no screenshot in time".to_string())?
            .map_err(|e| format!("ScreenCaptureKit took no screenshot: {e}"))?;
        Ok(Some(image.0))
    })
}

/// The image's pixels, drawn into an RGBA bitmap of its size (whatever the
/// image's own layout).
unsafe fn rgba(image: CGImageRef) -> Result<RgbaImage, String> {
    let (w, h) = (CGImageGetWidth(image), CGImageGetHeight(image));
    let mut data = vec![0u8; w * h * 4];
    let space = CGColorSpaceCreateWithName(kCGColorSpaceSRGB);
    let context = CGBitmapContextCreate(
        data.as_mut_ptr() as *mut c_void,
        w,
        h,
        8,
        w * 4,
        space,
        IMAGE_ALPHA_PREMULTIPLIED_LAST | BITMAP_BYTE_ORDER_32_BIG,
    );
    if !space.is_null() {
        CGColorSpaceRelease(space);
    }
    if context.is_null() {
        return Err(format!("cannot make a {w}×{h} bitmap for the screenshot"));
    }
    let rect = CGRect { origin: CGPoint { x: 0.0, y: 0.0 }, size: CGSize { width: w as f64, height: h as f64 } };
    CGContextDrawImage(context, rect, image);
    CGContextRelease(context);
    for px in data.as_chunks_mut::<4>().0 {
        px[3] = 255;
    }
    RgbaImage::from_raw(w as u32, h as u32, data).ok_or_else(|| "the screenshot's bitmap is short".into())
}

pub struct Macos {
    display_id: u32,
    display: Display,
    screen: Screen,
    source: *mut c_void,
    /// Modifiers held, which every event carries.
    held_keys: Vec<Keysym>,
    held_buttons: Vec<Button>,
    clicks: Clicks,
    may_post: bool,
    asked_screen: bool,
}

impl Macos {
    pub fn connect() -> Result<Macos, String> {
        // Also connects to the window server, which ScreenCaptureKit needs
        // before its first call in a process that is not an app.
        let display_id = unsafe { CGMainDisplayID() };
        let display = Self::measure(display_id)?;
        let source = unsafe { CGEventSourceCreate(HID_SYSTEM_STATE) };
        if source.is_null() {
            return Err("macOS made no event source for input".into());
        }
        // Otherwise the user's own mouse and keys are ignored for a moment
        // after each event: the kill switch would not see them.
        unsafe { CGEventSourceSetLocalEventsSuppressionInterval(source, 0.0) };
        let mut missing = Vec::new();
        if !unsafe { CGPreflightScreenCaptureAccess() } {
            missing.push("Screen Recording");
        }
        if !accessibility(false) {
            missing.push("Accessibility");
        }
        let mut detail = Self::version();
        if !missing.is_empty() {
            detail.push_str(&format!(
                "; {} must still allow it (System Settings > Privacy & Security > {})",
                responsible_app(),
                missing.join(", ")
            ));
        }
        Ok(Macos {
            display_id,
            display,
            screen: Screen {
                backend: "macos",
                detail,
                output: Some(format!("display {display_id}")),
                width: display.pixels.0,
                height: display.pixels.1,
                scale: display.scale(),
            },
            source,
            held_keys: Vec::new(),
            held_buttons: Vec::new(),
            clicks: Clicks::default(),
            may_post: false,
            asked_screen: false,
        })
    }

    fn version() -> String {
        autoreleasepool(|_| unsafe {
            let Some(cls) = AnyClass::get(c"NSProcessInfo") else { return "macOS".into() };
            let info: Retained<AnyObject> = msg_send![cls, processInfo];
            let v: Retained<AnyObject> = msg_send![&*info, operatingSystemVersionString];
            let p: *const c_char = msg_send![&*v, UTF8String];
            if p.is_null() {
                "macOS".into()
            } else {
                format!("macOS {}", CStr::from_ptr(p).to_string_lossy())
            }
        })
    }

    fn measure(id: u32) -> Result<Display, String> {
        unsafe {
            let bounds = CGDisplayBounds(id);
            let mode = CGDisplayCopyDisplayMode(id);
            if mode.is_null() {
                return Err(format!("macOS gives no display mode for display {id}"));
            }
            let pixels = (CGDisplayModeGetPixelWidth(mode) as u32, CGDisplayModeGetPixelHeight(mode) as u32);
            CGDisplayModeRelease(mode);
            if pixels.0 == 0 || pixels.1 == 0 {
                return Err(format!("display {id} has no pixels"));
            }
            Ok(Display {
                origin: (bounds.origin.x, bounds.origin.y),
                points: (bounds.size.width, bounds.size.height),
                pixels,
            })
        }
    }

    fn check_input(&mut self) -> Result<(), String> {
        if self.may_post {
            return Ok(());
        }
        if accessibility(false) || accessibility(true) {
            self.may_post = true;
            return Ok(());
        }
        let app = responsible_app();
        Err(quartz::permission_error("control the mouse and keyboard", "Accessibility", &app, "try again"))
    }

    fn check_screen(&mut self) -> Result<(), String> {
        if unsafe { CGPreflightScreenCaptureAccess() } {
            return Ok(());
        }
        if !self.asked_screen {
            self.asked_screen = true;
            if unsafe { CGRequestScreenCaptureAccess() } {
                return Ok(());
            }
        }
        let app = responsible_app();
        let after = format!("quit and reopen {app} (macOS applies it only to apps started after)");
        Err(quartz::permission_error("record the screen", "Screen Recording", &app, &after))
    }

    fn flags(&self) -> u64 {
        quartz::flags(&self.held_keys)
    }

    fn post(&self, event: CGEventRef, flags: u64) -> Result<(), String> {
        if event.is_null() {
            return Err("macOS made no event".into());
        }
        unsafe {
            CGEventSetFlags(event, flags);
            CGEventPost(HID_EVENT_TAP, event);
            CFRelease(event);
        }
        Ok(())
    }

    fn pointer_points(&self) -> CGPoint {
        unsafe {
            let e = CGEventCreate(std::ptr::null_mut());
            if e.is_null() {
                return CGPoint { x: 0.0, y: 0.0 };
            }
            let p = CGEventGetLocation(e);
            CFRelease(e);
            p
        }
    }

    fn mouse(&self, kind: u32, at: CGPoint, button: Button, clicks: Option<i64>) -> Result<(), String> {
        let event = unsafe { CGEventCreateMouseEvent(self.source, kind, at, quartz::button_number(button)) };
        if !event.is_null() {
            unsafe {
                if let Some(n) = clicks {
                    CGEventSetIntegerValueField(event, MOUSE_EVENT_CLICK_STATE, n);
                }
                CGEventSetIntegerValueField(event, MOUSE_EVENT_BUTTON_NUMBER, quartz::button_number(button) as i64);
            }
        }
        self.post(event, self.flags())
    }

    fn key(&self, code: u16, down: bool, text: Option<&[u16]>, flags: u64) -> Result<(), String> {
        let event = unsafe { CGEventCreateKeyboardEvent(self.source, code, down) };
        if let (false, Some(units)) = (event.is_null(), text) {
            unsafe { CGEventKeyboardSetUnicodeString(event, units.len(), units.as_ptr()) };
        }
        self.post(event, flags)
    }
}

impl Drop for Macos {
    fn drop(&mut self) {
        unsafe { CFRelease(self.source) };
    }
}

impl Backend for Macos {
    fn screen(&self) -> Screen {
        self.screen.clone()
    }

    fn capture(&mut self) -> Result<RgbaImage, String> {
        self.check_screen()?;
        let (w, h) = (self.display.pixels.0 as usize, self.display.pixels.1 as usize);
        let (image, has_pointer) = match sck_capture(self.display_id, w, h)? {
            Some(image) => (image, true),
            None => (unsafe { CGDisplayCreateImage(self.display_id) }, false),
        };
        if image.is_null() {
            return Err("macOS gave no screenshot of the display".into());
        }
        let img = unsafe { rgba(image) };
        unsafe { CGImageRelease(image) };
        let mut img = img?;
        if (img.width(), img.height()) != self.display.pixels {
            img =
                image::imageops::resize(&img, self.display.pixels.0, self.display.pixels.1, image::imageops::Triangle);
        }
        if !has_pointer {
            if let Some((x, y)) = self.cursor()? {
                draw_pointer(&mut img, x, y);
            }
        }
        Ok(img)
    }

    fn move_to(&mut self, x: i32, y: i32) -> Result<(), String> {
        self.check_input()?;
        let (px, py) = self.display.to_points(x, y);
        let button = self.held_buttons.first().copied().unwrap_or(Button::Left);
        self.mouse(quartz::move_event(&self.held_buttons), CGPoint { x: px, y: py }, button, None)
    }

    fn button(&mut self, button: Button, down: bool) -> Result<(), String> {
        self.check_input()?;
        let at = self.pointer_points();
        let now = Instant::now();
        let count = if down {
            self.clicks.press(button, self.display.to_pixels(at.x, at.y), now)
        } else {
            self.clicks.release(button, now)
        };
        self.held_buttons.retain(|b| *b != button);
        if down {
            self.held_buttons.push(button);
        }
        self.mouse(quartz::button_event(button, down), at, button, Some(count))
    }

    fn scroll(&mut self, dx: i32, dy: i32) -> Result<(), String> {
        self.check_input()?;
        for (vertical, horizontal) in quartz::scroll_events(dx, dy) {
            let event =
                unsafe { CGEventCreateScrollWheelEvent2(self.source, SCROLL_UNIT_LINE, 2, vertical, horizontal, 0) };
            self.post(event, self.flags())?;
        }
        Ok(())
    }

    fn keys(&mut self, keys: &[Keysym], down: bool) -> Result<(), String> {
        self.check_input()?;
        for &k in keys {
            if quartz::modifier_flag(k) != 0 {
                self.held_keys.retain(|h| *h != k);
                if down {
                    self.held_keys.push(k);
                }
            }
            match quartz::combo_key(k) {
                Ok(key) => {
                    let flags = self.flags() | if key.shift { quartz::flag::SHIFT } else { 0 };
                    self.key(key.code, down, None, flags)?;
                }
                Err(Some(c)) => {
                    let mut units = [0; 2];
                    let units = c.encode_utf16(&mut units);
                    self.key(0, down, Some(units), self.flags())?;
                }
                Err(None) => return Err(format!("{} has no key on a Mac keyboard", crate::keysym::xkb_name(k))),
            }
        }
        Ok(())
    }

    fn type_text(&mut self, text: &str) -> Result<(), String> {
        self.check_input()?;
        let flags = self.flags();
        for e in quartz::text_events(text) {
            match e {
                TextEvent::Key(code) => {
                    self.key(code, true, None, flags)?;
                    self.key(code, false, None, flags)?;
                }
                TextEvent::Unicode(units) => {
                    self.key(0, true, Some(&units), flags)?;
                    self.key(0, false, Some(&units), flags)?;
                }
            }
        }
        Ok(())
    }

    fn cursor(&mut self) -> Result<Option<(i32, i32)>, String> {
        let p = self.pointer_points();
        Ok(Some(self.display.to_pixels(p.x, p.y)))
    }

    fn sync(&mut self) -> Result<(), String> {
        // Posted events reach the apps a moment later.
        std::thread::sleep(Duration::from_millis(10));
        Ok(())
    }
}
