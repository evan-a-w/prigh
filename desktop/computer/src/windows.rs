//! Windows: the primary monitor, captured with DXGI Desktop Duplication
//! (GDI's BitBlt where duplication is unavailable: RDP sessions, some
//! virtual GPUs, rotated screens), input with SendInput, the pointer with
//! GetCursorPos (so the kill switch works). The process is per-monitor DPI
//! aware (v2), so every coordinate is in physical pixels.
//!
//! What Windows does not allow, and the errors say so: the secure desktop
//! (UAC prompts, the lock screen) can be neither seen nor driven, and UIPI
//! drops input sent to windows of an elevated process unless the helper is
//! elevated too ([crate::win_input::uipi_blocked]).
//!
//! The pure parts (keys, text, coordinates, pixels) are in
//! [crate::win_input], tested on every platform.

use crate::action::Button;
use crate::backend::{draw_pointer, Backend, Screen};
use crate::keysym::{xkb_name, Keysym};
use crate::win_input::{self, Key, TextEvent, VirtualDesk};
use image::RgbaImage;
use std::time::{Duration, Instant};
use windows::core::Interface;
use windows::Win32::Foundation::{CloseHandle, HANDLE, HMODULE, HWND, POINT, RECT};
use windows::Win32::Graphics::Direct3D::D3D_DRIVER_TYPE_UNKNOWN;
use windows::Win32::Graphics::Direct3D11::{
    D3D11CreateDevice, ID3D11Device, ID3D11DeviceContext, ID3D11Texture2D, D3D11_CPU_ACCESS_READ,
    D3D11_CREATE_DEVICE_BGRA_SUPPORT, D3D11_MAPPED_SUBRESOURCE, D3D11_MAP_READ, D3D11_SDK_VERSION,
    D3D11_TEXTURE2D_DESC, D3D11_USAGE_STAGING,
};
use windows::Win32::Graphics::Dxgi::Common::{
    DXGI_FORMAT_B8G8R8A8_UNORM, DXGI_MODE_ROTATION_IDENTITY, DXGI_MODE_ROTATION_UNSPECIFIED, DXGI_SAMPLE_DESC,
};
use windows::Win32::Graphics::Dxgi::{
    CreateDXGIFactory1, IDXGIFactory1, IDXGIOutput1, IDXGIOutputDuplication, IDXGIResource, DXGI_ERROR_WAIT_TIMEOUT,
    DXGI_OUTDUPL_FRAME_INFO,
};
use windows::Win32::Graphics::Gdi::{
    BitBlt, CreateCompatibleBitmap, CreateCompatibleDC, DeleteDC, DeleteObject, GetDC, GetDIBits, GetMonitorInfoW,
    MonitorFromPoint, ReleaseDC, SelectObject, BITMAPINFO, BITMAPINFOHEADER, BI_RGB, CAPTUREBLT, DIB_RGB_COLORS,
    HMONITOR, MONITORINFO, MONITORINFOEXW, MONITOR_DEFAULTTOPRIMARY, SRCCOPY,
};
use windows::Win32::Security::{
    GetSidSubAuthority, GetSidSubAuthorityCount, GetTokenInformation, TokenIntegrityLevel, TOKEN_MANDATORY_LABEL,
    TOKEN_QUERY,
};
use windows::Win32::System::StationsAndDesktops::{
    CloseDesktop, GetUserObjectInformationW, OpenInputDesktop, DESKTOP_CONTROL_FLAGS, DESKTOP_READOBJECTS, UOI_NAME,
};
use windows::Win32::System::Threading::{
    GetCurrentProcess, OpenProcess, OpenProcessToken, QueryFullProcessImageNameW, PROCESS_NAME_WIN32,
    PROCESS_QUERY_LIMITED_INFORMATION,
};
use windows::Win32::UI::HiDpi::{
    GetDpiForMonitor, SetProcessDpiAwarenessContext, DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2, MDT_EFFECTIVE_DPI,
};
use windows::Win32::UI::Input::KeyboardAndMouse::{
    MapVirtualKeyW, SendInput, VkKeyScanW, INPUT, INPUT_0, INPUT_KEYBOARD, INPUT_MOUSE, KEYBDINPUT, KEYBD_EVENT_FLAGS,
    KEYEVENTF_EXTENDEDKEY, KEYEVENTF_KEYUP, KEYEVENTF_UNICODE, MAPVK_VK_TO_VSC, MOUSEEVENTF_ABSOLUTE, MOUSEEVENTF_MOVE,
    MOUSEEVENTF_VIRTUALDESK, MOUSEINPUT, MOUSE_EVENT_FLAGS, VIRTUAL_KEY,
};
use windows::Win32::UI::WindowsAndMessaging::{
    GetAncestor, GetCursorPos, GetForegroundWindow, GetSystemMetrics, GetWindowTextW, GetWindowThreadProcessId,
    WindowFromPoint, GA_ROOT, SM_CXVIRTUALSCREEN, SM_CYVIRTUALSCREEN, SM_XVIRTUALSCREEN, SM_YVIRTUALSCREEN,
};

/// How long after DXGI failed to try it again (it fails for good in an RDP
/// session, but also for a moment around a mode change).
const DXGI_RETRY: Duration = Duration::from_secs(10);

/// One output's desktop duplication, and the last frame (DXGI hands out a
/// frame only when something changed).
struct Duplication {
    context: ID3D11DeviceContext,
    duplication: IDXGIOutputDuplication,
    staging: ID3D11Texture2D,
    have_frame: bool,
}

impl Duplication {
    fn open(monitor: HMONITOR, width: u32, height: u32) -> Result<Duplication, String> {
        let e = |what: &'static str| move |e: windows::core::Error| format!("DXGI {what}: {e}");
        unsafe {
            let factory: IDXGIFactory1 = CreateDXGIFactory1().map_err(e("factory"))?;
            let mut a = 0;
            while let Ok(adapter) = factory.EnumAdapters1(a) {
                a += 1;
                let mut o = 0;
                while let Ok(output) = adapter.EnumOutputs(o) {
                    o += 1;
                    let desc = output.GetDesc().map_err(e("output"))?;
                    if desc.Monitor != monitor {
                        continue;
                    }
                    if desc.Rotation != DXGI_MODE_ROTATION_IDENTITY && desc.Rotation != DXGI_MODE_ROTATION_UNSPECIFIED {
                        return Err("the screen is rotated".into());
                    }
                    let output: IDXGIOutput1 = output.cast().map_err(e("IDXGIOutput1"))?;
                    let mut device: Option<ID3D11Device> = None;
                    let mut context: Option<ID3D11DeviceContext> = None;
                    D3D11CreateDevice(
                        &adapter,
                        D3D_DRIVER_TYPE_UNKNOWN,
                        HMODULE::default(),
                        D3D11_CREATE_DEVICE_BGRA_SUPPORT,
                        None,
                        D3D11_SDK_VERSION,
                        Some(&mut device),
                        None,
                        Some(&mut context),
                    )
                    .map_err(e("Direct3D device"))?;
                    let (device, context) = (device.ok_or("no Direct3D device")?, context.ok_or("no context")?);
                    let duplication = output.DuplicateOutput(&device).map_err(e("DuplicateOutput"))?;
                    let mode = duplication.GetDesc().ModeDesc;
                    if (mode.Width, mode.Height) != (width, height) {
                        return Err(format!(
                            "DXGI gives {}×{} for a {width}×{height} monitor",
                            mode.Width, mode.Height
                        ));
                    }
                    let desc = D3D11_TEXTURE2D_DESC {
                        Width: width,
                        Height: height,
                        MipLevels: 1,
                        ArraySize: 1,
                        Format: DXGI_FORMAT_B8G8R8A8_UNORM,
                        SampleDesc: DXGI_SAMPLE_DESC { Count: 1, Quality: 0 },
                        Usage: D3D11_USAGE_STAGING,
                        BindFlags: 0,
                        CPUAccessFlags: D3D11_CPU_ACCESS_READ.0 as u32,
                        MiscFlags: 0,
                    };
                    let mut staging = None;
                    device.CreateTexture2D(&desc, None, Some(&mut staging)).map_err(e("staging texture"))?;
                    return Ok(Duplication {
                        context,
                        duplication,
                        staging: staging.ok_or("no staging texture")?,
                        have_frame: false,
                    });
                }
            }
        }
        Err("no DXGI output shows the primary monitor".into())
    }

    fn capture(&mut self, width: u32, height: u32) -> Result<RgbaImage, String> {
        unsafe {
            // the first frame is the whole desktop and comes at once; after
            // it a timeout means nothing changed
            let timeout = if self.have_frame { 30 } else { 1000 };
            let mut info = DXGI_OUTDUPL_FRAME_INFO::default();
            let mut resource: Option<IDXGIResource> = None;
            match self.duplication.AcquireNextFrame(timeout, &mut info, &mut resource) {
                Ok(()) => {
                    let copied = resource
                        .ok_or_else(|| "DXGI gave no frame".to_string())
                        .and_then(|r| r.cast::<ID3D11Texture2D>().map_err(|e| e.to_string()))
                        .map(|t| self.context.CopyResource(&self.staging, &t));
                    let _ = self.duplication.ReleaseFrame();
                    copied.map_err(|e| format!("DXGI frame: {e}"))?;
                    self.have_frame = true;
                }
                Err(e) if e.code() == DXGI_ERROR_WAIT_TIMEOUT && self.have_frame => {}
                // DXGI_ERROR_ACCESS_LOST (a desktop switch, a mode change)
                // and the rest: the caller opens it again or uses GDI
                Err(e) => return Err(format!("DXGI AcquireNextFrame: {e}")),
            }
            let mut mapped = D3D11_MAPPED_SUBRESOURCE::default();
            self.context
                .Map(&self.staging, 0, D3D11_MAP_READ, 0, Some(&mut mapped))
                .map_err(|e| format!("DXGI Map: {e}"))?;
            let stride = mapped.RowPitch as usize;
            let bytes = std::slice::from_raw_parts(mapped.pData as *const u8, stride * height as usize);
            let img = win_input::bgra_to_rgba(bytes, width, height, stride);
            self.context.Unmap(&self.staging, 0);
            img
        }
    }
}

/// The screen with GDI: slower, but works wherever there is a desktop.
fn gdi_capture(x: i32, y: i32, width: u32, height: u32) -> Result<RgbaImage, String> {
    unsafe {
        let screen = GetDC(None);
        if screen.is_invalid() {
            return Err("GDI: GetDC failed".into());
        }
        let mem = CreateCompatibleDC(Some(screen));
        let bitmap = CreateCompatibleBitmap(screen, width as i32, height as i32);
        let old = SelectObject(mem, bitmap.into());
        let result = BitBlt(mem, 0, 0, width as i32, height as i32, Some(screen), x, y, SRCCOPY | CAPTUREBLT)
            .map_err(|e| format!("GDI BitBlt: {e}"))
            .and_then(|()| {
                let mut info = BITMAPINFO {
                    bmiHeader: BITMAPINFOHEADER {
                        biSize: std::mem::size_of::<BITMAPINFOHEADER>() as u32,
                        biWidth: width as i32,
                        // negative: rows top-down
                        biHeight: -(height as i32),
                        biPlanes: 1,
                        biBitCount: 32,
                        biCompression: BI_RGB.0,
                        ..Default::default()
                    },
                    ..Default::default()
                };
                let mut buf = vec![0u8; width as usize * height as usize * 4];
                SelectObject(mem, old);
                let lines = GetDIBits(mem, bitmap, 0, height, Some(buf.as_mut_ptr().cast()), &mut info, DIB_RGB_COLORS);
                if lines != height as i32 {
                    return Err(format!("GDI GetDIBits copied {lines} of {height} rows"));
                }
                win_input::bgra_to_rgba(&buf, width, height, width as usize * 4)
            });
        let _ = DeleteObject(bitmap.into());
        let _ = DeleteDC(mem);
        ReleaseDC(None, screen);
        result
    }
}

fn wide_to_string(w: &[u16]) -> String {
    let end = w.iter().position(|&c| c == 0).unwrap_or(w.len());
    String::from_utf16_lossy(&w[..end])
}

/// The input desktop's name ("Default", or "Winlogon" for the secure
/// desktop); an error when it cannot even be opened, which is the secure
/// desktop too.
fn input_desktop() -> Result<(), String> {
    unsafe {
        let desk = OpenInputDesktop(DESKTOP_CONTROL_FLAGS(0), false, DESKTOP_READOBJECTS)
            .map_err(|_| win_input::SECURE_DESKTOP.to_string())?;
        let mut name = [0u16; 64];
        let mut needed = 0;
        let named = GetUserObjectInformationW(
            HANDLE(desk.0),
            UOI_NAME,
            Some(name.as_mut_ptr().cast()),
            (name.len() * 2) as u32,
            Some(&mut needed),
        );
        let _ = CloseDesktop(desk);
        match named {
            Ok(()) if !wide_to_string(&name).eq_ignore_ascii_case("default") => {
                Err(win_input::SECURE_DESKTOP.to_string())
            }
            _ => Ok(()),
        }
    }
}

/// A token's integrity level (the RID: 0x2000 medium, 0x3000 high), what
/// UIPI compares.
fn integrity(process: HANDLE) -> Result<u32, String> {
    unsafe {
        let mut token = HANDLE::default();
        OpenProcessToken(process, TOKEN_QUERY, &mut token).map_err(|e| e.to_string())?;
        let mut buf = [0u8; 128];
        let mut len = 0;
        let got =
            GetTokenInformation(token, TokenIntegrityLevel, Some(buf.as_mut_ptr().cast()), buf.len() as u32, &mut len);
        let _ = CloseHandle(token);
        got.map_err(|e| e.to_string())?;
        let label = &*(buf.as_ptr() as *const TOKEN_MANDATORY_LABEL);
        let sid = label.Label.Sid;
        let count = *GetSidSubAuthorityCount(sid) as u32;
        Ok(*GetSidSubAuthority(sid, count - 1))
    }
}

/// "notepad.exe" ("Untitled - Notepad") for errors.
fn describe(hwnd: HWND, process: Option<HANDLE>) -> String {
    unsafe {
        let mut title = [0u16; 256];
        let n = GetWindowTextW(hwnd, &mut title).max(0) as usize;
        let title = String::from_utf16_lossy(&title[..n]);
        let exe = process.and_then(|p| {
            let mut buf = [0u16; 512];
            let mut len = buf.len() as u32;
            QueryFullProcessImageNameW(p, PROCESS_NAME_WIN32, windows::core::PWSTR(buf.as_mut_ptr()), &mut len)
                .ok()
                .map(|()| String::from_utf16_lossy(&buf[..len as usize]))
                .map(|path| path.rsplit('\\').next().unwrap_or(&path).to_string())
        });
        match (exe, title.is_empty()) {
            (Some(exe), false) => format!("{exe} ({title:?})"),
            (Some(exe), true) => exe,
            (None, false) => format!("the window {title:?}"),
            (None, true) => "the window there".into(),
        }
    }
}

pub struct Windows {
    monitor: HMONITOR,
    /// The monitor's place on the virtual desktop.
    origin: (i32, i32),
    screen: Screen,
    duplication: Option<Duplication>,
    dxgi_failed: Option<(Instant, String)>,
    /// The helper's own integrity level, to tell what UIPI blocks.
    integrity: u32,
}

impl Windows {
    pub fn connect() -> Result<Windows, String> {
        unsafe {
            // fails when already set (a manifest, an earlier call): fine
            let _ = SetProcessDpiAwarenessContext(DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2);
            input_desktop()?;
            let monitor = MonitorFromPoint(POINT { x: 0, y: 0 }, MONITOR_DEFAULTTOPRIMARY);
            let mut info = MONITORINFOEXW::default();
            info.monitorInfo.cbSize = std::mem::size_of::<MONITORINFOEXW>() as u32;
            if !GetMonitorInfoW(monitor, &mut info as *mut MONITORINFOEXW as *mut MONITORINFO).as_bool() {
                return Err("cannot read the primary monitor's size (GetMonitorInfo failed)".into());
            }
            let RECT { left, top, right, bottom } = info.monitorInfo.rcMonitor;
            let (width, height) = ((right - left) as u32, (bottom - top) as u32);
            let (mut dpi_x, mut dpi_y) = (96, 96);
            let _ = GetDpiForMonitor(monitor, MDT_EFFECTIVE_DPI, &mut dpi_x, &mut dpi_y);
            let mut w = Windows {
                monitor,
                origin: (left, top),
                screen: Screen {
                    backend: "windows",
                    detail: String::new(),
                    output: Some(wide_to_string(&info.szDevice).trim_start_matches(r"\\.\").to_string()),
                    width,
                    height,
                    scale: dpi_x as f64 / 96.0,
                },
                duplication: None,
                dxgi_failed: None,
                integrity: integrity(GetCurrentProcess()).unwrap_or(0x2000),
            };
            w.open_duplication();
            w.screen.detail = match &w.dxgi_failed {
                None => "DXGI desktop duplication".into(),
                Some((_, e)) => format!("GDI (DXGI: {e})"),
            };
            Ok(w)
        }
    }

    fn open_duplication(&mut self) {
        match Duplication::open(self.monitor, self.screen.width, self.screen.height) {
            Ok(d) => {
                self.duplication = Some(d);
                self.dxgi_failed = None;
            }
            Err(e) => self.dxgi_failed = Some((Instant::now(), e)),
        }
    }

    fn send(&self, inputs: &[INPUT]) -> Result<(), String> {
        if inputs.is_empty() {
            return Ok(());
        }
        let sent = unsafe { SendInput(inputs, std::mem::size_of::<INPUT>() as i32) };
        if sent as usize != inputs.len() {
            input_desktop()?;
            return Err(format!(
                "SendInput took {sent} of {} events: {}",
                inputs.len(),
                windows::core::Error::from_thread()
            ));
        }
        Ok(())
    }

    /// Fails when UIPI would drop input to `hwnd`'s window.
    fn check_uipi(&self, hwnd: HWND, what: &str) -> Result<(), String> {
        if hwnd.is_invalid() {
            return Ok(());
        }
        unsafe {
            let hwnd = GetAncestor(hwnd, GA_ROOT);
            let mut pid = 0;
            GetWindowThreadProcessId(hwnd, Some(&mut pid));
            let process = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, false, pid).ok();
            // a token we may not even read is more privileged than ours
            let theirs = process.map_or(Err(String::new()), integrity);
            let blocked = match theirs {
                Ok(level) => level > self.integrity,
                Err(_) => self.integrity < 0x3000,
            };
            let result = if blocked { Err(win_input::uipi_blocked(what, &describe(hwnd, process))) } else { Ok(()) };
            if let Some(p) = process {
                let _ = CloseHandle(p);
            }
            result
        }
    }

    fn at_cursor(&self) -> HWND {
        unsafe {
            let mut p = POINT::default();
            if GetCursorPos(&mut p).is_err() {
                return HWND::default();
            }
            WindowFromPoint(p)
        }
    }

    fn key_inputs(&self, k: Keysym, up: bool) -> Result<Vec<INPUT>, String> {
        let key = win_input::key_of_keysym(k)
            .ok_or_else(|| format!("{} has no key on Windows (F25 and above, ...)", xkb_name(k)))?;
        let mut keys: Vec<(u16, bool)> = Vec::new();
        match key {
            Key::Vk { vk, extended } => keys.push((vk, extended)),
            Key::Char(c) => {
                let mut buf = [0u16; 2];
                let units = c.encode_utf16(&mut buf);
                let scan = if units.len() == 1 { unsafe { VkKeyScanW(units[0]) } } else { -1 };
                if scan == -1 {
                    // not on this layout: as the character itself
                    return Ok(units
                        .iter()
                        .map(|&u| {
                            keyboard(0, u, KEYEVENTF_UNICODE | if up { KEYEVENTF_KEYUP } else { Default::default() })
                        })
                        .collect());
                }
                keys.extend(win_input::scan_modifiers((scan >> 8) as u8).into_iter().map(|m| (m, false)));
                keys.push(((scan & 0xff) as u16, false));
            }
        }
        if up {
            keys.reverse();
        }
        Ok(keys
            .into_iter()
            .map(|(vk, extended)| {
                let mut flags = KEYBD_EVENT_FLAGS::default();
                if extended {
                    flags |= KEYEVENTF_EXTENDEDKEY;
                }
                if up {
                    flags |= KEYEVENTF_KEYUP;
                }
                keyboard(vk, unsafe { MapVirtualKeyW(vk as u32, MAPVK_VK_TO_VSC) } as u16, flags)
            })
            .collect())
    }
}

fn keyboard(vk: u16, scan: u16, flags: KEYBD_EVENT_FLAGS) -> INPUT {
    INPUT {
        r#type: INPUT_KEYBOARD,
        Anonymous: INPUT_0 {
            ki: KEYBDINPUT { wVk: VIRTUAL_KEY(vk), wScan: scan, dwFlags: flags, time: 0, dwExtraInfo: 0 },
        },
    }
}

fn mouse(dx: i32, dy: i32, data: i32, flags: u32) -> INPUT {
    INPUT {
        r#type: INPUT_MOUSE,
        Anonymous: INPUT_0 {
            mi: MOUSEINPUT {
                dx,
                dy,
                mouseData: data as u32,
                dwFlags: MOUSE_EVENT_FLAGS(flags),
                time: 0,
                dwExtraInfo: 0,
            },
        },
    }
}

impl Backend for Windows {
    fn screen(&self) -> Screen {
        self.screen.clone()
    }

    fn capture(&mut self) -> Result<RgbaImage, String> {
        input_desktop()?;
        let (w, h) = (self.screen.width, self.screen.height);
        if self.duplication.is_none() && self.dxgi_failed.as_ref().is_none_or(|(at, _)| at.elapsed() > DXGI_RETRY) {
            self.open_duplication();
        }
        let mut img = None;
        if let Some(d) = &mut self.duplication {
            match d.capture(w, h) {
                Ok(i) => img = Some(i),
                Err(e) => {
                    self.duplication = None;
                    self.dxgi_failed = Some((Instant::now(), e));
                }
            }
        }
        let mut img = match img {
            Some(i) => i,
            None => gdi_capture(self.origin.0, self.origin.1, w, h)?,
        };
        if let Some((x, y)) = self.cursor()? {
            draw_pointer(&mut img, x, y);
        }
        Ok(img)
    }

    fn move_to(&mut self, x: i32, y: i32) -> Result<(), String> {
        let desk = unsafe {
            VirtualDesk {
                x: GetSystemMetrics(SM_XVIRTUALSCREEN),
                y: GetSystemMetrics(SM_YVIRTUALSCREEN),
                width: GetSystemMetrics(SM_CXVIRTUALSCREEN),
                height: GetSystemMetrics(SM_CYVIRTUALSCREEN),
            }
        };
        let (nx, ny) = desk.normalize(x + self.origin.0, y + self.origin.1);
        self.send(&[mouse(nx, ny, 0, MOUSEEVENTF_MOVE.0 | MOUSEEVENTF_ABSOLUTE.0 | MOUSEEVENTF_VIRTUALDESK.0)])
    }

    fn button(&mut self, button: Button, down: bool) -> Result<(), String> {
        self.check_uipi(self.at_cursor(), "clicks")?;
        let (flags, data) = win_input::button_flags(button, down);
        self.send(&[mouse(0, 0, data as i32, flags)])
    }

    fn scroll(&mut self, dx: i32, dy: i32) -> Result<(), String> {
        self.check_uipi(self.at_cursor(), "scrolling")?;
        let inputs: Vec<INPUT> = win_input::wheel(dx, dy).into_iter().map(|(f, d)| mouse(0, 0, d, f)).collect();
        self.send(&inputs)
    }

    fn keys(&mut self, keys: &[Keysym], down: bool) -> Result<(), String> {
        if down {
            self.check_uipi(unsafe { GetForegroundWindow() }, "keys")?;
        }
        let mut inputs = Vec::new();
        for &k in keys {
            inputs.extend(self.key_inputs(k, !down)?);
        }
        self.send(&inputs)
    }

    fn type_text(&mut self, text: &str) -> Result<(), String> {
        self.check_uipi(unsafe { GetForegroundWindow() }, "typing")?;
        let inputs: Vec<INPUT> = win_input::text_events(text)
            .into_iter()
            .map(|e| match e {
                TextEvent::Vk { vk, up } => keyboard(
                    vk,
                    unsafe { MapVirtualKeyW(vk as u32, MAPVK_VK_TO_VSC) } as u16,
                    if up { KEYEVENTF_KEYUP } else { Default::default() },
                ),
                TextEvent::Unicode { unit, up } => {
                    keyboard(0, unit, KEYEVENTF_UNICODE | if up { KEYEVENTF_KEYUP } else { Default::default() })
                }
            })
            .collect();
        self.send(&inputs)
    }

    fn cursor(&mut self) -> Result<Option<(i32, i32)>, String> {
        let mut p = POINT::default();
        unsafe { GetCursorPos(&mut p) }
            .map_err(|e| input_desktop().err().unwrap_or_else(|| format!("GetCursorPos: {e}")))?;
        Ok(Some((p.x - self.origin.0, p.y - self.origin.1)))
    }
}
