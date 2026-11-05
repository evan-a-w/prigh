// prigh's desktop app: a native window (the system webview: WebKitGTK,
// WKWebView, WebView2) on prigh-web, the backend's own or served by `prigh
// desktop`, the sidecar this process starts and stops. See "Desktop app" in
// ARCHITECTURE.md.
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use std::io::{BufRead, BufReader, Read};
use std::path::PathBuf;
use std::process::{ChildStdin, Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

use prigh_desktop_core::failure::{self, Failure};
use prigh_desktop_core::launch::{self, Os};
use prigh_desktop_core::navigation::{self, Decision};
use prigh_desktop_core::tail::Tail;
use prigh_desktop_core::zoom::{self, Step};
use prigh_desktop_core::{ready, search_path};
use shared_child::SharedChild;
use tauri::webview::{Color, DownloadEvent, NewWindowResponse, PageLoadEvent, PermissionKind, PermissionResponse};
use tauri::{AppHandle, Manager, RunEvent, Url, WebviewUrl, WebviewWindowBuilder};
use tauri_plugin_dialog::{DialogExt, MessageDialogKind};
use tauri_plugin_opener::OpenerExt;

const OS: Os = if cfg!(windows) { Os::Windows } else { Os::Unix };
const WINDOW: &str = "main";
/// How long the sidecar has to exit after its stdin closes before it is
/// killed.
const STOP_GRACE: Duration = Duration::from_secs(5);
#[cfg(unix)]
const TERM_GRACE: Duration = Duration::from_secs(2);
const TEST_CLOSE_URL: &str = "about:blank#prigh-desktop-test-close";
/// WebKitGTK's page on its graphics: whether it renders with the GPU.
const GPU_INFO_URL: &str = "webkit://gpu";

fn log(msg: impl AsRef<str>) {
    for line in msg.as_ref().lines() {
        eprintln!("prigh-desktop: {line}");
    }
}

fn env_flag(name: &str) -> bool {
    std::env::var(name).is_ok_and(|v| !v.is_empty() && v != "0")
}

#[derive(Default)]
struct Sidecar {
    /// The origins of the sites its page lines said to show: its own, then
    /// the backend's under `-ui remote`.
    origins: Mutex<Vec<String>>,
    child: Mutex<Option<Arc<SharedChild>>>,
    stdin: Mutex<Option<ChildStdin>>,
    /// We asked it to stop (the window closed): its exit is not a failure.
    stopping: AtomicBool,
    /// A failure is being reported, which ends the app: its exit says
    /// nothing more.
    failing: AtomicBool,
    exited: AtomicBool,
}

impl Sidecar {
    fn origins(&self) -> Vec<String> {
        self.origins.lock().unwrap().clone()
    }

    fn is_ours(&self, url: &str) -> bool {
        navigation::origin(url).is_some_and(|o| self.origins.lock().unwrap().contains(&o))
    }

    /// Closes its stdin, which tells it to stop. If it has not within
    /// [STOP_GRACE]: SIGTERM (Unix; it stops its tools' process groups on
    /// that too), and SIGKILL after [TERM_GRACE] more.
    fn stop(self: &Arc<Self>) {
        if self.stopping.swap(true, Ordering::SeqCst) {
            return;
        }
        drop(self.stdin.lock().unwrap().take());
        let Some(child) = self.child.lock().unwrap().clone() else { return };
        let me = self.clone();
        thread::spawn(move || {
            if !matches!(child.wait_timeout(STOP_GRACE), Ok(None)) || me.exited.load(Ordering::SeqCst) {
                return;
            }
            #[cfg(unix)]
            {
                use shared_child::unix::SharedChildExt;
                log("prigh desktop did not stop when asked: terminating it");
                let _ = child.send_signal(libc::SIGTERM);
                if !matches!(child.wait_timeout(TERM_GRACE), Ok(None)) || me.exited.load(Ordering::SeqCst) {
                    return;
                }
            }
            log("prigh desktop did not stop: killing it");
            let _ = child.kill();
        });
    }
}

/// The PATH for the sidecar on Linux and macOS: see [search_path].
fn sidecar_path() -> String {
    let shell = std::env::var("SHELL").ok().filter(|s| !s.is_empty()).unwrap_or_else(|| "/bin/sh".into());
    let login = Command::new(&shell)
        .args(["-l", "-c", "env"])
        .stdin(Stdio::null())
        .stderr(Stdio::null())
        .stdout(Stdio::piped())
        .spawn()
        .ok()
        .and_then(|child| {
            let child = SharedChild::new(child).ok()?;
            let mut out = String::new();
            let stdout = child.take_stdout();
            let reader = thread::spawn(move || {
                if let Some(mut s) = stdout {
                    let _ = s.read_to_string(&mut out);
                }
                out
            });
            match child.wait_timeout(Duration::from_secs(5)) {
                Ok(Some(_)) => reader.join().ok(),
                _ => {
                    log(format!("{shell} -l -c env did not finish: using the inherited PATH"));
                    let _ = child.kill();
                    None
                }
            }
        })
        .and_then(|out| search_path::from_env_output(&out));
    search_path::compose(
        login.as_deref(),
        std::env::var("PATH").ok().as_deref(),
        std::env::var("HOME").ok().as_deref(),
        std::env::var("USER").ok().as_deref(),
    )
}

fn is_executable(path: &str) -> bool {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::metadata(path).is_ok_and(|m| m.is_file() && m.permissions().mode() & 0o111 != 0)
    }
    #[cfg(not(unix))]
    {
        std::path::Path::new(path).is_file()
    }
}

/// Shows what went wrong (in a dialog unless $PRIGH_DESKTOP_NO_DIALOG) and
/// exits.
fn fail(app: &AppHandle, f: Failure, code: i32) {
    let m = failure::message(OS, &f);
    log(format!("{}: {}", m.title, m.body));
    if !env_flag("PRIGH_DESKTOP_NO_DIALOG") {
        app.dialog().message(m.body).title(m.title).kind(MessageDialogKind::Error).blocking_show();
    }
    app.exit(code);
}

fn start_sidecar(app: AppHandle, sidecar: Arc<Sidecar>) {
    thread::spawn(move || {
        let path = if OS == Os::Unix { Some(sidecar_path()) } else { None };
        let env = launch::Env {
            backend: std::env::var("PRIGH_DESKTOP_BACKEND").ok(),
            args: std::env::var("PRIGH_DESKTOP_ARGS").ok(),
            wsl_distro: std::env::var("PRIGH_DESKTOP_WSL_DISTRO").ok(),
        };
        let plan = launch::plan(OS, &env, |name| search_path::find(name, path.as_deref().unwrap_or(""), is_executable));
        let plan = match plan {
            Ok(p) => p,
            Err(launch::Error::NotFound { name }) => return fail(&app, Failure::NotFound { name }, 1),
            Err(launch::Error::BadArgs(e)) => return fail(&app, Failure::BadArgs(e), 2),
        };
        log(format!("starting {} {}", plan.program, plan.args.join(" ")));
        let mut cmd = Command::new(&plan.program);
        cmd.args(&plan.args).stdin(Stdio::piped()).stdout(Stdio::piped()).stderr(Stdio::piped());
        if let Some(path) = &path {
            cmd.env("PATH", path);
        }
        if let Ok(home) = std::env::var("HOME") {
            cmd.current_dir(home);
        }
        #[cfg(windows)]
        {
            use std::os::windows::process::CommandExt;
            const CREATE_NO_WINDOW: u32 = 0x0800_0000;
            cmd.creation_flags(CREATE_NO_WINDOW);
        }
        let child = match SharedChild::spawn(&mut cmd) {
            Ok(c) => Arc::new(c),
            Err(e) => {
                let missing = e.kind() == std::io::ErrorKind::NotFound;
                return fail(&app, Failure::Spawn { program: plan.program, error: e.to_string(), missing }, 1);
            }
        };
        *sidecar.stdin.lock().unwrap() = child.take_stdin();
        // Were the app to exit before this, its end would close the pipe:
        // the sidecar never outlives it.
        *sidecar.child.lock().unwrap() = Some(child.clone());

        let tail = Arc::new(Mutex::new(Tail::new(30)));
        let stderr = child.take_stderr().map(|s| {
            let tail = tail.clone();
            thread::spawn(move || {
                for line in BufReader::new(s).lines().map_while(Result::ok) {
                    eprintln!("{line}");
                    tail.lock().unwrap().push(&line);
                }
            })
        });
        let ready_seen = Arc::new(AtomicBool::new(false));
        if let Some(out) = child.take_stdout() {
            let (app, sidecar, child, ready_seen, tail) =
                (app.clone(), sidecar.clone(), child.clone(), ready_seen.clone(), tail.clone());
            thread::spawn(move || {
                let mut lines = BufReader::new(out).lines().map_while(Result::ok);
                let Some(first) = lines.next() else { return };
                ready_seen.store(true, Ordering::SeqCst);
                match ready::parse(&first) {
                    Ok(page) => on_page(&app, &sidecar, &page),
                    Err(e) => {
                        sidecar.failing.store(true, Ordering::SeqCst);
                        let _ = child.kill();
                        thread::sleep(Duration::from_millis(200));
                        let stderr = tail.lock().unwrap().text();
                        return fail(&app, Failure::BadReady { message: e.0, stderr }, 1);
                    }
                }
                // Later pages: the backend's own site once the connect form
                // has connected (`-ui remote`).
                for line in lines {
                    match ready::parse(&line) {
                        Ok(page) => on_page(&app, &sidecar, &page),
                        Err(e) => log(e.0),
                    }
                }
            });
        }

        let status = child.wait();
        sidecar.exited.store(true, Ordering::SeqCst);
        if let Some(t) = stderr {
            let _ = t.join();
        }
        if sidecar.failing.load(Ordering::SeqCst) {
            return;
        }
        let stopping = sidecar.stopping.load(Ordering::SeqCst);
        match status {
            Ok(s) if s.success() || stopping => {
                log(format!("prigh desktop exited ({s})"));
                app.exit(0)
            }
            Ok(s) => {
                let stderr = tail.lock().unwrap().text();
                let ready = ready_seen.load(Ordering::SeqCst);
                fail(&app, Failure::Exited { code: s.code(), ready, stderr }, s.code().unwrap_or(1).max(1))
            }
            Err(e) => {
                log(format!("waiting for prigh desktop: {e}"));
                app.exit(1)
            }
        }
    });
}

fn on_page(app: &AppHandle, sidecar: &Sidecar, page: &ready::Page) {
    let url = page.url.as_str();
    let (Ok(parsed), Some(origin)) = (Url::parse(url), navigation::origin(url)) else {
        log(format!("prigh desktop's url does not parse: {url}"));
        return;
    };
    let first = {
        let mut origins = sidecar.origins.lock().unwrap();
        let first = origins.is_empty();
        if !origins.contains(&origin) {
            origins.push(origin);
        }
        first
    };
    if page.remote {
        log(format!("showing the backend's prigh-web at {}", redact(url)));
    } else {
        log(format!("prigh desktop is ready at {}", redact(url)));
    }
    let Some(w) = app.get_webview_window(WINDOW) else { return };
    let target = if first && cfg!(target_os = "linux") && env_flag("PRIGH_DESKTOP_GPU_INFO") {
        log(format!("PRIGH_DESKTOP_GPU_INFO: showing {GPU_INFO_URL} instead (reload to go on to prigh)"));
        GPU_INFO_URL.parse().expect("webkit://gpu")
    } else {
        parsed
    };
    if let Err(e) = w.navigate(target) {
        log(format!("navigating to prigh-web: {e}"));
    }
}

/// The URL without its fragment, which holds the run's key.
fn redact(url: &str) -> &str {
    url.split('#').next().unwrap_or(url)
}

fn open_external(app: &AppHandle, url: &str) {
    log(format!("opening {url} in the system browser"));
    if let Err(e) = app.opener().open_url(url, None::<&str>) {
        log(format!("could not open {url}: {e}"));
    }
}

/// The window's zoom (Ctrl+= / Ctrl+- / Ctrl+0 and Ctrl+wheel on Linux, the
/// View menu on macOS; WebView2 zooms by itself on Windows), kept between
/// runs in the app's config directory.
struct Zoom {
    level: Mutex<f64>,
    file: Option<PathBuf>,
}

impl Zoom {
    fn load(app: &AppHandle) -> Zoom {
        let file = app.path().app_config_dir().ok().map(|d| d.join("zoom"));
        let level =
            file.as_ref().and_then(|f| std::fs::read_to_string(f).ok()).and_then(|t| zoom::parse(&t)).unwrap_or(1.0);
        Zoom { level: Mutex::new(level), file }
    }

    fn level(&self) -> f64 {
        *self.level.lock().unwrap()
    }

    fn apply(&self, app: &AppHandle, step: Step) {
        let level = {
            let mut level = self.level.lock().unwrap();
            *level = zoom::step(*level, step);
            *level
        };
        if let Some(w) = app.get_webview_window(WINDOW) {
            if let Err(e) = w.set_zoom(level) {
                log(format!("zooming to {level}: {e}"));
            }
        }
        if let Some(file) = &self.file {
            let saved = file
                .parent()
                .map_or(Ok(()), std::fs::create_dir_all)
                .and_then(|()| std::fs::write(file, zoom::format(level)));
            if let Err(e) = saved {
                log(format!("keeping the zoom in {}: {e}", file.display()));
            }
        }
    }
}

fn build_window(app: &AppHandle, sidecar: Arc<Sidecar>, zoom: Arc<Zoom>) -> tauri::Result<()> {
    let (nav_app, nav_sidecar) = (app.clone(), sidecar.clone());
    let (new_app, new_sidecar) = (app.clone(), sidecar.clone());
    let gpu_info = cfg!(target_os = "linux") && env_flag("PRIGH_DESKTOP_GPU_INFO");
    let perm_sidecar = sidecar.clone();
    let load_sidecar = sidecar;
    let close_on_load = env_flag("PRIGH_DESKTOP_TEST_CLOSE_ON_LOAD");
    // Tests: a script run on each of the sidecar's pages, which closes the
    // window by going to [TEST_CLOSE_URL].
    let test_script = std::env::var("PRIGH_DESKTOP_TEST_SCRIPT")
        .ok()
        .filter(|p| !p.is_empty())
        .map(|p| std::fs::read_to_string(&p).unwrap_or_else(|e| panic!("PRIGH_DESKTOP_TEST_SCRIPT {p}: {e}")));
    let test_close = test_script.is_some();
    let window = WebviewWindowBuilder::new(app, WINDOW, WebviewUrl::App("index.html".into()))
        .title("prigh")
        .inner_size(1200.0, 800.0)
        .min_inner_size(320.0, 480.0)
        .background_color(Color(0x1b, 0x1e, 0x23, 0xff))
        // Files dropped on the window go to the page (prigh-web's
        // attachments and uploads), not to Tauri.
        .disable_drag_drop_handler()
        // WebView2's own Ctrl+=/-/0, Ctrl+wheel and pinch (Windows).
        .zoom_hotkeys_enabled(true)
        .on_navigation(move |url| match navigation::decide(url.as_str(), &nav_sidecar.origins()) {
            _ if test_close && url.as_str() == TEST_CLOSE_URL => {
                if let Some(w) = nav_app.get_webview_window(WINDOW) {
                    let _ = w.close();
                }
                false
            }
            _ if gpu_info && url.as_str() == GPU_INFO_URL => true,
            Decision::Quit => {
                log("the page's Quit: stopping prigh desktop");
                nav_sidecar.stop();
                false
            }
            Decision::Allow => true,
            Decision::External => {
                open_external(&nav_app, url.as_str());
                false
            }
            Decision::Block => {
                log(format!("refused to navigate to {url}"));
                false
            }
        })
        .on_new_window(move |url, _| match navigation::decide(url.as_str(), &new_sidecar.origins()) {
            Decision::Allow => NewWindowResponse::Allow,
            Decision::Quit => {
                new_sidecar.stop();
                NewWindowResponse::Deny
            }
            Decision::External => {
                open_external(&new_app, url.as_str());
                NewWindowResponse::Deny
            }
            Decision::Block => {
                log(format!("refused to open a window on {url}"));
                NewWindowResponse::Deny
            }
        })
        .on_download(|_, event| {
            match event {
                DownloadEvent::Requested { url, destination } => {
                    log(format!("downloading {} to {}", redact(url.as_str()), destination.display()))
                }
                DownloadEvent::Finished { url, path, success } => log(format!(
                    "download of {} {}{}",
                    redact(url.as_str()),
                    if success { "finished" } else { "failed" },
                    path.map(|p| format!(": {}", p.display())).unwrap_or_default()
                )),
                _ => {}
            }
            true
        })
        // Only prigh-web's own page asks: dictation needs the microphone,
        // finished turns notify, paste reads the clipboard.
        .on_permission_request(move |webview, kind| {
            let ours = webview.url().ok().is_some_and(|u| perm_sidecar.is_ours(u.as_str()));
            match kind {
                PermissionKind::Microphone | PermissionKind::Notifications | PermissionKind::ClipboardRead if ours => {
                    PermissionResponse::Allow
                }
                _ if ours => PermissionResponse::Default,
                _ => PermissionResponse::Deny,
            }
        })
        .on_page_load(move |window, payload| {
            if payload.event() != PageLoadEvent::Finished {
                return;
            }
            let url = payload.url().as_str();
            if load_sidecar.is_ours(url) {
                log(format!("loaded {}", redact(url)));
                if close_on_load {
                    let _ = window.close();
                }
                if let Some(script) = &test_script {
                    if let Err(e) = window.eval(script) {
                        log(format!("PRIGH_DESKTOP_TEST_SCRIPT: {e}"));
                    }
                }
            }
        })
        .build()?;
    if (zoom.level() - 1.0).abs() > 0.001 {
        let _ = window.set_zoom(zoom.level());
    }
    #[cfg(target_os = "linux")]
    {
        let (app, zoom) = (app.clone(), zoom.clone());
        window.with_webview(move |webview| linux::set_up(webview.inner(), app, zoom))?;
    }
    Ok(())
}

#[cfg(target_os = "linux")]
mod linux {
    use std::cell::Cell;
    use std::sync::Arc;

    use gtk::prelude::*;
    use prigh_desktop_core::zoom::Step;
    use tauri::AppHandle;
    use webkit2gtk::{FileChooserRequestExt, HardwareAccelerationPolicy, SettingsExt, WebViewExt};

    use super::{log, Zoom};

    /// GTK reads GTK_USE_PORTAL once, when it first opens a file chooser:
    /// set (as some Wayland sessions do) with no file-chooser portal running
    /// (Hyprland's own portal has none), the chooser never opens. Then GTK's
    /// own dialog is used instead. Before GTK starts.
    pub fn check_file_chooser_portal() {
        let wanted = std::env::var("GTK_USE_PORTAL").is_ok_and(|v| !v.is_empty() && v != "0");
        if !wanted {
            return;
        }
        let answer = gio::bus_get_sync(gio::BusType::Session, gio::Cancellable::NONE).and_then(|bus| {
            bus.call_sync(
                Some("org.freedesktop.portal.Desktop"),
                "/org/freedesktop/portal/desktop",
                "org.freedesktop.DBus.Properties",
                "Get",
                Some(&("org.freedesktop.portal.FileChooser", "version").to_variant()),
                None,
                gio::DBusCallFlags::NONE,
                2000,
                gio::Cancellable::NONE,
            )
        });
        if let Err(e) = answer {
            log(format!(
                "GTK_USE_PORTAL is set but no file-chooser portal answers ({e}): using GTK's own file chooser. \
                 To use the portal's, run one that has a FileChooser (e.g. xdg-desktop-portal-gtk)."
            ));
            std::env::remove_var("GTK_USE_PORTAL");
        }
    }

    pub fn set_up(webview: webkit2gtk::WebView, app: AppHandle, zoom: Arc<Zoom>) {
        if let Some(settings) = WebViewExt::settings(&webview) {
            // The GPU always (WebKitGTK may otherwise fall back to the CPU
            // for a page it judges simple); PRIGH_DESKTOP_GPU_INFO=1 shows
            // whether it got one.
            settings.set_hardware_acceleration_policy(HardwareAccelerationPolicy::Always);
            settings.set_enable_webgl(true);
            settings.set_enable_smooth_scrolling(true);
            // Zoom scales the whole page, as a browser's does.
            settings.set_zoom_text_only(false);
        }
        webview.connect_run_file_chooser(|_, request| {
            log(format!(
                "the page opens a file chooser{}",
                if request.selects_multiple() { " (several files)" } else { "" }
            ));
            false
        });
        {
            let (app, zoom) = (app.clone(), zoom.clone());
            webview.connect_key_press_event(move |widget, key| {
                use gtk::gdk::keys::constants as k;
                let ctrl = key.state().contains(gtk::gdk::ModifierType::CONTROL_MASK);
                let keyval = key.keyval();
                let step = match keyval {
                    _ if !ctrl => None,
                    k::plus | k::equal | k::KP_Add => Some(Step::In),
                    k::minus | k::underscore | k::KP_Subtract => Some(Step::Out),
                    k::_0 | k::KP_0 => Some(Step::Reset),
                    _ => None,
                };
                if let Some(step) = step {
                    zoom.apply(&app, step);
                    return glib::Propagation::Stop;
                }
                if keyval == k::F11 {
                    if let Some(window) = widget.toplevel().and_then(|t| t.downcast::<gtk::Window>().ok()) {
                        let full =
                            window.window().is_some_and(|w| w.state().contains(gtk::gdk::WindowState::FULLSCREEN));
                        if full {
                            window.unfullscreen()
                        } else {
                            window.fullscreen()
                        }
                    }
                    return glib::Propagation::Stop;
                }
                glib::Propagation::Proceed
            });
        }
        // Ctrl+wheel; a touchpad's smooth scrolling adds up to a step.
        let pending = Cell::new(0.0);
        webview.connect_scroll_event(move |_, event| {
            if !event.state().contains(gtk::gdk::ModifierType::CONTROL_MASK) {
                return glib::Propagation::Proceed;
            }
            let dy = match event.direction() {
                gtk::gdk::ScrollDirection::Up => -1.0,
                gtk::gdk::ScrollDirection::Down => 1.0,
                gtk::gdk::ScrollDirection::Smooth => event.delta().1,
                _ => 0.0,
            };
            pending.set(pending.get() + dy);
            if pending.get() <= -1.0 {
                pending.set(0.0);
                zoom.apply(&app, Step::In);
            } else if pending.get() >= 1.0 {
                pending.set(0.0);
                zoom.apply(&app, Step::Out);
            }
            glib::Propagation::Stop
        });
    }
}

#[cfg(target_os = "macos")]
const QUIT: &str = "quit";
#[cfg(target_os = "macos")]
const ZOOM_IN: &str = "zoom-in";
#[cfg(target_os = "macos")]
const ZOOM_OUT: &str = "zoom-out";
#[cfg(target_os = "macos")]
const ZOOM_RESET: &str = "zoom-reset";

/// macOS needs an app menu: WKWebView's Copy, Paste and Select All come
/// from its Edit items. Its Quit (⌘Q) stops the sidecar as closing the
/// window does, whose exit ends the app; elsewhere there is no menu and
/// Ctrl+Q reaches the page, whose Quit stops the sidecar itself.
#[cfg(target_os = "macos")]
fn macos_menu(app: &AppHandle) -> tauri::Result<tauri::menu::Menu<tauri::Wry>> {
    use tauri::menu::{Menu, MenuItem, PredefinedMenuItem, Submenu};
    let app_menu = Submenu::with_items(
        app,
        "prigh",
        true,
        &[
            &PredefinedMenuItem::about(app, None, None)?,
            &PredefinedMenuItem::separator(app)?,
            &PredefinedMenuItem::hide(app, None)?,
            &PredefinedMenuItem::hide_others(app, None)?,
            &PredefinedMenuItem::show_all(app, None)?,
            &PredefinedMenuItem::separator(app)?,
            &MenuItem::with_id(app, QUIT, "Quit prigh", true, Some("CmdOrCtrl+Q"))?,
        ],
    )?;
    let edit = Submenu::with_items(
        app,
        "Edit",
        true,
        &[
            &PredefinedMenuItem::undo(app, None)?,
            &PredefinedMenuItem::redo(app, None)?,
            &PredefinedMenuItem::separator(app)?,
            &PredefinedMenuItem::cut(app, None)?,
            &PredefinedMenuItem::copy(app, None)?,
            &PredefinedMenuItem::paste(app, None)?,
            &PredefinedMenuItem::select_all(app, None)?,
        ],
    )?;
    let view = Submenu::with_items(
        app,
        "View",
        true,
        &[
            &MenuItem::with_id(app, ZOOM_IN, "Zoom In", true, Some("CmdOrCtrl+="))?,
            &MenuItem::with_id(app, ZOOM_OUT, "Zoom Out", true, Some("CmdOrCtrl+-"))?,
            &MenuItem::with_id(app, ZOOM_RESET, "Actual Size", true, Some("CmdOrCtrl+0"))?,
        ],
    )?;
    let window = Submenu::with_items(
        app,
        "Window",
        true,
        &[
            &PredefinedMenuItem::minimize(app, None)?,
            &PredefinedMenuItem::fullscreen(app, None)?,
            &PredefinedMenuItem::separator(app)?,
            &PredefinedMenuItem::close_window(app, None)?,
        ],
    )?;
    Menu::with_items(app, &[&app_menu, &edit, &view, &window])
}

fn main() {
    #[cfg(target_os = "linux")]
    linux::check_file_chooser_portal();
    let sidecar = Arc::new(Sidecar::default());
    let setup_sidecar = sidecar.clone();
    let builder = tauri::Builder::default();
    #[cfg(target_os = "macos")]
    let builder = {
        let quit_sidecar = sidecar.clone();
        builder.menu(macos_menu).on_menu_event(move |app, event| {
            let step = match event.id().as_ref() {
                QUIT => {
                    log("Quit prigh: stopping prigh desktop");
                    quit_sidecar.stop();
                    None
                }
                ZOOM_IN => Some(Step::In),
                ZOOM_OUT => Some(Step::Out),
                ZOOM_RESET => Some(Step::Reset),
                _ => None,
            };
            if let (Some(step), Some(zoom)) = (step, app.try_state::<Arc<Zoom>>()) {
                zoom.apply(app, step);
            }
        })
    };
    let app = builder
        .plugin(tauri_plugin_single_instance::init(|app, _, _| {
            if let Some(w) = app.get_webview_window(WINDOW) {
                let _ = w.unminimize();
                let _ = w.show();
                let _ = w.set_focus();
            }
        }))
        .plugin(tauri_plugin_window_state::Builder::default().build())
        .plugin(tauri_plugin_opener::init())
        .plugin(tauri_plugin_dialog::init())
        .setup(move |app| {
            let zoom = Arc::new(Zoom::load(app.handle()));
            app.manage(zoom.clone());
            build_window(app.handle(), setup_sidecar.clone(), zoom)?;
            start_sidecar(app.handle().clone(), setup_sidecar.clone());
            Ok(())
        })
        .build(tauri::generate_context!())
        .expect("building the prigh desktop app");
    app.run(move |_, event| {
        // The last window closed: stop the sidecar first; its exit ends the
        // app (an exit we asked for has a code).
        if let RunEvent::ExitRequested { code: None, api, .. } = event {
            if !sidecar.exited.load(Ordering::SeqCst) && sidecar.child.lock().unwrap().is_some() {
                log("the window closed: stopping prigh desktop");
                api.prevent_exit();
                sidecar.stop();
            }
        }
    });
}
