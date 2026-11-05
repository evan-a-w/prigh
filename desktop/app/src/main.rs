// prigh's desktop app: a native window (the system webview: WebKitGTK,
// WKWebView, WebView2) on prigh-web, served by `prigh desktop`, the sidecar
// this process starts and stops. See desktop/README.md.
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use std::io::{BufRead, BufReader, Read};
use std::process::{ChildStdin, Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

use prigh_desktop_core::failure::{self, Failure};
use prigh_desktop_core::launch::{self, Os};
use prigh_desktop_core::navigation::{self, Decision};
use prigh_desktop_core::tail::Tail;
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
    /// The origin of its site, once its ready line said it.
    origin: Mutex<Option<String>>,
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
    fn origin(&self) -> Option<String> {
        self.origin.lock().unwrap().clone()
    }

    /// Closes its stdin, which tells it to stop, and kills it if it has not
    /// within [STOP_GRACE].
    fn stop(self: &Arc<Self>) {
        if self.stopping.swap(true, Ordering::SeqCst) {
            return;
        }
        drop(self.stdin.lock().unwrap().take());
        let Some(child) = self.child.lock().unwrap().clone() else { return };
        let me = self.clone();
        thread::spawn(move || {
            if let Ok(None) = child.wait_timeout(STOP_GRACE) {
                if !me.exited.load(Ordering::SeqCst) {
                    log("prigh desktop did not stop when asked: killing it");
                    let _ = child.kill();
                }
            }
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
                    Ok(url) => on_ready(&app, &sidecar, &url),
                    Err(e) => {
                        sidecar.failing.store(true, Ordering::SeqCst);
                        let _ = child.kill();
                        thread::sleep(Duration::from_millis(200));
                        let stderr = tail.lock().unwrap().text();
                        return fail(&app, Failure::BadReady { message: e.0, stderr }, 1);
                    }
                }
                for line in lines {
                    log(format!("prigh desktop printed after its ready line: {line}"));
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

fn on_ready(app: &AppHandle, sidecar: &Sidecar, url: &str) {
    let Ok(parsed) = Url::parse(url) else {
        log(format!("prigh desktop's url does not parse: {url}"));
        return;
    };
    *sidecar.origin.lock().unwrap() = navigation::origin(url);
    log(format!("prigh desktop is ready at {}", redact(url)));
    if let Some(w) = app.get_webview_window(WINDOW) {
        if let Err(e) = w.navigate(parsed) {
            log(format!("navigating to prigh-web: {e}"));
        }
    }
}

/// The URL without its fragment, which holds a one-time sign-in code.
fn redact(url: &str) -> &str {
    url.split('#').next().unwrap_or(url)
}

fn open_external(app: &AppHandle, url: &str) {
    log(format!("opening {url} in the system browser"));
    if let Err(e) = app.opener().open_url(url, None::<&str>) {
        log(format!("could not open {url}: {e}"));
    }
}

fn build_window(app: &AppHandle, sidecar: Arc<Sidecar>) -> tauri::Result<()> {
    let (nav_app, nav_sidecar) = (app.clone(), sidecar.clone());
    let (new_app, new_sidecar) = (app.clone(), sidecar.clone());
    let perm_sidecar = sidecar.clone();
    let load_sidecar = sidecar;
    let close_on_load = env_flag("PRIGH_DESKTOP_TEST_CLOSE_ON_LOAD");
    WebviewWindowBuilder::new(app, WINDOW, WebviewUrl::App("index.html".into()))
        .title("prigh")
        .inner_size(1200.0, 800.0)
        .min_inner_size(320.0, 480.0)
        .background_color(Color(0x1b, 0x1e, 0x23, 0xff))
        .on_navigation(move |url| match navigation::decide(url.as_str(), nav_sidecar.origin().as_deref()) {
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
        .on_new_window(move |url, _| match navigation::decide(url.as_str(), new_sidecar.origin().as_deref()) {
            Decision::Allow => NewWindowResponse::Allow,
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
            let ours = webview
                .url()
                .ok()
                .and_then(|u| navigation::origin(u.as_str()))
                .is_some_and(|o| Some(o) == perm_sidecar.origin());
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
            if navigation::origin(url).is_some_and(|o| Some(o) == load_sidecar.origin()) {
                log(format!("loaded {}", redact(url)));
                if close_on_load {
                    let _ = window.close();
                }
            }
        })
        .build()?;
    Ok(())
}

fn main() {
    let sidecar = Arc::new(Sidecar::default());
    let setup_sidecar = sidecar.clone();
    let app = tauri::Builder::default()
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
            build_window(app.handle(), setup_sidecar.clone())?;
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
