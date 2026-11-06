// prigh's desktop app: prigh-web in a native window (the system webview:
// WebKitGTK, WKWebView, WebView2) or an installed Chromium-family browser's
// app window, the backend's own or served by `prigh
// desktop`, the sidecar this process starts and stops. See "Desktop app" in
// ARCHITECTURE.md.
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use std::collections::HashMap;
use std::io::{BufRead, BufReader, Read};
use std::path::PathBuf;
use std::process::{ChildStdin, Command, Stdio};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

use prigh_desktop_core::browser::{self, Choice, Found, Mode, Platform};
use prigh_desktop_core::failure::{self, Failure};
use prigh_desktop_core::launch::{self, Os};
use prigh_desktop_core::navigation::{self, Decision, Remote};
use prigh_desktop_core::profile;
use prigh_desktop_core::tail::Tail;
use prigh_desktop_core::zoom::{self, Step};
use prigh_desktop_core::{ready, search_path};
use shared_child::SharedChild;
use tauri::webview::{Color, DownloadEvent, NewWindowResponse, PageLoadEvent, PermissionKind, PermissionResponse};
use tauri::{AppHandle, Manager, RunEvent, Url, WebviewUrl, WebviewWindowBuilder};
use tauri_plugin_dialog::{DialogExt, MessageDialogKind};
use tauri_plugin_opener::OpenerExt;

const OS: Os = if cfg!(windows) { Os::Windows } else { Os::Unix };
const PLATFORM: Platform = if cfg!(target_os = "macos") {
    Platform::MacOs
} else if cfg!(windows) {
    Platform::Windows
} else {
    Platform::Linux
};
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
    /// The page is in a browser's app window, not the app's own.
    browser: Mutex<Option<Arc<BrowserWindow>>>,
    /// The backend's site, as its last page line said.
    remote: Mutex<Option<Remote>>,
    /// The app's own windows' navigations, by label.
    windows: Mutex<HashMap<String, navigation::Window>>,
}

impl Sidecar {
    fn browser(&self) -> Option<Arc<BrowserWindow>> {
        self.browser.lock().unwrap().clone()
    }

    fn origins(&self) -> Vec<String> {
        self.origins.lock().unwrap().clone()
    }

    fn loaded(&self, label: &str, url: &str) -> Option<String> {
        let (origins, remote) = (self.origins(), self.remote());
        let mut windows = self.windows.lock().unwrap();
        windows.entry(label.to_string()).or_default().on_loaded(url, &origins, remote.as_ref())
    }

    fn remote(&self) -> Option<Remote> {
        self.remote.lock().unwrap().clone()
    }

    /// What the app's window `label` does about a navigation.
    fn step(&self, label: &str, url: &str) -> navigation::Step {
        let (origins, remote) = (self.origins(), self.remote());
        let mut windows = self.windows.lock().unwrap();
        windows.entry(label.to_string()).or_default().on_navigation(url, &origins, remote.as_ref(), Instant::now())
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

/// The page in an installed Chromium-family browser's app window (see
/// [prigh_desktop_core::browser]), with a profile of prigh's own: a browser
/// instance of its own, which exits when its last window closes. The app
/// then has no window: the browser's exit stops the sidecar, and the
/// sidecar's exit closes the browser.
struct BrowserWindow {
    found: Found,
    profile: PathBuf,
    child: Mutex<Option<Arc<SharedChild>>>,
    /// The page it opened, for another window on a second launch.
    url: Mutex<Option<String>>,
    /// We are closing it (the sidecar exited): its exit says nothing.
    closing: AtomicBool,
    /// The instance already on the profile that our launch handed the
    /// window to.
    handed_to: Mutex<Option<i32>>,
    /// Its DevTools, once it runs.
    #[cfg(target_os = "linux")]
    devtools: Mutex<Option<Arc<devtools_pipe::Watcher>>>,
}

/// How long the browser has to exit after SIGTERM before it is killed.
#[cfg(unix)]
const BROWSER_GRACE: Duration = Duration::from_secs(3);

impl BrowserWindow {
    fn command(&self, url: &str) -> Command {
        let mut cmd = Command::new(&self.found.program);
        cmd.args(browser::args(PLATFORM, &self.profile.to_string_lossy(), url))
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::piped());
        cmd
    }

    /// Opens the page; when the browser exits (its last window closed),
    /// stops the sidecar.
    fn open(self: &Arc<Self>, app: &AppHandle, sidecar: &Arc<Sidecar>, url: &str) {
        let url = browser::with_browser_shell(url);
        *self.url.lock().unwrap() = Some(url.clone());
        if let Err(e) = std::fs::create_dir_all(&self.profile) {
            log(format!("making the browser's profile {}: {e}", self.profile.display()));
        }
        // One app runs at a time (single-instance), so a browser on prigh's
        // profile now was left by an app that died (killed, or Ctrl-C in a
        // terminal): its windows show a page whose sidecar is gone, and one
        // handed to it would close with it, so it goes.
        #[cfg(unix)]
        if let Some(pid) = self.instance() {
            log(format!("{} was left running on prigh's profile (pid {pid}): closing it", self.found.name));
            self.end_instance(pid);
        }
        self.write_settings();
        #[cfg(target_os = "linux")]
        self.write_taskbar_entry(&url);
        log(format!("opening the page in {}'s app window ({})", self.found.name, self.found.program));
        let mut cmd = self.command(&url);
        // Its own process group: closing it reaches its helpers too.
        #[cfg(unix)]
        std::os::unix::process::CommandExt::process_group(&mut cmd, 0);
        #[cfg(target_os = "linux")]
        let devtools = devtools_pipe::attach(&mut cmd)
            .map_err(|e| log(format!("no DevTools pipe (links will open in its own windows): {e}")))
            .ok();
        let child = match SharedChild::spawn(&mut cmd) {
            Ok(c) => Arc::new(c),
            Err(e) => {
                sidecar.failing.store(true, Ordering::SeqCst);
                sidecar.stop();
                let program = self.found.program.clone();
                return fail(app, Failure::BrowserSpawn { program, error: e.to_string() }, 1);
            }
        };
        *self.child.lock().unwrap() = Some(child.clone());
        #[cfg(target_os = "linux")]
        if let Some(pipe) = devtools {
            *self.devtools.lock().unwrap() = Some(pipe.watch(app.clone(), sidecar.clone(), self.clone()));
        }
        // Browsers say a lot on stderr: only kept, for when it fails.
        let tail = Arc::new(Mutex::new(Tail::new(20)));
        if let Some(s) = child.take_stderr() {
            let tail = tail.clone();
            thread::spawn(move || {
                for line in BufReader::new(s).lines().map_while(Result::ok) {
                    tail.lock().unwrap().push(&line);
                }
            });
        }
        let (me, sidecar) = (self.clone(), sidecar.clone());
        thread::spawn(move || {
            let status = child.wait();
            if me.closing.load(Ordering::SeqCst) {
                return;
            }
            // Already running on the profile (left by an app that was
            // killed), the browser hands it the window and exits: the
            // window is that instance's.
            #[cfg(unix)]
            if let Some(pid) = me.instance().filter(|p| *p as u32 != child.id()) {
                log(format!("{} handed the window to its instance on prigh's profile (pid {pid})", me.found.name));
                *me.handed_to.lock().unwrap() = Some(pid);
                while alive(pid) && !me.closing.load(Ordering::SeqCst) {
                    thread::sleep(Duration::from_millis(500));
                }
                if me.closing.load(Ordering::SeqCst) {
                    return;
                }
            }
            match status {
                Ok(s) if !s.success() => {
                    log(format!("{} exited ({s}):\n{}", me.found.name, tail.lock().unwrap().text()))
                }
                _ => {}
            }
            log("the browser window closed: stopping prigh desktop");
            sidecar.stop();
        });
    }

    /// prigh's settings, merged into the profile's (see
    /// [prigh_desktop_core::profile]) while no browser runs on it.
    fn write_settings(&self) {
        let write = |path: PathBuf, merge: &dyn Fn(Option<&str>) -> String| {
            let existing = std::fs::read_to_string(&path).ok();
            let tmp = path.with_extension("prigh-tmp");
            let written = path
                .parent()
                .map_or(Ok(()), std::fs::create_dir_all)
                .and_then(|()| std::fs::write(&tmp, merge(existing.as_deref())))
                .and_then(|()| std::fs::rename(&tmp, &path));
            if let Err(e) = written {
                log(format!("writing the browser's settings {}: {e}", path.display()));
            }
        };
        let downloads = downloads_dir();
        write(self.profile.join("Default").join("Preferences"), &|e| profile::preferences(e, &downloads));
        write(self.profile.join("Local State"), &profile::local_state);
    }

    /// The desktop entry named after the window's app_id on Wayland (see
    /// [prigh_desktop_core::taskbar]), in the user's applications.
    #[cfg(target_os = "linux")]
    fn write_taskbar_entry(&self, url: &str) {
        let Some(app_id) = prigh_desktop_core::taskbar::app_id(&self.found.program, url) else { return };
        let var = |name: &str| std::env::var(name).ok().filter(|v| !v.is_empty());
        let Some(data) = var("XDG_DATA_HOME").or_else(|| var("HOME").map(|h| format!("{h}/.local/share"))) else {
            return;
        };
        // How the app was started: makeWrapper's wrapper passes its own path.
        let exec = std::env::args()
            .next()
            .filter(|a| !a.contains('/') || a.starts_with('/'))
            .or_else(|| std::env::current_exe().ok().map(|p| p.display().to_string()))
            .unwrap_or_else(|| "prigh-desktop".into());
        let path = PathBuf::from(data).join("applications").join(format!("{app_id}.desktop"));
        let entry = prigh_desktop_core::taskbar::desktop_entry(&app_id, &exec);
        if std::fs::read_to_string(&path).ok().as_deref() == Some(&entry) {
            return;
        }
        let written = path.parent().map_or(Ok(()), std::fs::create_dir_all).and_then(|()| std::fs::write(&path, entry));
        match written {
            Ok(()) => log(format!("the window's taskbar entry: {}", path.display())),
            Err(e) => log(format!("writing the window's taskbar entry {}: {e}", path.display())),
        }
    }

    /// The browser instance running on prigh's profile: the pid in its
    /// SingletonLock, if that process is alive and runs on this profile.
    #[cfg(unix)]
    fn instance(&self) -> Option<i32> {
        let target = std::fs::read_link(self.profile.join("SingletonLock")).ok()?;
        let pid = browser::singleton_pid(&target.to_string_lossy())?;
        (alive(pid) && self.runs_on_profile(pid)).then_some(pid)
    }

    #[cfg(unix)]
    fn runs_on_profile(&self, pid: i32) -> bool {
        // Only Linux says (in /proc) what a process runs: elsewhere a pid
        // from the lock is not trusted with a signal.
        // The command line can be rewritten (its title) or reordered (a
        // relaunch): files it holds open in the profile say it too.
        std::fs::read(format!("/proc/{pid}/cmdline"))
            .is_ok_and(|c| browser::names_profile(&c, &self.profile.to_string_lossy()))
            || std::fs::read_dir(format!("/proc/{pid}/fd")).is_ok_and(|fds| {
                let profile = std::fs::canonicalize(&self.profile).unwrap_or_else(|_| self.profile.clone());
                fds.filter_map(Result::ok)
                    .filter_map(|fd| std::fs::read_link(fd.path()).ok())
                    .any(|target| target.starts_with(&profile))
            })
    }

    /// Ends the instance `pid` on prigh's profile: SIGTERM, then SIGKILL.
    #[cfg(unix)]
    fn end_instance(&self, pid: i32) {
        let running = || alive(pid) && self.runs_on_profile(pid);
        unsafe { libc::kill(pid, libc::SIGTERM) };
        let deadline = std::time::Instant::now() + BROWSER_GRACE;
        while running() && std::time::Instant::now() < deadline {
            thread::sleep(Duration::from_millis(100));
        }
        if running() {
            log("it did not close: killing it");
            unsafe { libc::kill(pid, libc::SIGKILL) };
            while alive(pid) && std::time::Instant::now() < deadline + BROWSER_GRACE {
                thread::sleep(Duration::from_millis(100));
            }
        }
    }

    /// The backend's site, as the sidecar now says to show it (see
    /// [prigh_desktop_core::devtools::Links::set_remote]).
    fn set_remote(&self, remote: Remote, here: bool) {
        #[cfg(target_os = "linux")]
        if let Some(watcher) = self.devtools.lock().unwrap().clone() {
            watcher.set_remote(remote, here);
            return;
        }
        let _ = here;
        if !remote.cookies.is_empty() {
            log("no DevTools pipe to give the browser window the sign-in: it signs in to the site itself");
        }
    }

    /// A second launch of the app: another window on the page.
    fn another_window(&self) {
        if let Some(url) = self.url.lock().unwrap().clone() {
            self.window_on(&url);
        }
    }

    /// Another app window on `url`: a launch of the browser on the
    /// profile, which hands it to the running instance.
    fn window_on(&self, url: &str) {
        match self.command(url).stderr(Stdio::null()).spawn() {
            Ok(mut c) => {
                thread::spawn(move || {
                    let _ = c.wait();
                });
            }
            Err(e) => log(format!("opening another window: {e}")),
        }
    }

    /// The sidecar exited: closes the browser, SIGTERM first (Unix), to
    /// its process group while the process we started runs, and to the
    /// instance it handed the window to.
    fn close(&self) {
        if self.closing.swap(true, Ordering::SeqCst) {
            return;
        }
        let child = self.child.lock().unwrap().clone();
        #[cfg(unix)]
        {
            // Only what we started, or the instance it handed the window to
            // (still that process: alive, on this profile).
            let handed = || (*self.handed_to.lock().unwrap()).filter(|p| alive(*p) && self.runs_on_profile(*p));
            let signal = |sig: libc::c_int| {
                if let Some(c) = &child {
                    // Not reaped: its pid (its group's id) is still ours.
                    if matches!(c.try_wait(), Ok(None)) {
                        unsafe { libc::killpg(c.id() as libc::pid_t, sig) };
                    }
                }
                if let Some(pid) = handed() {
                    unsafe { libc::kill(pid, sig) };
                }
            };
            let running =
                || child.as_ref().is_some_and(|c| matches!(c.try_wait(), Ok(None))) || self.instance().is_some();
            if !running() {
                return;
            }
            log("prigh desktop exited: closing the browser window");
            signal(libc::SIGTERM);
            let deadline = std::time::Instant::now() + BROWSER_GRACE;
            while running() && std::time::Instant::now() < deadline {
                thread::sleep(Duration::from_millis(100));
            }
            if running() {
                log("the browser did not close: killing it");
                signal(libc::SIGKILL);
            }
        }
        #[cfg(not(unix))]
        if let Some(c) = child {
            log("prigh desktop exited: closing the browser window");
            let _ = c.kill();
        }
    }
}

/// The browser's DevTools on a pipe (`--remote-debugging-pipe`: it reads
/// commands on its fd 3 and writes on its fd 4), for where its links go
/// and which of its windows stay (see [prigh_desktop_core::devtools]). Only
/// the app holds it: no port.
#[cfg(target_os = "linux")]
mod devtools_pipe {
    use super::{downloads_dir, env_flag, log, open_external, BrowserWindow, Sidecar};
    use prigh_desktop_core::devtools::{Effect, Links};
    use prigh_desktop_core::navigation::Remote;
    use std::fs::File;
    use std::io::{BufRead, BufReader, Write};
    use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
    use std::os::unix::process::CommandExt;
    use std::process::Command;
    use std::sync::{Arc, Mutex};
    use std::thread;
    use std::time::Instant;
    use tauri::AppHandle;

    pub struct Pipe {
        to_browser: File,
        from_browser: File,
        /// The browser's ends, closed here once it has them.
        theirs: [OwnedFd; 2],
    }

    fn pipe() -> std::io::Result<(OwnedFd, OwnedFd)> {
        let mut fds = [0; 2];
        if unsafe { libc::pipe2(fds.as_mut_ptr(), libc::O_CLOEXEC) } != 0 {
            return Err(std::io::Error::last_os_error());
        }
        Ok(unsafe { (OwnedFd::from_raw_fd(fds[0]), OwnedFd::from_raw_fd(fds[1])) })
    }

    /// Gives the browser `cmd` starts the pipe.
    pub fn attach(cmd: &mut Command) -> std::io::Result<Pipe> {
        let (commands_read, commands_write) = pipe()?;
        let (events_read, events_write) = pipe()?;
        let (read, write) = (commands_read.as_raw_fd(), events_write.as_raw_fd());
        cmd.arg("--remote-debugging-pipe");
        unsafe {
            cmd.pre_exec(move || {
                // Above 4 first, so that neither is overwritten by the
                // other's dup2; dup2 clears close-on-exec on 3 and 4.
                let read = libc::fcntl(read, libc::F_DUPFD_CLOEXEC, 5);
                let write = libc::fcntl(write, libc::F_DUPFD_CLOEXEC, 5);
                if read < 0 || write < 0 || libc::dup2(read, 3) < 0 || libc::dup2(write, 4) < 0 {
                    return Err(std::io::Error::last_os_error());
                }
                Ok(())
            })
        };
        Ok(Pipe {
            to_browser: File::from(commands_write),
            from_browser: File::from(events_read),
            theirs: [commands_read, events_write],
        })
    }

    /// The browser's DevTools: [Links], and the pipe to the browser.
    pub struct Watcher {
        state: Mutex<(Links, File)>,
        app: AppHandle,
        window: Arc<BrowserWindow>,
    }

    impl Watcher {
        fn apply(&self, to_browser: &mut File, effects: Vec<Effect>) {
            for effect in effects {
                match effect {
                    Effect::Send(command) => {
                        let _ = to_browser.write_all(command.as_bytes()).and_then(|()| to_browser.write_all(b"\0"));
                    }
                    Effect::OpenExternally(url) => open_external(&self.app, &url),
                    Effect::OpenWindow(url) => {
                        log("a popup on prigh's site: opening it in another app window");
                        self.window.window_on(&url)
                    }
                    Effect::Log(text) => log(text),
                }
            }
        }

        pub fn set_remote(&self, remote: Remote, here: bool) {
            let mut state = self.state.lock().unwrap();
            let (links, to_browser) = &mut *state;
            let effects = links.set_remote(remote, here);
            self.apply(to_browser, effects);
        }
    }

    impl Pipe {
        /// The browser started: follows its pages until it exits.
        pub fn watch(self, app: AppHandle, sidecar: Arc<Sidecar>, window: Arc<BrowserWindow>) -> Arc<Watcher> {
            let Pipe { to_browser, from_browser, theirs } = self;
            drop(theirs);
            let mut links = Links::new(env_flag("PRIGH_DESKTOP_DEVTOOLS"));
            let start = links.start(&downloads_dir());
            let watcher = Arc::new(Watcher { state: Mutex::new((links, to_browser)), app, window });
            {
                let mut state = watcher.state.lock().unwrap();
                let (links, to_browser) = &mut *state;
                watcher.apply(to_browser, start.into_iter().map(Effect::Send).collect());
                if let Some(remote) = sidecar.remote() {
                    let effects = links.set_remote(remote, false);
                    watcher.apply(to_browser, effects);
                }
            }
            let me = watcher.clone();
            thread::spawn(move || {
                let mut reader = BufReader::new(from_browser);
                let mut msg = Vec::new();
                while reader.read_until(0, &mut msg).is_ok_and(|n| n > 0) {
                    if msg.last() == Some(&0) {
                        msg.pop();
                    }
                    let text = String::from_utf8_lossy(&msg);
                    let mut state = me.state.lock().unwrap();
                    let (links, to_browser) = &mut *state;
                    let effects = links.on_message(&text, &sidecar.origins(), Instant::now());
                    me.apply(to_browser, effects);
                    drop(state);
                    msg.clear();
                }
            });
            watcher
        }
    }
}

/// See [profile::downloads_dir].
fn downloads_dir() -> String {
    let var = |name: &str| std::env::var(name).ok().filter(|v| !v.is_empty());
    let home = var("HOME").or_else(|| var("USERPROFILE")).unwrap_or_default();
    let config = var("XDG_CONFIG_HOME").unwrap_or_else(|| format!("{home}/.config"));
    profile::downloads_dir(&home, std::fs::read_to_string(format!("{config}/user-dirs.dirs")).ok().as_deref())
}

#[cfg(unix)]
fn alive(pid: i32) -> bool {
    unsafe { libc::kill(pid, 0) == 0 }
}

/// Where the page is shown: `None` for the app's own window.
fn window_choice(app: &AppHandle) -> Result<Option<Arc<BrowserWindow>>, Failure> {
    let var = |name: &str| std::env::var(name).ok().filter(|v| !v.is_empty());
    let mode = browser::mode(var("PRIGH_DESKTOP_WINDOW").as_deref()).map_err(Failure::BadWindow)?;
    if mode == Mode::Webview {
        return Ok(None);
    }
    // No login shell (it may take seconds): the usual install places cover
    // a launcher's bare PATH.
    let path = search_path::compose(None, var("PATH").as_deref(), var("HOME").as_deref(), var("USER").as_deref());
    let places = browser::Places {
        home: var("HOME"),
        windows_roots: ["ProgramFiles", "ProgramFiles(x86)", "LOCALAPPDATA"].iter().filter_map(|v| var(v)).collect(),
    };
    let on_path = |name: &str| if OS == Os::Windows { None } else { search_path::find(name, &path, is_executable) };
    let found = browser::find(PLATFORM, var("PRIGH_DESKTOP_BROWSER").as_deref(), &places, on_path, is_executable)
        .map_err(Failure::BadWindow)?;
    match browser::choose(mode, PLATFORM, found) {
        Choice::Webview => Ok(None),
        Choice::NoBrowser => Err(Failure::NoBrowser),
        Choice::Browser(found) => {
            let profile = app
                .path()
                .app_data_dir()
                .map_err(|e| Failure::BadWindow(format!("no data directory for the browser's profile: {e}")))?
                .join("browser");
            Ok(Some(Arc::new(BrowserWindow {
                found,
                profile,
                child: Mutex::new(None),
                url: Mutex::new(None),
                closing: AtomicBool::new(false),
                handed_to: Mutex::new(None),
                #[cfg(target_os = "linux")]
                devtools: Mutex::new(None),
            })))
        }
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
        if let Some(b) = sidecar.browser() {
            b.close();
        }
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

fn on_page(app: &AppHandle, sidecar: &Arc<Sidecar>, page: &ready::Page) {
    let url = page.url.as_str();
    if page.external {
        // Its fragment has the sign-in's one-time state: not for the log.
        log(format!("opening {} in the system browser to sign in", redact(url)));
        if let Err(e) = app.opener().open_url(url, None::<&str>) {
            log(format!("could not open {}: {e}", redact(url)));
        }
        return;
    }
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
        let remote = Remote {
            url: url.to_string(),
            cookies: page.cookie.as_deref().map(navigation::cookie_pairs).unwrap_or_default(),
            signin_page: page.signin.clone(),
        };
        *sidecar.remote.lock().unwrap() = Some(remote.clone());
        if page.here {
            log("signing in to the backend's site in the window");
        }
        if let Some(b) = sidecar.browser() {
            b.set_remote(remote, page.here);
        } else if let Some(w) = app.get_webview_window(WINDOW) {
            let mut windows = sidecar.windows.lock().unwrap();
            let window = windows.entry(WINDOW.to_string()).or_default();
            if page.here {
                window.sign_in_here();
            } else if !remote.cookies.is_empty() {
                set_cookies(&w, &remote);
                window.cookies_given(&remote.cookies);
            }
        }
    } else {
        log(format!("prigh desktop is ready at {}", redact(url)));
    }
    if let Some(b) = sidecar.browser() {
        // Later pages (the backend's site after the connect form) the
        // page goes to itself.
        if first {
            b.open(app, sidecar, url);
        }
        return;
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

/// After a refused navigation of the app's window `label`: gives it the
/// backend's site's cookies (`cookies`), then loads `url`.
fn later_in_window(app: &AppHandle, sidecar: &Arc<Sidecar>, label: &str, url: String, cookies: bool) {
    let (app, sidecar, label) = (app.clone(), sidecar.clone(), label.to_string());
    thread::spawn(move || {
        let Some(w) = app.get_webview_window(&label) else { return };
        if cookies {
            if let Some(remote) = sidecar.remote() {
                set_cookies(&w, &remote);
            }
        }
        match Url::parse(&url) {
            Ok(parsed) => {
                if let Err(e) = w.navigate(parsed) {
                    log(format!("navigating to {}: {e}", redact(&url)));
                }
            }
            Err(e) => log(format!("{}: {e}", redact(&url))),
        }
    });
}

/// Gives the app's window the backend's site's cookies (from signing in to
/// its proxy through the default browser).
fn set_cookies(w: &tauri::WebviewWindow, remote: &Remote) {
    let Some(domain) = Url::parse(&remote.url).ok().and_then(|u| u.host_str().map(str::to_string)) else { return };
    for (name, value) in &remote.cookies {
        let cookie = tauri::webview::Cookie::build((name.clone(), value.clone()))
            .domain(domain.clone())
            .path("/")
            .secure(remote.url.starts_with("https://"))
            .http_only(true)
            .build();
        match w.set_cookie(cookie) {
            Ok(()) => log(format!("gave the window the cookie {name} for {domain}")),
            Err(e) => log(format!("could not give the window the cookie {name} for {domain}: {e}")),
        }
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
        for w in app.webview_windows().values() {
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

/// Labels of the windows after the first ([WINDOW]).
static WINDOWS: AtomicUsize = AtomicUsize::new(1);

/// Another window on the page's site (the page's Open in new window), with
/// the same sidecar: the app ends when the last window closes. Off the
/// webview's callback, which building a window from would deadlock.
fn another_app_window(app: &AppHandle, sidecar: Arc<Sidecar>, url: Url) {
    let app = app.clone();
    thread::spawn(move || {
        let Some(zoom) = app.try_state::<Arc<Zoom>>().map(|z| z.inner().clone()) else { return };
        let label = format!("{WINDOW}-{}", WINDOWS.fetch_add(1, Ordering::SeqCst));
        log(format!("opening another window on {}", redact(url.as_str())));
        if let Err(e) = build_window(&app, sidecar, zoom, &label, WebviewUrl::External(url)) {
            log(format!("opening another window: {e}"));
        }
    });
}

fn build_window(
    app: &AppHandle,
    sidecar: Arc<Sidecar>,
    zoom: Arc<Zoom>,
    label: &str,
    url: WebviewUrl,
) -> tauri::Result<()> {
    let (nav_app, nav_sidecar, nav_label) = (app.clone(), sidecar.clone(), label.to_string());
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
    let window = WebviewWindowBuilder::new(app, label, url)
        .title("prigh")
        .inner_size(1200.0, 800.0)
        .min_inner_size(320.0, 480.0)
        .background_color(Color(0x1b, 0x1e, 0x23, 0xff))
        // Files dropped on the window go to the page (prigh-web's
        // attachments and uploads), not to Tauri.
        .disable_drag_drop_handler()
        // WebView2's own Ctrl+=/-/0, Ctrl+wheel and pinch (Windows).
        .zoom_hotkeys_enabled(true)
        .on_navigation(move |url| {
            if test_close && url.as_str() == TEST_CLOSE_URL {
                if let Some(w) = nav_app.get_webview_window(&nav_label) {
                    let _ = w.close();
                }
                return false;
            }
            if gpu_info && url.as_str() == GPU_INFO_URL {
                return true;
            }
            match nav_sidecar.step(&nav_label, url.as_str()) {
                navigation::Step::Quit => {
                    log("the page's Quit: stopping prigh desktop");
                    nav_sidecar.stop();
                    false
                }
                navigation::Step::Allow => true,
                navigation::Step::Away(text) => {
                    log(text);
                    true
                }
                navigation::Step::Open(url) => {
                    open_external(&nav_app, &url);
                    false
                }
                navigation::Step::Refuse(text) => {
                    if let Some(text) = text {
                        log(text);
                    }
                    false
                }
                navigation::Step::Cookies { url } => {
                    log("the backend's site wants a sign-in: giving the window the one from the browser");
                    later_in_window(&nav_app, &nav_sidecar, &nav_label, url, true);
                    false
                }
                navigation::Step::SignIn { url } => {
                    log("the backend's site wants a sign-in: showing prigh's sign-in page");
                    later_in_window(&nav_app, &nav_sidecar, &nav_label, url, false);
                    false
                }
            }
        })
        .on_new_window(move |url, _| match navigation::decide(url.as_str(), &new_sidecar.origins()) {
            Decision::Allow => {
                another_app_window(&new_app, new_sidecar.clone(), url);
                NewWindowResponse::Deny
            }
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
            if let Some(text) = load_sidecar.loaded(window.label(), url) {
                log(text);
            }
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
            // The page opens a window (Open in new window) from an effect run
            // after the click, which WebKit no longer counts as the user's;
            // the shell's on_new_window still decides where it goes.
            settings.set_javascript_can_open_windows_automatically(true);
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

/// Ctrl-C in a terminal, a SIGTERM or the terminal closing: the app stops
/// as when its window closes (the sidecar first, then the browser), rather
/// than dying and leaving the browser, in a process group of its own,
/// running. Again: at once.
#[cfg(target_os = "linux")]
fn stop_on_signals(app: &AppHandle, sidecar: &Arc<Sidecar>) {
    for (signal, name) in [(libc::SIGINT, "SIGINT"), (libc::SIGTERM, "SIGTERM"), (libc::SIGHUP, "SIGHUP")] {
        let (app, sidecar) = (app.clone(), sidecar.clone());
        glib::unix_signal_add(signal, move || {
            if sidecar.stopping.load(Ordering::SeqCst) || sidecar.child.lock().unwrap().is_none() {
                log(format!("{name} again: stopping now"));
                if let Some(b) = sidecar.browser() {
                    b.close();
                }
                app.exit(1);
            } else {
                log(format!("{name}: stopping prigh desktop"));
                sidecar.stop();
            }
            glib::ControlFlow::Continue
        });
    }
}

fn main() {
    #[cfg(target_os = "linux")]
    linux::check_file_chooser_portal();
    let sidecar = Arc::new(Sidecar::default());
    let setup_sidecar = sidecar.clone();
    let instance_sidecar = sidecar.clone();
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
        .plugin(tauri_plugin_single_instance::init(move |app, _, _| {
            if let Some(w) = app.get_webview_window(WINDOW).or_else(|| app.webview_windows().into_values().next()) {
                let _ = w.unminimize();
                let _ = w.show();
                let _ = w.set_focus();
            } else if let Some(b) = instance_sidecar.browser() {
                b.another_window();
            }
        }))
        .plugin(tauri_plugin_window_state::Builder::default().build())
        .plugin(tauri_plugin_opener::init())
        .plugin(tauri_plugin_dialog::init())
        .setup(move |app| {
            let zoom = Arc::new(Zoom::load(app.handle()));
            app.manage(zoom.clone());
            #[cfg(target_os = "linux")]
            stop_on_signals(app.handle(), &setup_sidecar);
            match window_choice(app.handle()) {
                Err(f) => {
                    let app = app.handle().clone();
                    thread::spawn(move || fail(&app, f, 2));
                    return Ok(());
                }
                Ok(Some(b)) => *setup_sidecar.browser.lock().unwrap() = Some(b),
                Ok(None) => build_window(
                    app.handle(),
                    setup_sidecar.clone(),
                    zoom,
                    WINDOW,
                    WebviewUrl::App("index.html".into()),
                )?,
            }
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
