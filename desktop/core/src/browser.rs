//! The window prigh-web is shown in (`PRIGH_DESKTOP_WINDOW`): the system
//! webview (Tauri's window), or an installed Chromium-family browser's app
//! window (`--app=URL`, with a profile of prigh's own so that it is a
//! browser instance of its own, which exits when its last window closes).
//! `auto` takes the browser on Linux, whose webview (WebKitGTK) is the
//! weakest, and the webview elsewhere: WebView2 is Chromium already, and on
//! macOS Chrome keeps running when its last window closes, so closing the
//! window would not end the app.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    Auto,
    Browser,
    Webview,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Platform {
    Linux,
    MacOs,
    Windows,
}

pub fn mode(value: Option<&str>) -> Result<Mode, String> {
    match value.map(str::trim).filter(|v| !v.is_empty()) {
        None | Some("auto") => Ok(Mode::Auto),
        Some("browser") => Ok(Mode::Browser),
        Some("webview") => Ok(Mode::Webview),
        Some(other) => Err(format!("PRIGH_DESKTOP_WINDOW is {other:?}: use auto, browser or webview")),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Found {
    pub name: String,
    pub program: String,
}

/// The browsers looked for, best first: (name, program names on a Linux
/// PATH, macOS app, Windows path under an install root).
const BROWSERS: &[(&str, &[&str], &str, &str)] = &[
    (
        "Google Chrome",
        &["google-chrome-stable", "google-chrome"],
        "Google Chrome.app/Contents/MacOS/Google Chrome",
        r"Google\Chrome\Application\chrome.exe",
    ),
    (
        "Chromium",
        &["chromium", "chromium-browser"],
        "Chromium.app/Contents/MacOS/Chromium",
        r"Chromium\Application\chrome.exe",
    ),
    (
        "Brave",
        &["brave", "brave-browser", "brave-browser-stable"],
        "Brave Browser.app/Contents/MacOS/Brave Browser",
        r"BraveSoftware\Brave-Browser\Application\brave.exe",
    ),
    (
        "Microsoft Edge",
        &["microsoft-edge-stable", "microsoft-edge"],
        "Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
        r"Microsoft\Edge\Application\msedge.exe",
    ),
    (
        "Vivaldi",
        &["vivaldi-stable", "vivaldi"],
        "Vivaldi.app/Contents/MacOS/Vivaldi",
        r"Vivaldi\Application\vivaldi.exe",
    ),
];

/// Where to look besides the PATH: the user's home (macOS ~/Applications)
/// and Windows' install roots (%ProgramFiles%, %ProgramFiles(x86)%,
/// %LocalAppData%).
#[derive(Default)]
pub struct Places {
    pub home: Option<String>,
    pub windows_roots: Vec<String>,
}

/// `explicit` ($PRIGH_DESKTOP_BROWSER) wins when it is executable; else the
/// first browser found: on the PATH (`on_path` finds a program name there),
/// or where the platform installs apps (`executable` checks a path).
pub fn find(
    platform: Platform,
    explicit: Option<&str>,
    places: &Places,
    on_path: impl Fn(&str) -> Option<String>,
    executable: impl Fn(&str) -> bool,
) -> Result<Option<Found>, String> {
    if let Some(path) = explicit.map(str::trim).filter(|p| !p.is_empty()) {
        let program = if path.contains('/') || path.contains('\\') { Some(path.to_string()) } else { on_path(path) };
        return match program.filter(|p| executable(p)) {
            Some(program) => Ok(Some(Found { name: path.to_string(), program })),
            None => Err(format!("PRIGH_DESKTOP_BROWSER is {path:?}, which is not an executable")),
        };
    }
    for (name, linux, mac, win) in BROWSERS {
        let found = match platform {
            Platform::Linux => linux.iter().find_map(|p| on_path(p)),
            Platform::MacOs => {
                let mut dirs = vec!["/Applications".to_string()];
                if let Some(home) = &places.home {
                    dirs.push(format!("{home}/Applications"));
                }
                dirs.into_iter().map(|d| format!("{d}/{mac}")).find(|p| executable(p))
            }
            Platform::Windows => places.windows_roots.iter().map(|r| format!(r"{r}\{win}")).find(|p| executable(p)),
        };
        if let Some(program) = found {
            return Ok(Some(Found { name: name.to_string(), program }));
        }
    }
    Ok(None)
}

#[derive(Debug, PartialEq, Eq)]
pub enum Choice {
    Webview,
    Browser(Found),
    /// `browser` was asked for and there is none.
    NoBrowser,
}

pub fn choose(mode: Mode, platform: Platform, found: Option<Found>) -> Choice {
    match (mode, found) {
        (Mode::Webview, _) => Choice::Webview,
        (Mode::Browser, Some(f)) => Choice::Browser(f),
        (Mode::Browser, None) => Choice::NoBrowser,
        (Mode::Auto, Some(f)) if platform == Platform::Linux => Choice::Browser(f),
        (Mode::Auto, _) => Choice::Webview,
    }
}

/// The class X11 window managers see (`--class`); on Wayland Chromium names
/// an app window's app_id after its site and profile instead.
pub const WINDOW_CLASS: &str = "prigh";

/// The browser's arguments for an app window on `url` in `profile`.
pub fn args(platform: Platform, profile: &str, url: &str) -> Vec<String> {
    let mut args = vec![
        format!("--app={url}"),
        format!("--user-data-dir={profile}"),
        "--no-first-run".to_string(),
        "--no-default-browser-check".to_string(),
    ];
    if platform == Platform::Linux {
        args.push(format!("--class={WINDOW_CLASS}"));
        args.push("--ozone-platform-hint=auto".to_string());
    }
    args
}

/// The page's URL with `shell=browser` in its fragment: prigh-web then
/// quits by closing its window (an app window may), since the backend's
/// own page cannot reach `prigh desktop`.
pub fn with_browser_shell(url: &str) -> String {
    match url.split_once('#') {
        Some((base, "")) => format!("{base}#shell=browser"),
        Some((base, fragment)) => format!("{base}#{fragment}&shell=browser"),
        None => format!("{url}#shell=browser"),
    }
}

/// The pid in a Chromium profile's `SingletonLock` (a symlink to
/// `HOST-PID`): the instance running on the profile, when a launch only
/// handed its URL to it and exited.
pub fn singleton_pid(lock_target: &str) -> Option<i32> {
    lock_target.rsplit_once('-').and_then(|(_, pid)| pid.parse().ok()).filter(|p| *p > 0)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn show(r: Result<Option<Found>, String>) -> String {
        match r {
            Ok(Some(f)) => format!("{}: {}", f.name, f.program),
            Ok(None) => "none".into(),
            Err(e) => format!("error: {e}"),
        }
    }

    #[test]
    fn modes() {
        let got: Vec<_> = [None, Some(""), Some("auto"), Some(" browser "), Some("webview"), Some("chrome")]
            .into_iter()
            .map(mode)
            .collect();
        assert_eq!(
            format!("{got:?}"),
            r#"[Ok(Auto), Ok(Auto), Ok(Auto), Ok(Browser), Ok(Webview), Err("PRIGH_DESKTOP_WINDOW is \"chrome\": use auto, browser or webview")]"#
        );
    }

    #[test]
    fn finds_on_linux() {
        let path = |installed: &'static [&'static str]| {
            move |name: &str| installed.contains(&name).then(|| format!("/usr/bin/{name}"))
        };
        let places = Places::default();
        let any = |_: &str| true;
        assert_eq!(show(find(Platform::Linux, None, &places, path(&[]), any)), "none");
        assert_eq!(
            show(find(Platform::Linux, None, &places, path(&["vivaldi", "brave"]), any)),
            "Brave: /usr/bin/brave"
        );
        assert_eq!(
            show(find(Platform::Linux, None, &places, path(&["chromium", "google-chrome"]), any)),
            "Google Chrome: /usr/bin/google-chrome"
        );
        assert_eq!(
            show(find(Platform::Linux, Some("chromium"), &places, path(&["chromium", "google-chrome"]), any)),
            "chromium: /usr/bin/chromium"
        );
        assert_eq!(
            show(find(Platform::Linux, Some("/opt/x/chrome"), &places, path(&[]), |p: &str| p == "/opt/x/chrome")),
            "/opt/x/chrome: /opt/x/chrome"
        );
        assert_eq!(
            show(find(Platform::Linux, Some("/nope"), &places, path(&["chromium"]), |_: &str| false)),
            "error: PRIGH_DESKTOP_BROWSER is \"/nope\", which is not an executable"
        );
    }

    #[test]
    fn finds_on_macos_and_windows() {
        let places = Places {
            home: Some("/Users/me".into()),
            windows_roots: vec![r"C:\Program Files".into(), r"C:\Users\me\AppData\Local".into()],
        };
        let none = |_: &str| None;
        assert_eq!(
            show(find(Platform::MacOs, None, &places, none, |p: &str| p
                == "/Users/me/Applications/Brave Browser.app/Contents/MacOS/Brave Browser")),
            "Brave: /Users/me/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"
        );
        assert_eq!(
            show(find(Platform::Windows, None, &places, none, |p: &str| p.ends_with("msedge.exe")
                || p == r"C:\Users\me\AppData\Local\Google\Chrome\Application\chrome.exe")),
            r"Google Chrome: C:\Users\me\AppData\Local\Google\Chrome\Application\chrome.exe"
        );
        assert_eq!(show(find(Platform::MacOs, None, &places, none, |_: &str| false)), "none");
    }

    #[test]
    fn chooses() {
        let f = || Some(Found { name: "Chromium".into(), program: "/usr/bin/chromium".into() });
        let got: Vec<String> = [
            (Mode::Auto, Platform::Linux, f()),
            (Mode::Auto, Platform::Linux, None),
            (Mode::Auto, Platform::MacOs, f()),
            (Mode::Auto, Platform::Windows, f()),
            (Mode::Browser, Platform::MacOs, f()),
            (Mode::Browser, Platform::Linux, None),
            (Mode::Webview, Platform::Linux, f()),
        ]
        .into_iter()
        .map(|(m, p, f)| format!("{m:?} {p:?}: {:?}", choose(m, p, f)))
        .collect();
        assert_eq!(
            got.join("\n"),
            "Auto Linux: Browser(Found { name: \"Chromium\", program: \"/usr/bin/chromium\" })\n\
             Auto Linux: Webview\n\
             Auto MacOs: Webview\n\
             Auto Windows: Webview\n\
             Browser MacOs: Browser(Found { name: \"Chromium\", program: \"/usr/bin/chromium\" })\n\
             Browser Linux: NoBrowser\n\
             Webview Linux: Webview"
        );
    }

    #[test]
    fn arguments() {
        assert_eq!(
            args(Platform::Linux, "/home/me/.local/share/prigh/browser", "http://127.0.0.1:7780/#desktop=k").join(" "),
            "--app=http://127.0.0.1:7780/#desktop=k --user-data-dir=/home/me/.local/share/prigh/browser --no-first-run \
             --no-default-browser-check --class=prigh --ozone-platform-hint=auto"
        );
        assert_eq!(
            args(Platform::Windows, r"C:\p", "https://ai.example.com/").join(" "),
            r"--app=https://ai.example.com/ --user-data-dir=C:\p --no-first-run --no-default-browser-check"
        );
    }

    #[test]
    fn shell_fragment() {
        assert_eq!(
            with_browser_shell("http://127.0.0.1:7780/#desktop=k"),
            "http://127.0.0.1:7780/#desktop=k&shell=browser"
        );
        assert_eq!(
            with_browser_shell("https://ai.example.com/#app=desktop&host=h"),
            "https://ai.example.com/#app=desktop&host=h&shell=browser"
        );
        assert_eq!(with_browser_shell("https://ai.example.com/"), "https://ai.example.com/#shell=browser");
        assert_eq!(with_browser_shell("https://ai.example.com/#"), "https://ai.example.com/#shell=browser");
    }

    #[test]
    fn singleton() {
        assert_eq!(singleton_pid("laptop-12345"), Some(12345));
        assert_eq!(singleton_pid("my-laptop-7"), Some(7));
        assert_eq!(singleton_pid("laptop"), None);
        assert_eq!(singleton_pid("laptop-0"), None);
    }
}
