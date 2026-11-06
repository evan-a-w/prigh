//! Where the window may go. It shows the shell's loading page, then the
//! sidecar's site and, under `prigh desktop -ui remote`, the backend's own
//! site; web links elsewhere open in the system browser, and anything else
//! (file:, javascript:, custom schemes) is refused. A page asks to quit the
//! app by going to [QUIT_URL] (the backend's site cannot reach the sidecar).

use std::collections::HashMap;
use std::time::{Duration, Instant};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Decision {
    Allow,
    /// Open it in the system browser instead.
    External,
    Block,
    /// Stop the sidecar, which ends the app.
    Quit,
}

pub const QUIT_URL: &str = "about:blank#prigh-desktop-quit";

/// Whatever a site does, a window cannot open it in the default browser
/// tab after tab: at most [OPEN_BURST] times within this.
pub const OPEN_GAP: Duration = Duration::from_secs(5);

/// How many times one window may open one site in the default browser
/// within [OPEN_GAP] (a link that opens a window and then another).
pub const OPEN_BURST: usize = 2;

/// When each window last opened each site in the default browser.
#[derive(Default)]
pub struct OpenGuard {
    /// The times within the last [OPEN_GAP], and whether refusing was said.
    opened: HashMap<(String, String), (Vec<Instant>, bool)>,
}

impl OpenGuard {
    /// Whether `window` may open `url` now; if not, what to log (once until
    /// it may again).
    pub fn check(&mut self, window: &str, url: &str, now: Instant) -> Result<(), Option<String>> {
        let site = origin(url).unwrap_or_else(|| url.to_string());
        let (times, told) = self.opened.entry((window.to_string(), site.clone())).or_default();
        times.retain(|at| now.saturating_duration_since(*at) < OPEN_GAP);
        if times.len() < OPEN_BURST {
            times.push(now);
            *told = false;
            Ok(())
        } else if std::mem::replace(told, true) {
            Err(None)
        } else {
            Err(Some(format!(
                "{site} again within {}s: not opening it in the default browser again",
                OPEN_GAP.as_secs()
            )))
        }
    }

    /// The window closed.
    pub fn forget(&mut self, window: &str) {
        self.opened.retain(|(w, _), _| w != window);
    }
}

/// The backend's own site, as the sidecar last said to show it: its page,
/// the sign-in proxy's cookie for it (from signing in through the default
/// browser), and the sidecar's page that offers to sign in.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Remote {
    pub url: String,
    pub cookies: Vec<(String, String)>,
    pub signin_page: Option<String>,
}

/// The pairs of a `Cookie` header (`a=b; c=d`).
pub fn cookie_pairs(header: &str) -> Vec<(String, String)> {
    header
        .split(';')
        .filter_map(|pair| {
            let (name, value) = pair.split_once('=')?;
            let name = name.trim();
            (!name.is_empty()).then(|| (name.to_string(), value.trim().to_string()))
        })
        .collect()
}

/// What the app's own window does about a navigation.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Step {
    Allow,
    /// Refuse it, and open it in the system browser.
    Open(String),
    /// Refuse it (and log this, if anything).
    Refuse(Option<String>),
    Quit,
    /// Refuse it: give the window the site's cookies, then load `url`.
    Cookies {
        url: String,
    },
    /// Refuse it: show the sidecar's sign-in page.
    SignIn {
        url: String,
    },
    /// Allow it, and log this: the window signs in there.
    Away(String),
}

/// One of the app's own windows (the system webview), which tells a link
/// to another site from a sign-in proxy's redirect by what came before:
/// the window cannot see why it navigates. A navigation elsewhere while a
/// navigation to prigh's site has not loaded is that site's redirect (a
/// sign-in); from a page of prigh's that has loaded, it is a link (or the
/// page's script). See [crate::devtools] for the browser's window.
#[derive(Default)]
pub struct Window {
    /// prigh's page being loaded: redirected away, it is signing in.
    pending: Option<String>,
    /// Signing in on this site, until it is back on prigh's.
    away: Option<String>,
    /// The user chose to sign in in the window.
    here: bool,
    /// The backend's site's cookies it was given: given again only when
    /// they are new (a sign-in since), not whenever the site loads (a
    /// refused load may say it finished), so a site that does not take
    /// them sends the window to prigh's sign-in page instead of round and
    /// round.
    cookies_given: Option<Vec<(String, String)>>,
    guard: OpenGuard,
}

impl Window {
    /// The user chose to sign in in the window (the sidecar's page said).
    pub fn sign_in_here(&mut self) {
        self.here = true;
    }

    /// The site's cookies are the window's now.
    pub fn cookies_given(&mut self, cookies: &[(String, String)]) {
        self.cookies_given = Some(cookies.to_vec());
    }

    /// A navigation to `url` at `now`; `ours`: the origins of prigh's sites.
    pub fn on_navigation(&mut self, url: &str, ours: &[String], remote: Option<&Remote>, now: Instant) -> Step {
        match decide(url, ours) {
            Decision::Quit => Step::Quit,
            Decision::Block => Step::Refuse(Some(format!("refused to navigate to {url}"))),
            Decision::Allow => {
                if origin(url).is_some_and(|o| ours.contains(&o)) {
                    self.pending = Some(url.to_string());
                }
                Step::Allow
            }
            Decision::External if self.away.is_some() => Step::Allow,
            Decision::External => {
                let site = origin(url).unwrap_or_else(|| url.to_string());
                let Some(pending) = self.pending.clone() else {
                    return match self.guard.check("", url, now) {
                        Ok(()) => Step::Open(url.to_string()),
                        Err(log) => Step::Refuse(log),
                    };
                };
                let from_remote = remote.filter(|r| origin(&pending).is_some() && origin(&pending) == origin(&r.url));
                match from_remote {
                    Some(r)
                        if !self.here
                            && self.cookies_given.as_deref() != Some(&r.cookies[..])
                            && !r.cookies.is_empty() =>
                    {
                        self.cookies_given = Some(r.cookies.clone());
                        Step::Cookies { url: pending }
                    }
                    Some(Remote { signin_page: Some(page), .. }) if !self.here => {
                        self.pending = None;
                        Step::SignIn { url: page.clone() }
                    }
                    _ => {
                        self.away = Some(site.clone());
                        Step::Away(format!(
                            "the window went to {site} on its own (a sign-in?): it stays there until it is back"
                        ))
                    }
                }
            }
        }
    }

    /// A page finished loading; what to log.
    pub fn on_loaded(&mut self, url: &str, ours: &[String], remote: Option<&Remote>) -> Option<String> {
        self.pending = None;
        let o = origin(url)?;
        if !ours.contains(&o) {
            return None;
        }
        if remote.is_some_and(|r| origin(&r.url).as_ref() == Some(&o)) {
            self.here = false;
        }
        self.away.take().map(|away| format!("the window is back from {away}"))
    }
}

/// `scheme://host[:port]`, lowercased, with the scheme's default port
/// dropped; None for URLs without an authority.
pub fn origin(url: &str) -> Option<String> {
    let (scheme, rest) = url.split_once("://")?;
    let scheme = scheme.to_ascii_lowercase();
    if scheme.is_empty() || !scheme.chars().all(|c| c.is_ascii_alphanumeric() || "+-.".contains(c)) {
        return None;
    }
    let authority = rest.split(['/', '?', '#']).next().unwrap_or("");
    let host_port = authority.rsplit_once('@').map_or(authority, |(_, h)| h).to_ascii_lowercase();
    if host_port.is_empty() {
        return None;
    }
    let default_port = match scheme.as_str() {
        "http" | "ws" => Some(":80"),
        "https" | "wss" => Some(":443"),
        _ => None,
    };
    let host_port = match default_port {
        Some(p) => host_port.strip_suffix(p).map(str::to_string).unwrap_or(host_port),
        None => host_port,
    };
    Some(format!("{scheme}://{host_port}"))
}

/// The loading page the window starts on, served by Tauri itself
/// (`tauri://localhost` on Linux and macOS, `http(s)://tauri.localhost` on
/// Windows).
fn is_shell_page(url: &str) -> bool {
    matches!(origin(url).as_deref(), Some("tauri://localhost" | "http://tauri.localhost" | "https://tauri.localhost"))
}

/// `ours`: the origins of the sites the sidecar has said to show.
pub fn decide(url: &str, ours: &[String]) -> Decision {
    let lower = url.to_ascii_lowercase();
    if lower == QUIT_URL {
        return Decision::Quit;
    }
    if is_shell_page(url)
        || lower == "about:blank"
        || lower.starts_with("about:srcdoc")
        || lower.starts_with("blob:")
        || lower.starts_with("data:")
    {
        return Decision::Allow;
    }
    if origin(url).is_some_and(|o| ours.contains(&o)) {
        return Decision::Allow;
    }
    if lower.starts_with("http://") || lower.starts_with("https://") || lower.starts_with("mailto:") {
        Decision::External
    } else {
        Decision::Block
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Decision::*;

    #[test]
    fn origins() {
        assert_eq!(origin("http://127.0.0.1:7790/a?b#c").as_deref(), Some("http://127.0.0.1:7790"));
        assert_eq!(origin("HTTP://LocalHost:80/").as_deref(), Some("http://localhost"));
        assert_eq!(origin("https://u:p@Example.com:443").as_deref(), Some("https://example.com"));
        assert_eq!(origin("https://example.com:8443#x").as_deref(), Some("https://example.com:8443"));
        assert_eq!(origin("tauri://localhost/index.html").as_deref(), Some("tauri://localhost"));
        assert_eq!(origin("mailto:a@b"), None);
        assert_eq!(origin("file:///etc"), None);
    }

    #[test]
    fn decisions() {
        let site = &["http://127.0.0.1:7790".to_string()][..];
        let none: &[String] = &[];
        assert_eq!(decide("tauri://localhost/index.html", none), Allow);
        assert_eq!(decide("http://tauri.localhost/index.html", none), Allow);
        assert_eq!(decide("http://127.0.0.1:7790/#signin=x", site), Allow);
        assert_eq!(decide("http://127.0.0.1:7790/#desktop=k3y", site), Allow);
        assert_eq!(decide("http://127.0.0.1:7790/?session=abc#desktop=k3y", site), Allow);
        assert_eq!(decide("http://127.0.0.1:7790/files/a.txt", site), Allow);
        // before the sidecar has said where it is, nothing of it is allowed
        assert_eq!(decide("http://127.0.0.1:7790/", none), External);
        assert_eq!(decide("http://127.0.0.1:7791/", site), External);
        assert_eq!(decide("https://github.com/evan-a-w/prigh/pull/1", site), External);
        assert_eq!(decide("mailto:me@example.com", site), External);
        assert_eq!(decide("blob:http://127.0.0.1:7790/uuid", site), Allow);
        assert_eq!(decide("about:blank", site), Allow);
        assert_eq!(decide("file:///etc/passwd", site), Block);
        assert_eq!(decide("javascript:alert(1)", site), Block);
        assert_eq!(decide("vscode://file/x", site), Block);
        // the backend's own site too, once the sidecar has said to show it
        let both = &["http://127.0.0.1:7790".to_string(), "https://ai.example.com".to_string()][..];
        assert_eq!(decide("https://ai.example.com/#app=desktop&host=laptop", site), External);
        assert_eq!(decide("https://ai.example.com/#app=desktop&host=laptop", both), Allow);
        assert_eq!(decide("https://ai.example.com:443/files/x", both), Allow);
        assert_eq!(decide("http://ai.example.com/", both), External);
        assert_eq!(decide("https://evil.example/", both), External);
        // quitting, from any of them
        assert_eq!(decide(QUIT_URL, both), Quit);
        assert_eq!(decide("about:blank#prigh-desktop-quit", none), Quit);
        assert_eq!(decide("about:blank#prigh-desktop-quitx", both), Block);
    }

    fn at(secs: u64) -> Instant {
        static BASE: std::sync::OnceLock<Instant> = std::sync::OnceLock::new();
        *BASE.get_or_init(Instant::now) + Duration::from_secs(secs)
    }

    #[test]
    fn cookie_headers() {
        assert_eq!(
            cookie_pairs("authentik_proxy_x=a.b=c; other = y ;; =z; bare"),
            [("authentik_proxy_x".to_string(), "a.b=c".to_string()), ("other".to_string(), "y".to_string())]
        );
    }

    const LOCAL: &str = "http://127.0.0.1:7780/#desktop=k";
    const SITE: &str = "https://ai.example.com/#app=desktop";

    fn ours() -> Vec<String> {
        vec!["http://127.0.0.1:7780".to_string(), "https://ai.example.com".to_string()]
    }

    fn remote(cookies: bool) -> Remote {
        Remote {
            url: SITE.to_string(),
            cookies: if cookies { vec![("c".to_string(), "v".to_string())] } else { vec![] },
            signin_page: Some(format!("{LOCAL}&signin=1")),
        }
    }

    /// Navigations (`>URL`) and loads (`=URL`) in the window, and what it
    /// does about each.
    fn walk(w: &mut Window, remote: Option<&Remote>, steps: &[(&str, u64)]) -> Vec<String> {
        steps
            .iter()
            .map(|(step, secs)| match step.split_at(1) {
                (">", url) => format!("{url}: {:?}", w.on_navigation(url, &ours(), remote, at(*secs))),
                (_, url) => format!("loaded {url}: {:?}", w.on_loaded(url, &ours(), remote)),
            })
            .collect()
    }

    #[test]
    fn the_window_signs_in_to_a_proxy_in_it_and_links_go_out() {
        let mut w = Window::default();
        let out = walk(
            &mut w,
            None,
            &[
                (">tauri://localhost/index.html", 0),
                ("=tauri://localhost/index.html", 0),
                // The site, redirected through the proxy and back.
                (">https://ai.example.com/#app=desktop", 0),
                (">https://login.example.com/start?state=1", 0),
                (">https://idp.example/auth", 0),
                ("=https://idp.example/auth", 0),
                (">https://login.example.com/callback", 0),
                (">https://ai.example.com/", 0),
                ("=https://ai.example.com/", 0),
                // A link (or a script) from the loaded page.
                (">https://github.com/evan-a-w/prigh", 1),
                (">https://github.com/evan-a-w/prigh/pulls", 2),
                (">https://github.com/x", 3),
                (">https://github.com/z", 4),
                (">https://github.com/y", 7),
                (">mailto:a@b", 7),
                (">file:///etc/passwd", 7),
                (">about:blank#prigh-desktop-quit", 7),
            ],
        );
        assert_eq!(
            out,
            [
                "tauri://localhost/index.html: Allow",
                "loaded tauri://localhost/index.html: None",
                "https://ai.example.com/#app=desktop: Allow",
                "https://login.example.com/start?state=1: Away(\"the window went to https://login.example.com on its own (a sign-in?): it stays there until it is back\")",
                "https://idp.example/auth: Allow",
                "loaded https://idp.example/auth: None",
                "https://login.example.com/callback: Allow",
                "https://ai.example.com/: Allow",
                "loaded https://ai.example.com/: Some(\"the window is back from https://login.example.com\")",
                "https://github.com/evan-a-w/prigh: Open(\"https://github.com/evan-a-w/prigh\")",
                "https://github.com/evan-a-w/prigh/pulls: Open(\"https://github.com/evan-a-w/prigh/pulls\")",
                "https://github.com/x: Refuse(Some(\"https://github.com again within 5s: not opening it in the default browser again\"))",
                "https://github.com/z: Refuse(None)",
                "https://github.com/y: Open(\"https://github.com/y\")",
                "mailto:a@b: Open(\"mailto:a@b\")",
                "file:///etc/passwd: Refuse(Some(\"refused to navigate to file:///etc/passwd\"))",
                "about:blank#prigh-desktop-quit: Quit",
            ]
        );
    }

    #[test]
    fn a_site_that_keeps_redirecting_has_a_bounded_number_of_effects() {
        let mut w = Window::default();
        let mut opens = 0;
        let mut aways = 0;
        // The old loop: the site redirects to a fresh login address each
        // time it is loaded.
        for i in 0..100 {
            for step in [
                w.on_navigation(SITE, &ours(), None, at(i)),
                w.on_navigation(&format!("https://login.example.com/?state={i}"), &ours(), None, at(i)),
            ] {
                match step {
                    Step::Open(_) => opens += 1,
                    Step::Away(_) => aways += 1,
                    _ => {}
                }
            }
        }
        assert_eq!((opens, aways), (0, 1));
    }

    #[test]
    fn the_backends_site_wants_a_sign_in_the_window_gets_the_cookie_or_prighs_page() {
        let mut w = Window::default();
        let r = remote(true);
        let out = walk(
            &mut w,
            Some(&r),
            &[
                (">https://ai.example.com/#app=desktop", 0),
                (">https://login.example.com/start?state=1", 0),
                // The shell gave it the cookie: again.
                (">https://ai.example.com/#app=desktop", 0),
                (">https://login.example.com/start?state=2", 0),
                // Not good enough: prigh's sign-in page.
                (">http://127.0.0.1:7780/#desktop=k&signin=1", 0),
                ("=http://127.0.0.1:7780/#desktop=k&signin=1", 0),
            ],
        );
        assert_eq!(
            out,
            [
                "https://ai.example.com/#app=desktop: Allow",
                "https://login.example.com/start?state=1: Cookies { url: \"https://ai.example.com/#app=desktop\" }",
                "https://ai.example.com/#app=desktop: Allow",
                "https://login.example.com/start?state=2: SignIn { url: \"http://127.0.0.1:7780/#desktop=k&signin=1\" }",
                "http://127.0.0.1:7780/#desktop=k&signin=1: Allow",
                "loaded http://127.0.0.1:7780/#desktop=k&signin=1: None",
            ]
        );
        // Sign in here: the shell says so, and sends it to the site.
        w.sign_in_here();
        let out = walk(
            &mut w,
            Some(&r),
            &[
                (">https://ai.example.com/#app=desktop", 0),
                (">https://login.example.com/start?state=3", 0),
                ("=https://login.example.com/flow", 0),
                (">https://ai.example.com/", 0),
                ("=https://ai.example.com/", 0),
                // Expired later: the cookie it was given, so the page.
                (">https://ai.example.com/", 0),
                (">https://login.example.com/start?state=4", 0),
                (">https://ai.example.com/", 0),
                (">https://login.example.com/start?state=5", 0),
            ],
        );
        assert_eq!(
            out,
            [
                "https://ai.example.com/#app=desktop: Allow",
                "https://login.example.com/start?state=3: Away(\"the window went to https://login.example.com on its own (a sign-in?): it stays there until it is back\")",
                "loaded https://login.example.com/flow: None",
                "https://ai.example.com/: Allow",
                "loaded https://ai.example.com/: Some(\"the window is back from https://login.example.com\")",
                "https://ai.example.com/: Allow",
                "https://login.example.com/start?state=4: SignIn { url: \"http://127.0.0.1:7780/#desktop=k&signin=1\" }",
                "https://ai.example.com/: Allow",
                "https://login.example.com/start?state=5: SignIn { url: \"http://127.0.0.1:7780/#desktop=k&signin=1\" }",
            ]
        );
        // Without a cookie, straight to prigh's page.
        let mut w = Window::default();
        let r = remote(false);
        let out = walk(&mut w, Some(&r), &[(">https://ai.example.com/", 0), (">https://login.example.com/", 0)]);
        assert_eq!(out[1], "https://login.example.com/: SignIn { url: \"http://127.0.0.1:7780/#desktop=k&signin=1\" }");
    }
}
