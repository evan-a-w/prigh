//! Where the window may go. It shows the shell's loading page, then the
//! sidecar's site and, under `prigh desktop -ui remote`, the backend's own
//! site; web links elsewhere open in the system browser, and anything else
//! (file:, javascript:, custom schemes) is refused. A page asks to quit the
//! app by going to [QUIT_URL] (the backend's site cannot reach the sidecar).

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
}
