//! The browser window's identity in the taskbar on Wayland. There a
//! Chromium-family browser ignores `--class` for an `--app` window and
//! names it after the site and the profile, e.g. `chrome-127.0.0.1__-Default`
//! (the port, query and fragment left out, the path's `/`s as `_`s); the
//! app writes a desktop entry by that name so that docks and taskbars show
//! prigh's name and icon for it.

/// The app_id the browser `program` gives an `--app={url}` window on
/// prigh's profile (its `Default` profile directory).
pub fn app_id(program: &str, url: &str) -> Option<String> {
    let rest = url.split_once("://")?.1;
    let rest = rest.split(['#', '?']).next().unwrap_or(rest);
    let (authority, path) = rest.split_once('/').map_or((rest, ""), |(a, p)| (a, p));
    let authority = authority.rsplit('@').next().unwrap_or(authority);
    let host = match authority.strip_prefix('[') {
        Some(v6) => v6.split(']').next().unwrap_or(v6),
        None => authority.split(':').next().unwrap_or(authority),
    };
    if host.is_empty() {
        return None;
    }
    let base = program.rsplit(['/', '\\']).next().unwrap_or(program).to_lowercase();
    let prefix = if base.contains("brave") {
        "brave"
    } else if base.contains("edge") {
        "msedge"
    } else if base.contains("vivaldi") {
        "vivaldi"
    } else {
        "chrome"
    };
    Some(format!("{prefix}-{host}__{}-Default", path.replace('/', "_")))
}

/// The desktop entry for `app_id`: prigh's name and icon, launching the app
/// again. Not `NoDisplay`: some taskbars (Noctalia's) skip hidden entries
/// when they match a window to an app, then take a Chromium window for the
/// browser itself.
pub fn desktop_entry(app_id: &str, exec: &str) -> String {
    let exec = if exec.contains([' ', '"', '\\', '\'']) {
        format!("\"{}\"", exec.replace('\\', "\\\\").replace('"', "\\\""))
    } else {
        exec.to_string()
    };
    format!(
        "[Desktop Entry]\nType=Application\nName=prigh\nComment=prigh's window in the browser\nExec={exec}\n\
         Icon=prigh-desktop\nTerminal=false\nStartupWMClass={app_id}\n"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn app_ids() {
        let chromium = "/usr/bin/chromium";
        let local = "http://127.0.0.1:7780/#desktop=k&shell=browser";
        assert_eq!(app_id(chromium, local).as_deref(), Some("chrome-127.0.0.1__-Default"));
        assert_eq!(app_id("google-chrome-stable", local).as_deref(), Some("chrome-127.0.0.1__-Default"));
        assert_eq!(
            app_id("/opt/brave.com/brave/brave-browser", "https://ai.example.com/#shell=browser").as_deref(),
            Some("brave-ai.example.com__-Default")
        );
        assert_eq!(
            app_id("microsoft-edge-stable", "https://ai.example.com:8443/prigh/web?x=1").as_deref(),
            Some("msedge-ai.example.com__prigh_web-Default")
        );
        assert_eq!(app_id("vivaldi", "http://[::1]:7780/").as_deref(), Some("vivaldi-::1__-Default"));
        assert_eq!(app_id(chromium, "http://u@host").as_deref(), Some("chrome-host__-Default"));
        assert_eq!(app_id(chromium, "not a url"), None);
    }

    #[test]
    fn desktop_entries() {
        assert_eq!(
            desktop_entry("chrome-127.0.0.1__-Default", "/nix/store/x-prigh-desktop/bin/prigh-desktop"),
            "[Desktop Entry]\nType=Application\nName=prigh\nComment=prigh's window in the browser\n\
             Exec=/nix/store/x-prigh-desktop/bin/prigh-desktop\nIcon=prigh-desktop\nTerminal=false\n\
             StartupWMClass=chrome-127.0.0.1__-Default\n"
        );
        assert!(desktop_entry("a", "/opt/my apps/prigh-desktop").contains("\nExec=\"/opt/my apps/prigh-desktop\"\n"));
    }
}
