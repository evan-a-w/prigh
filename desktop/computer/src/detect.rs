//! Which backend drives this desktop. On Linux: a Wayland compositor with
//! wlroots' virtual-pointer and screencopy protocols (Hyprland, sway,
//! river, ...) is driven through them; otherwise an X11 session through
//! XTEST. On macOS: ScreenCaptureKit and Quartz events. GNOME and KDE on
//! Wayland need xdg-desktop-portal (RemoteDesktop and ScreenCast), and
//! Windows its own backend: follow-ups.

use crate::backend::Backend;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    Fake,
    Wlroots,
    X11,
    Macos,
}

impl Kind {
    pub fn parse(s: &str) -> Result<Option<Kind>, String> {
        match s {
            "auto" => Ok(None),
            "fake" => Ok(Some(Kind::Fake)),
            "wlroots" => Ok(Some(Kind::Wlroots)),
            "x11" => Ok(Some(Kind::X11)),
            "macos" => Ok(Some(Kind::Macos)),
            other => Err(format!("--backend {other:?}: use auto, wlroots, x11, macos or fake")),
        }
    }
}

/// What detection looks at.
#[derive(Debug, Clone, Default)]
pub struct Environment {
    pub os: &'static str,
    pub wayland_display: Option<String>,
    pub display: Option<String>,
    pub current_desktop: Option<String>,
    /// Whether the Wayland compositor offers wlroots' virtual pointer and
    /// screencopy; `None` when it could not be asked (no compositor).
    pub wlroots_globals: Option<Result<bool, String>>,
}

pub fn decide(env: &Environment) -> Result<Kind, String> {
    match env.os {
        "linux" | "freebsd" | "openbsd" | "netbsd" => {}
        "macos" => return Ok(Kind::Macos),
        "windows" => {
            return Err(
                "computer use on Windows is not supported yet (it is planned: a follow-up adds a Windows backend)"
                    .into(),
            )
        }
        os => return Err(format!("computer use is not supported on {os}")),
    }
    let desktop = env.current_desktop.clone().unwrap_or_default();
    if env.wayland_display.is_some() {
        match &env.wlroots_globals {
            Some(Ok(true)) => return Ok(Kind::Wlroots),
            Some(Ok(false)) => {
                let named = if desktop.is_empty() { "this compositor".to_string() } else { desktop.clone() };
                return Err(format!(
                    "{named} on Wayland lacks the wlroots virtual-pointer and screencopy protocols that prigh-computer \
                     uses (Hyprland, sway and other wlroots compositors have them). GNOME and KDE need \
                     xdg-desktop-portal's RemoteDesktop, which a follow-up adds; meanwhile log in to an X11 session, \
                     or force X11 (only XWayland windows) with --backend x11"
                ));
            }
            Some(Err(e)) if env.display.is_none() => {
                return Err(format!(
                    "cannot reach the Wayland compositor at $WAYLAND_DISPLAY ({e}), and there is no $DISPLAY"
                ));
            }
            Some(Err(_)) | None => {}
        }
    }
    if env.display.is_some() {
        return Ok(Kind::X11);
    }
    Err("no desktop to control: neither $WAYLAND_DISPLAY nor $DISPLAY is set (run the tool host inside the graphical \
         session, or pass those variables to it)"
        .into())
}

pub struct Options {
    pub fake_size: (u32, u32),
}

pub fn open(kind: Option<Kind>, options: &Options) -> Result<Box<dyn Backend>, String> {
    let kind = match kind {
        Some(k) => k,
        None => decide(&current())?,
    };
    match kind {
        Kind::Fake => Ok(Box::new(crate::fake::Fake::new(options.fake_size.0, options.fake_size.1))),
        #[cfg(target_os = "linux")]
        Kind::Wlroots => Ok(Box::new(crate::wlroots::Wlroots::connect()?)),
        #[cfg(target_os = "linux")]
        Kind::X11 => Ok(Box::new(crate::x11::X11::connect()?)),
        #[cfg(target_os = "macos")]
        Kind::Macos => Ok(Box::new(crate::macos::Macos::connect()?)),
        #[cfg(not(target_os = "linux"))]
        Kind::Wlroots | Kind::X11 => Err(unavailable(kind)),
        #[cfg(not(target_os = "macos"))]
        Kind::Macos => Err(unavailable(kind)),
    }
}

/// A backend this build lacks, asked for with --backend.
#[allow(dead_code)]
fn unavailable(kind: Kind) -> String {
    let (name, os) = match kind {
        Kind::Wlroots => ("wlroots", "Linux"),
        Kind::X11 => ("x11", "Linux"),
        Kind::Macos => ("macos", "macOS"),
        Kind::Fake => ("fake", "every system"),
    };
    format!("--backend {name} works only on {os}, not on {}", std::env::consts::OS)
}

pub fn current() -> Environment {
    let var = |k: &str| std::env::var(k).ok().filter(|v| !v.is_empty());
    let wayland_display = var("WAYLAND_DISPLAY");
    #[cfg(target_os = "linux")]
    let wlroots_globals = wayland_display.as_ref().map(|_| crate::wlroots::has_globals());
    #[cfg(not(target_os = "linux"))]
    let wlroots_globals = None;
    Environment {
        os: std::env::consts::OS,
        wayland_display,
        display: var("DISPLAY"),
        current_desktop: var("XDG_CURRENT_DESKTOP"),
        wlroots_globals,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn env(wayland: bool, display: bool, desktop: Option<&str>, globals: Option<Result<bool, String>>) -> Environment {
        Environment {
            os: "linux",
            wayland_display: wayland.then(|| "wayland-1".into()),
            display: display.then(|| ":0".into()),
            current_desktop: desktop.map(str::to_string),
            wlroots_globals: globals,
        }
    }

    #[test]
    fn decides() {
        assert_eq!(decide(&env(true, true, Some("Hyprland"), Some(Ok(true)))), Ok(Kind::Wlroots));
        assert_eq!(decide(&env(true, false, Some("sway"), Some(Ok(true)))), Ok(Kind::Wlroots));
        assert_eq!(decide(&env(false, true, None, None)), Ok(Kind::X11));
        // GNOME on Wayland has XWayland's $DISPLAY, but X11 would see only its windows
        let gnome = decide(&env(true, true, Some("GNOME"), Some(Ok(false)))).unwrap_err();
        assert!(gnome.starts_with("GNOME on Wayland lacks"), "{gnome}");
        assert!(gnome.contains("xdg-desktop-portal"));
        // a stale $WAYLAND_DISPLAY falls back to X11
        assert_eq!(decide(&env(true, true, None, Some(Err("refused".into())))), Ok(Kind::X11));
        assert!(decide(&env(true, false, None, Some(Err("refused".into())))).unwrap_err().contains("refused"));
        assert!(decide(&env(false, false, None, None)).unwrap_err().starts_with("no desktop to control"));
        let mac = Environment { os: "macos", ..Default::default() };
        assert_eq!(decide(&mac), Ok(Kind::Macos));
        let win = Environment { os: "windows", ..Default::default() };
        assert!(decide(&win).unwrap_err().contains("Windows is not supported yet"));
        assert_eq!(Kind::parse("auto"), Ok(None));
        assert_eq!(Kind::parse("x11"), Ok(Some(Kind::X11)));
        assert_eq!(Kind::parse("macos"), Ok(Some(Kind::Macos)));
        assert!(Kind::parse("mac").is_err());
    }
}
