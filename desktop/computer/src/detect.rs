//! Which backend drives this desktop. On Linux: a Wayland compositor with
//! wlroots' virtual-pointer and screencopy protocols (Hyprland, sway,
//! river, ...) is driven through them; otherwise an X11 session through
//! XTEST; other Wayland compositors (GNOME, KDE) through xdg-desktop-portal's
//! RemoteDesktop and ScreenCast. macOS and Windows need their own backends:
//! follow-ups.

use crate::backend::Backend;

/// What to do when the portal is missing.
pub const PORTAL_INSTALL: &str = "install xdg-desktop-portal with your desktop's backend (xdg-desktop-portal-gnome on \
                                  GNOME, xdg-desktop-portal-kde on KDE Plasma) and log in again";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    Fake,
    Wlroots,
    Portal,
    X11,
}

impl Kind {
    pub fn parse(s: &str) -> Result<Option<Kind>, String> {
        match s {
            "auto" => Ok(None),
            "fake" => Ok(Some(Kind::Fake)),
            "wlroots" => Ok(Some(Kind::Wlroots)),
            "portal" => Ok(Some(Kind::Portal)),
            "x11" => Ok(Some(Kind::X11)),
            other => Err(format!("--backend {other:?}: use auto, wlroots, portal, x11 or fake")),
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
    /// Whether xdg-desktop-portal's RemoteDesktop answers on the session bus
    /// (its version), asked only when the compositor lacks the wlroots
    /// protocols.
    pub portal: Option<Result<u32, String>>,
}

pub fn decide(env: &Environment) -> Result<Kind, String> {
    match env.os {
        "linux" | "freebsd" | "openbsd" | "netbsd" => {}
        "macos" => {
            return Err("computer use on macOS is not supported yet (it is planned: a follow-up adds a \
                        ScreenCaptureKit and CGEvent backend)"
                .into())
        }
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
                return match &env.portal {
                    Some(Ok(_)) => Ok(Kind::Portal),
                    Some(Err(e)) => Err(format!(
                        "{named} on Wayland lacks the wlroots virtual-pointer and screencopy protocols, and \
                         xdg-desktop-portal's RemoteDesktop (which GNOME, KDE and others offer instead) is missing: \
                         {e}. To fix it, {PORTAL_INSTALL}; or log in to an X11 session, or force X11 (only XWayland \
                         windows) with --backend x11"
                    )),
                    None => Err(format!(
                        "{named} on Wayland lacks the wlroots virtual-pointer and screencopy protocols that \
                         prigh-computer uses, and xdg-desktop-portal could not be asked: log in to an X11 session, or \
                         force X11 (only XWayland windows) with --backend x11"
                    )),
                };
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
        Kind::Portal => Ok(Box::new(crate::portal::open()?)),
        #[cfg(target_os = "linux")]
        Kind::X11 => Ok(Box::new(crate::x11::X11::connect()?)),
        #[cfg(not(target_os = "linux"))]
        Kind::Wlroots | Kind::Portal | Kind::X11 => {
            Err(decide(&current()).err().unwrap_or_else(|| "not supported here".into()))
        }
    }
}

pub fn current() -> Environment {
    let var = |k: &str| std::env::var(k).ok().filter(|v| !v.is_empty());
    let wayland_display = var("WAYLAND_DISPLAY");
    #[cfg(target_os = "linux")]
    let wlroots_globals = wayland_display.as_ref().map(|_| crate::wlroots::has_globals());
    #[cfg(not(target_os = "linux"))]
    let wlroots_globals = None;
    #[cfg(target_os = "linux")]
    let portal = matches!(wlroots_globals, Some(Ok(false)))
        .then(|| crate::portal::bus(None).and_then(|c| crate::portal::available(&c)));
    #[cfg(not(target_os = "linux"))]
    let portal = None;
    Environment {
        os: std::env::consts::OS,
        wayland_display,
        display: var("DISPLAY"),
        current_desktop: var("XDG_CURRENT_DESKTOP"),
        wlroots_globals,
        portal,
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
            portal: None,
        }
    }

    #[test]
    fn decides() {
        assert_eq!(decide(&env(true, true, Some("Hyprland"), Some(Ok(true)))), Ok(Kind::Wlroots));
        assert_eq!(decide(&env(true, false, Some("sway"), Some(Ok(true)))), Ok(Kind::Wlroots));
        assert_eq!(decide(&env(false, true, None, None)), Ok(Kind::X11));
        // GNOME on Wayland has XWayland's $DISPLAY, but X11 would see only its
        // windows: the portal drives it
        let gnome = |portal| Environment { portal, ..env(true, true, Some("GNOME"), Some(Ok(false))) };
        assert_eq!(decide(&gnome(Some(Ok(2)))), Ok(Kind::Portal));
        let missing = decide(&gnome(Some(Err("org.freedesktop.portal.Desktop was not provided".into())))).unwrap_err();
        assert_eq!(
            missing,
            "GNOME on Wayland lacks the wlroots virtual-pointer and screencopy protocols, and xdg-desktop-portal's \
             RemoteDesktop (which GNOME, KDE and others offer instead) is missing: org.freedesktop.portal.Desktop was \
             not provided. To fix it, install xdg-desktop-portal with your desktop's backend \
             (xdg-desktop-portal-gnome on GNOME, xdg-desktop-portal-kde on KDE Plasma) and log in again; or log in \
             to an X11 session, or force X11 (only XWayland windows) with --backend x11"
        );
        assert!(decide(&gnome(None)).unwrap_err().contains("could not be asked"));
        assert_eq!(Kind::parse("portal"), Ok(Some(Kind::Portal)));
        // a stale $WAYLAND_DISPLAY falls back to X11
        assert_eq!(decide(&env(true, true, None, Some(Err("refused".into())))), Ok(Kind::X11));
        assert!(decide(&env(true, false, None, Some(Err("refused".into())))).unwrap_err().contains("refused"));
        assert!(decide(&env(false, false, None, None)).unwrap_err().starts_with("no desktop to control"));
        let mac = Environment { os: "macos", ..Default::default() };
        assert!(decide(&mac).unwrap_err().contains("macOS is not supported yet"));
        let win = Environment { os: "windows", ..Default::default() };
        assert!(decide(&win).unwrap_err().contains("Windows is not supported yet"));
        assert_eq!(Kind::parse("auto"), Ok(None));
        assert_eq!(Kind::parse("x11"), Ok(Some(Kind::X11)));
        assert!(Kind::parse("mac").is_err());
    }
}
