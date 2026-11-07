//! How the sidecar is started: `<prigh> desktop [extra args]`, the prigh
//! from `$PRIGH_DESKTOP_BACKEND` or the PATH; on Windows, where prigh does
//! not run natively, inside WSL through a login shell (for the PATH the
//! user's profile sets up, e.g. Nix's).
//!
//! In WSL the tool host cannot see or drive the Windows desktop, so for
//! computer use the app hands it the Windows helper bundled next to it
//! (`prigh-computer.exe`): `$PRIGH_COMPUTER`, shared into WSL through
//! `$WSLENV` with `/p` (its path translated, `/mnt/c/...`). The tool host
//! runs it there as any program, through WSL's interop, talking to it
//! over the same pipes.

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Os {
    Unix,
    Windows,
}

#[derive(Debug, Default)]
pub struct Env {
    /// `$PRIGH_DESKTOP_BACKEND`: a path or a command name (on Windows, in
    /// WSL).
    pub backend: Option<String>,
    /// `$PRIGH_DESKTOP_ARGS`, see [crate::args].
    pub args: Option<String>,
    /// `$PRIGH_DESKTOP_WSL_DISTRO`: the WSL distribution (default: WSL's
    /// default one).
    pub wsl_distro: Option<String>,
    /// The Windows `prigh-computer.exe` bundled with the app, when it is
    /// there.
    pub bundled_computer: Option<String>,
    /// `$PRIGH_COMPUTER`: the user's choice wins over the bundled helper.
    pub computer: Option<String>,
    /// `$WSLENV`, which the shared variables join.
    pub wslenv: Option<String>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Launch {
    pub program: String,
    pub args: Vec<String>,
    /// Variables to set for it.
    pub env: Vec<(String, String)>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    NotFound { name: String },
    BadArgs(String),
}

pub fn plan(os: Os, env: &Env, find: impl Fn(&str) -> Option<String>) -> Result<Launch, Error> {
    let extra = match env.args.as_deref().filter(|a| !a.trim().is_empty()) {
        None => Vec::new(),
        Some(a) => crate::args::split(a).map_err(|e| Error::BadArgs(format!("$PRIGH_DESKTOP_ARGS: {}", e.0)))?,
    };
    let backend = env.backend.as_deref().filter(|b| !b.is_empty());
    match os {
        Os::Unix => {
            let name = backend.unwrap_or("prigh");
            let program = if name.contains('/') {
                name.to_string()
            } else {
                find(name).ok_or_else(|| Error::NotFound { name: name.to_string() })?
            };
            let mut args = vec!["desktop".to_string()];
            args.extend(extra);
            Ok(Launch { program, args, env: Vec::new() })
        }
        Os::Windows => {
            let mut args = Vec::new();
            if let Some(d) = env.wsl_distro.as_deref().filter(|d| !d.is_empty()) {
                args.extend(["-d".to_string(), d.to_string()]);
            }
            args.extend(
                ["--cd", "~", "-e", "bash", "-lc", "exec \"$0\" desktop \"$@\"", backend.unwrap_or("prigh")]
                    .map(String::from),
            );
            args.extend(extra);
            Ok(Launch { program: "wsl.exe".to_string(), args, env: wsl_env(env) })
        }
    }
}

/// The bundled helper as `$PRIGH_COMPUTER` in WSL, unless the user set
/// their own.
fn wsl_env(env: &Env) -> Vec<(String, String)> {
    let set = |v: &Option<String>| v.as_deref().filter(|v| !v.is_empty()).map(str::to_string);
    match (set(&env.computer), set(&env.bundled_computer)) {
        (None, Some(helper)) => {
            let shared = match set(&env.wslenv) {
                Some(w) if w.split(':').any(|v| v.split('/').next() == Some("PRIGH_COMPUTER")) => w,
                Some(w) => format!("{w}:PRIGH_COMPUTER/p"),
                None => "PRIGH_COMPUTER/p".to_string(),
            };
            vec![("PRIGH_COMPUTER".into(), helper), ("WSLENV".into(), shared)]
        }
        _ => Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn find(name: &str) -> Option<String> {
        (name == "prigh").then(|| "/nix/profile/bin/prigh".to_string())
    }

    #[test]
    fn unix() {
        assert_eq!(
            plan(Os::Unix, &Env::default(), find),
            Ok(Launch { program: "/nix/profile/bin/prigh".into(), args: vec!["desktop".into()], env: Vec::new() })
        );
        let env = Env {
            backend: Some("/store/prigh/bin/prigh".into()),
            args: Some("-cwd '/my proj' -tools remote".into()),
            ..Env::default()
        };
        assert_eq!(
            plan(Os::Unix, &env, find),
            Ok(Launch {
                program: "/store/prigh/bin/prigh".into(),
                args: ["desktop", "-cwd", "/my proj", "-tools", "remote"].map(String::from).to_vec(),
                env: Vec::new(),
            })
        );
        let env = Env { backend: Some("prigh-dev".into()), ..Env::default() };
        assert_eq!(plan(Os::Unix, &env, find), Err(Error::NotFound { name: "prigh-dev".into() }));
        assert_eq!(plan(Os::Unix, &Env::default(), |_| None), Err(Error::NotFound { name: "prigh".into() }));
        let env = Env { args: Some("-cwd 'x".into()), ..Env::default() };
        assert_eq!(
            plan(Os::Unix, &env, find),
            Err(Error::BadArgs("$PRIGH_DESKTOP_ARGS: unterminated ' in \"-cwd 'x\"".into()))
        );
    }

    #[test]
    fn windows() {
        assert_eq!(
            plan(Os::Windows, &Env::default(), |_| None),
            Ok(Launch {
                program: "wsl.exe".into(),
                args: ["--cd", "~", "-e", "bash", "-lc", "exec \"$0\" desktop \"$@\"", "prigh"]
                    .map(String::from)
                    .to_vec(),
                env: Vec::new()
            })
        );
        let env = Env {
            backend: Some("/home/me/.nix-profile/bin/prigh".into()),
            args: Some("-name laptop".into()),
            wsl_distro: Some("NixOS".into()),
            ..Env::default()
        };
        assert_eq!(
            plan(Os::Windows, &env, |_| None).unwrap().args,
            [
                "-d",
                "NixOS",
                "--cd",
                "~",
                "-e",
                "bash",
                "-lc",
                "exec \"$0\" desktop \"$@\"",
                "/home/me/.nix-profile/bin/prigh",
                "-name",
                "laptop"
            ]
        );
    }

    #[test]
    fn windows_computer_helper() {
        let helper = r"C:\Program Files\prigh\prigh-computer.exe";
        let plan = |env: Env| plan(Os::Windows, &env, |_| None).unwrap().env;
        let shared = |wslenv: &str| {
            vec![("PRIGH_COMPUTER".to_string(), helper.to_string()), ("WSLENV".to_string(), wslenv.to_string())]
        };
        // not bundled: nothing
        assert_eq!(plan(Env::default()), vec![]);
        assert_eq!(plan(Env { bundled_computer: Some(helper.into()), ..Env::default() }), shared("PRIGH_COMPUTER/p"));
        assert_eq!(
            plan(Env { bundled_computer: Some(helper.into()), wslenv: Some("USERPROFILE/p".into()), ..Env::default() }),
            shared("USERPROFILE/p:PRIGH_COMPUTER/p")
        );
        // already shared by the user: as it is
        assert_eq!(
            plan(Env {
                bundled_computer: Some(helper.into()),
                wslenv: Some("PRIGH_COMPUTER/u".into()),
                ..Env::default()
            }),
            shared("PRIGH_COMPUTER/u")
        );
        // the user's own helper wins (they share it themselves)
        assert_eq!(
            plan(Env {
                bundled_computer: Some(helper.into()),
                computer: Some("/usr/bin/prigh-computer".into()),
                ..Env::default()
            }),
            vec![]
        );
        // Unix: never
        let unix = Env { bundled_computer: Some(helper.into()), ..Env::default() };
        assert_eq!(super::plan(Os::Unix, &unix, find).unwrap().env, vec![]);
    }
}
