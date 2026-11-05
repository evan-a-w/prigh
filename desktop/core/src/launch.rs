//! How the sidecar is started: `<prigh> desktop [extra args]`, the prigh
//! from `$PRIGH_DESKTOP_BACKEND` or the PATH; on Windows, where prigh does
//! not run natively, inside WSL through a login shell (for the PATH the
//! user's profile sets up, e.g. Nix's).

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
}

#[derive(Debug, PartialEq, Eq)]
pub struct Launch {
    pub program: String,
    pub args: Vec<String>,
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
            Ok(Launch { program, args })
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
            Ok(Launch { program: "wsl.exe".to_string(), args })
        }
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
            Ok(Launch { program: "/nix/profile/bin/prigh".into(), args: vec!["desktop".into()] })
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
                args: ["desktop", "-cwd", "/my proj", "-tools", "remote"].map(String::from).to_vec()
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
                    .to_vec()
            })
        );
        let env = Env {
            backend: Some("/home/me/.nix-profile/bin/prigh".into()),
            args: Some("-name laptop".into()),
            wsl_distro: Some("NixOS".into()),
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
}
