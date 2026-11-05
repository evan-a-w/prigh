//! The PATH the sidecar gets on Linux and macOS. An app started from a
//! desktop launcher or the Dock often has a bare PATH (on macOS always),
//! without the Nix profile or Homebrew, so neither `prigh` nor the tools it
//! runs for the model would be found. The login shell's PATH comes first,
//! then the inherited one, then the usual install locations.

pub fn compose(login: Option<&str>, inherited: Option<&str>, home: Option<&str>, user: Option<&str>) -> String {
    let mut dirs: Vec<String> = Vec::new();
    let mut add = |d: &str| {
        if !d.is_empty() && !dirs.iter().any(|x| x == d) {
            dirs.push(d.to_string());
        }
    };
    for p in [login, inherited].into_iter().flatten() {
        p.split(':').for_each(&mut add);
    }
    if let Some(home) = home {
        add(&format!("{home}/.nix-profile/bin"));
        add(&format!("{home}/.local/state/nix/profile/bin"));
        add(&format!("{home}/.local/bin"));
    }
    if let Some(user) = user {
        add(&format!("/etc/profiles/per-user/{user}/bin"));
    }
    for d in [
        "/run/current-system/sw/bin",
        "/nix/var/nix/profiles/default/bin",
        "/opt/homebrew/bin",
        "/usr/local/bin",
        "/usr/bin",
        "/bin",
    ] {
        add(d)
    }
    dirs.join(":")
}

/// The first `dir/name` in `path` for which `is_executable` holds.
pub fn find(name: &str, path: &str, is_executable: impl Fn(&str) -> bool) -> Option<String> {
    path.split(':')
        .filter(|d| !d.is_empty())
        .map(|d| format!("{}/{name}", d.trim_end_matches('/')))
        .find(|p| is_executable(p))
}

/// The PATH in the output of `env` run by the login shell (`$SHELL -l -c
/// env`, the same in sh, bash, zsh and fish); profile scripts may print
/// other lines too.
pub fn from_env_output(out: &str) -> Option<String> {
    out.lines().rev().find_map(|l| l.strip_prefix("PATH=")).filter(|p| !p.is_empty()).map(str::to_string)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn composes() {
        assert_eq!(
            compose(Some("/a:/b"), Some("/b:/usr/bin::/c"), Some("/home/me"), Some("me")),
            "/a:/b:/usr/bin:/c:/home/me/.nix-profile/bin:/home/me/.local/state/nix/profile/bin:/home/me/.local/bin:\
             /etc/profiles/per-user/me/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:\
             /opt/homebrew/bin:/usr/local/bin:/bin"
        );
        assert_eq!(
            compose(None, None, None, None),
            "/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin"
        );
    }

    #[test]
    fn finds() {
        let exists = |p: &str| p == "/b/prigh" || p == "/c/prigh";
        assert_eq!(find("prigh", "/a:/b/:/c", exists).as_deref(), Some("/b/prigh"));
        assert_eq!(find("prigh", "/a", exists), None);
    }

    #[test]
    fn env_output() {
        assert_eq!(from_env_output("welcome!\nHOME=/h\nPATH=/a:/b\nMANPATH=/m\n").as_deref(), Some("/a:/b"));
        assert_eq!(from_env_output(""), None);
        assert_eq!(from_env_output("PATH=\n"), None);
    }
}
