//! The login shell's PATH ([crate::search_path]), cached between launches:
//! `$SHELL -l -c env` takes 0.1 s with a light zsh and seconds with a heavy
//! one. A launch uses the cached PATH at once and runs the shell in the
//! background to refresh it for the next launch; only without a cache (or
//! when forced, `PRIGH_DESKTOP_REFRESH_ENV=1`) does it wait for the shell.

use std::path::Path;

/// What is cached: the PATH the login shell `shell` gave, at `saved_at`
/// (seconds since the epoch).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cached {
    pub shell: String,
    pub path: String,
    pub saved_at: u64,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Lookup {
    Hit(Cached),
    /// No cache, or one for another shell.
    Miss,
    /// Unreadable: why.
    Corrupt(String),
}

pub fn read(file: &Path, shell: &str) -> Lookup {
    let text = match std::fs::read_to_string(file) {
        Ok(t) => t,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Lookup::Miss,
        Err(e) => return Lookup::Corrupt(e.to_string()),
    };
    let json: serde_json::Value = match serde_json::from_str(&text) {
        Ok(j) => j,
        Err(e) => return Lookup::Corrupt(e.to_string()),
    };
    let field = |name: &str| json.get(name);
    match (
        field("shell").and_then(|v| v.as_str()),
        field("path").and_then(|v| v.as_str()),
        field("saved_at").and_then(|v| v.as_u64()),
    ) {
        (Some(s), Some(path), Some(saved_at)) if !path.is_empty() => {
            if s == shell {
                Lookup::Hit(Cached { shell: s.to_string(), path: path.to_string(), saved_at })
            } else {
                Lookup::Miss
            }
        }
        _ => Lookup::Corrupt("expected {\"shell\", \"path\", \"saved_at\"}".into()),
    }
}

/// Replaces the cache whole (a launch reading it meanwhile sees the old one
/// or the new one).
pub fn write(file: &Path, c: &Cached) -> std::io::Result<()> {
    if let Some(dir) = file.parent() {
        std::fs::create_dir_all(dir)?;
    }
    let json = serde_json::json!({ "shell": c.shell, "path": c.path, "saved_at": c.saved_at });
    let tmp = file.with_extension(format!("tmp{}", std::process::id()));
    std::fs::write(&tmp, json.to_string())?;
    std::fs::rename(&tmp, file).inspect_err(|_| {
        let _ = std::fs::remove_file(&tmp);
    })
}

#[derive(Debug, PartialEq, Eq)]
pub enum Source {
    /// Cached `age` seconds ago.
    Cache { age: u64 },
    /// The login shell, just now.
    Shell,
    /// None: the shell gave no PATH.
    Nothing,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Resolved {
    pub login: Option<String>,
    pub source: Source,
    /// Run [refresh] in the background, for the next launch.
    pub refresh: bool,
    /// For the log: a cache that could not be read or written.
    pub notes: Vec<String>,
}

/// The login PATH for this launch. `run` runs the login shell (its PATH,
/// if it gave one); it is called only when there is no usable cache or
/// `force`.
pub fn resolve(file: &Path, shell: &str, force: bool, now: u64, run: impl FnOnce() -> Option<String>) -> Resolved {
    let mut notes = Vec::new();
    match (force, read(file, shell)) {
        (false, Lookup::Hit(c)) => {
            return Resolved {
                login: Some(c.path),
                source: Source::Cache { age: now.saturating_sub(c.saved_at) },
                refresh: true,
                notes,
            }
        }
        (_, Lookup::Corrupt(why)) => notes.push(format!("ignoring {}: {why}", file.display())),
        _ => (),
    }
    let (login, note) = refresh(file, shell, now, run);
    notes.extend(note);
    let source = if login.is_some() { Source::Shell } else { Source::Nothing };
    Resolved { login, source, refresh: false, notes }
}

/// Runs the login shell and caches its PATH (a run that gives none leaves
/// the cache as it is); the PATH, and what went wrong saving it.
pub fn refresh(
    file: &Path,
    shell: &str,
    now: u64,
    run: impl FnOnce() -> Option<String>,
) -> (Option<String>, Option<String>) {
    let login = run();
    let note = login.as_ref().and_then(|path| {
        write(file, &Cached { shell: shell.to_string(), path: path.clone(), saved_at: now })
            .err()
            .map(|e| format!("cannot cache the login PATH in {}: {e}", file.display()))
    });
    (login, note)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::Cell;
    use std::path::PathBuf;

    fn dir(name: &str) -> PathBuf {
        let d = std::env::temp_dir().join(format!("prigh-login-env-{name}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&d);
        d
    }

    /// A login shell that gives `path`, counting its runs.
    fn shell<'a>(runs: &'a Cell<u32>, path: Option<&'a str>) -> impl FnOnce() -> Option<String> + 'a {
        move || {
            runs.set(runs.get() + 1);
            path.map(str::to_string)
        }
    }

    #[test]
    fn miss_runs_the_shell_and_caches() {
        let d = dir("miss");
        let file = d.join("sub/login-env.json");
        let runs = Cell::new(0);
        let r = resolve(&file, "/bin/zsh", false, 100, shell(&runs, Some("/a:/b")));
        assert_eq!(r, Resolved { login: Some("/a:/b".into()), source: Source::Shell, refresh: false, notes: vec![] });
        assert_eq!(runs.get(), 1);
        assert_eq!(
            read(&file, "/bin/zsh"),
            Lookup::Hit(Cached { shell: "/bin/zsh".into(), path: "/a:/b".into(), saved_at: 100 })
        );
        // Another shell's cache is a miss.
        assert_eq!(read(&file, "/bin/bash"), Lookup::Miss);
        // A shell that gives no PATH caches nothing.
        let other = d.join("other.json");
        let r = resolve(&other, "/bin/zsh", false, 100, shell(&runs, None));
        assert_eq!(r, Resolved { login: None, source: Source::Nothing, refresh: false, notes: vec![] });
        assert_eq!(read(&other, "/bin/zsh"), Lookup::Miss);
        let _ = std::fs::remove_dir_all(&d);
    }

    #[test]
    fn hit_uses_the_cache_without_the_shell() {
        let d = dir("hit");
        let file = d.join("login-env.json");
        write(&file, &Cached { shell: "/bin/zsh".into(), path: "/cached".into(), saved_at: 100 }).unwrap();
        let runs = Cell::new(0);
        let r = resolve(&file, "/bin/zsh", false, 160, shell(&runs, Some("/new")));
        assert_eq!(
            r,
            Resolved { login: Some("/cached".into()), source: Source::Cache { age: 60 }, refresh: true, notes: vec![] }
        );
        assert_eq!(runs.get(), 0);
        let _ = std::fs::remove_dir_all(&d);
    }

    #[test]
    fn refresh_updates_the_cache_for_next_time() {
        let d = dir("refresh");
        let file = d.join("login-env.json");
        write(&file, &Cached { shell: "/bin/zsh".into(), path: "/old".into(), saved_at: 100 }).unwrap();
        let runs = Cell::new(0);
        assert_eq!(refresh(&file, "/bin/zsh", 200, shell(&runs, Some("/new"))), (Some("/new".into()), None));
        let r = resolve(&file, "/bin/zsh", false, 210, shell(&runs, Some("/newer")));
        assert_eq!((r.login.as_deref(), r.source), (Some("/new"), Source::Cache { age: 10 }));
        // A shell that fails keeps the last PATH.
        assert_eq!(refresh(&file, "/bin/zsh", 300, shell(&runs, None)), (None, None));
        assert_eq!(
            read(&file, "/bin/zsh"),
            Lookup::Hit(Cached { shell: "/bin/zsh".into(), path: "/new".into(), saved_at: 200 })
        );
        // Forced: the shell now, whatever is cached.
        let r = resolve(&file, "/bin/zsh", true, 400, shell(&runs, Some("/forced")));
        assert_eq!(r, Resolved { login: Some("/forced".into()), source: Source::Shell, refresh: false, notes: vec![] });
        assert_eq!(runs.get(), 3);
        assert!(matches!(read(&file, "/bin/zsh"), Lookup::Hit(Cached { saved_at: 400, .. })));
        // Unwritable: said, and the PATH still used.
        let blocked = d.join("blocked");
        std::fs::write(&blocked, "a file").unwrap();
        let (login, note) = refresh(&blocked.join("login-env.json"), "/bin/zsh", 500, shell(&runs, Some("/p")));
        assert_eq!(login.as_deref(), Some("/p"));
        assert!(note.unwrap().starts_with("cannot cache the login PATH in "));
        let _ = std::fs::remove_dir_all(&d);
    }

    #[test]
    fn corrupt_cache_is_replaced() {
        let d = dir("corrupt");
        std::fs::create_dir_all(&d).unwrap();
        let file = d.join("login-env.json");
        for text in [
            "{not json",
            "{\"shell\": \"/bin/zsh\"}",
            "{\"shell\": \"/bin/zsh\", \"path\": \"\", \"saved_at\": 1}",
            "[]",
        ] {
            std::fs::write(&file, text).unwrap();
            assert!(matches!(read(&file, "/bin/zsh"), Lookup::Corrupt(_)), "{text}");
            let runs = Cell::new(0);
            let r = resolve(&file, "/bin/zsh", false, 100, shell(&runs, Some("/a")));
            assert_eq!((r.login.as_deref(), r.source, r.refresh, runs.get()), (Some("/a"), Source::Shell, false, 1));
            assert_eq!(r.notes.len(), 1);
            assert!(r.notes[0].starts_with(&format!("ignoring {}: ", file.display())), "{:?}", r.notes);
            assert_eq!(
                read(&file, "/bin/zsh"),
                Lookup::Hit(Cached { shell: "/bin/zsh".into(), path: "/a".into(), saved_at: 100 })
            );
        }
        let _ = std::fs::remove_dir_all(&d);
    }
}
