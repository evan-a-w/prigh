//! The browser window's entry in launchers, docks and taskbars on Linux.
//! A desktop shell matches a window to a desktop entry by its class (its
//! app_id on Wayland) and the entry's `StartupWMClass`. The installed entry
//! (`prigh.desktop`) says `prigh-desktop`, the app's own window; a
//! Chromium-family `--app` window is `prigh` on X11 (`--class`) and on
//! Wayland is named after the site and the profile, e.g.
//! `chrome-127.0.0.1__-Default` (the port, query and fragment left out, the
//! path's `/`s as `_`s). So in browser mode the app writes a user entry with
//! the installed one's desktop-file ID, which shadows it (`$XDG_DATA_HOME`
//! comes first): launchers list prigh once, and the window matches it. It is
//! the installed entry, read at each launch, with only `Exec` (made stable)
//! and `StartupWMClass` rewritten, and [MARK]: the app only rewrites or
//! removes an entry with that mark. With the app's own window it removes
//! its own, so the installed entry shows again.

use std::path::{Path, PathBuf};

/// The desktop-file ID of the installed entry (`nix/desktop.nix`, Tauri's
/// deb) and of the app's override.
pub const ENTRY_ID: &str = "prigh.desktop";

/// The key that marks an entry as the app's own.
pub const MARK: &str = "X-Prigh-Generated";

/// The class of the browser `program`'s `--app={url}` window: its Wayland
/// app_id (see [app_id]) or, on X11, `--class`.
pub fn window_class(program: &str, url: &str, wayland: bool) -> Option<String> {
    if wayland {
        app_id(program, url)
    } else {
        Some(crate::browser::WINDOW_CLASS.to_string())
    }
}

/// Whether Chromium's `--ozone-platform-hint=auto` takes Wayland, from
/// `$WAYLAND_DISPLAY` and `$XDG_SESSION_TYPE`.
pub fn is_wayland(wayland_display: Option<&str>, session_type: Option<&str>) -> bool {
    wayland_display.is_some_and(|d| !d.is_empty()) || session_type == Some("wayland")
}

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

/// Whether `entry` is one the app wrote.
pub fn is_ours(entry: &str) -> bool {
    main_group(entry).any(|l| l.trim_end() == format!("{MARK}=true"))
}

/// Whether `file_name` with `entry` is one that earlier versions of the app
/// wrote, named after the window's app_id.
pub fn is_old_generated(file_name: &str, entry: &str) -> bool {
    ["chrome-", "brave-", "msedge-", "vivaldi-"].iter().any(|p| file_name.starts_with(p))
        && file_name.ends_with("-Default.desktop")
        && entry.lines().any(|l| l == "Name=prigh")
        && entry.lines().any(|l| l == "Comment=prigh's window in the browser")
}

/// The entry the package installs, from the same template and bundle
/// settings Tauri makes it from: for when none is installed (a build run
/// from the tree).
pub fn packaged_entry(exec: &str) -> String {
    let conf: serde_json::Value =
        serde_json::from_str(include_str!("../../app/tauri.conf.json")).expect("tauri.conf.json");
    let bundle = &conf["bundle"];
    // Tauri's freedesktop names for its categories.
    let categories = match bundle["category"].as_str() {
        Some("DeveloperTool") => "Development;",
        _ => "",
    };
    // A Windows checkout may have CRLF line ends.
    include_str!("../../app/prigh.desktop")
        .replace("\r\n", "\n")
        .replace("{{comment}}", bundle["shortDescription"].as_str().unwrap_or("prigh"))
        .replace("{{icon}}", conf["mainBinaryName"].as_str().unwrap_or("prigh-desktop"))
        .replace("{{categories}}", categories)
        .replace("{{exec}}", &quote_exec(exec))
}

/// The program `entry`'s `Exec` runs.
pub fn exec_program(entry: &str) -> Option<String> {
    let exec = main_group(entry).find_map(|l| l.strip_prefix("Exec="))?;
    Some(split_exec(exec)?.0)
}

/// The installed `entry` as the app's own: `Exec` runs `program` (its
/// arguments kept), `TryExec` names it (an entry whose program is gone is
/// not shown), `StartupWMClass` is `class`, nothing hides it, and it has
/// the mark.
pub fn override_entry(entry: &str, class: &str, program: &str) -> String {
    const MAIN: &str = "[Desktop Entry]";
    let quoted = quote_exec(program);
    let mut out = String::new();
    let mut group = "";
    let (mut seen_exec, mut seen_class) = (false, false);
    let end_main = |out: &mut String, seen_exec: bool, seen_class: bool| {
        if !seen_exec {
            out.push_str(&format!("Exec={quoted}\n"));
        }
        out.push_str(&format!("TryExec={}\n", escape_string(program)));
        if !seen_class {
            out.push_str(&format!("StartupWMClass={class}\n"));
        }
        out.push_str(&format!("{MARK}=true\n"));
    };
    for line in entry.lines() {
        if line.starts_with('[') {
            if group == MAIN {
                end_main(&mut out, seen_exec, seen_class);
            }
            group = line.trim_end();
        } else if group == MAIN {
            match line.split('=').next().unwrap_or("").trim() {
                "Exec" => {
                    let rest = line.split_once('=').and_then(|(_, v)| split_exec(v)).map(|(_, r)| r);
                    out.push_str(&format!("Exec={quoted}{}\n", escape_string(&rest.unwrap_or_default())));
                    seen_exec = true;
                    continue;
                }
                "StartupWMClass" => {
                    out.push_str(&format!("StartupWMClass={class}\n"));
                    seen_class = true;
                    continue;
                }
                "TryExec" | "NoDisplay" | "Hidden" | "DBusActivatable" | MARK => continue,
                _ => {}
            }
        }
        out.push_str(line);
        out.push('\n');
    }
    if group == MAIN {
        end_main(&mut out, seen_exec, seen_class);
    }
    out
}

/// `program`, the installed entry's in `data_dir` (an XDG data directory,
/// e.g. `~/.nix-profile/share`), through that directory's profile:
/// `<data_dir>/../bin/<name>`. A Nix profile's path stays the same across
/// updates; the store path in the entry goes away with its generation.
pub fn profile_program(data_dir: &Path, program: &str) -> Option<PathBuf> {
    let name = Path::new(program).file_name()?;
    Some(data_dir.parent()?.join("bin").join(name))
}

/// The XDG data directories.
#[derive(Debug)]
pub struct Dirs {
    /// `$XDG_DATA_HOME`: the user's, where the override goes.
    pub home: PathBuf,
    /// `$XDG_DATA_DIRS`: where the installed entry is.
    pub system: Vec<PathBuf>,
}

impl Dirs {
    /// From `$XDG_DATA_HOME`, `$HOME` and `$XDG_DATA_DIRS`, with the spec's
    /// defaults (relative paths are ignored).
    pub fn new(data_home: Option<&str>, home: Option<&str>, data_dirs: Option<&str>) -> Option<Dirs> {
        let absolute = |p: &&str| p.starts_with('/');
        let home = match data_home.filter(absolute) {
            Some(d) => PathBuf::from(d),
            None => PathBuf::from(home.filter(absolute)?).join(".local/share"),
        };
        let data_dirs = data_dirs.filter(|d| !d.is_empty()).unwrap_or("/usr/local/share/:/usr/share/");
        let system = data_dirs.split(':').filter(absolute).map(PathBuf::from).filter(|d| *d != home).collect();
        Some(Dirs { home, system })
    }
}

/// Brings the user's applications in line with the window: with `class`
/// (the browser's window), the override for it; without (the app's own),
/// none. Removes the entries earlier versions named after the app_id.
/// `own_program` is the app's executable: the entry in its own package (a
/// GApps wrapper puts that first in `$XDG_DATA_DIRS`; Nix's runs
/// `prigh-desktop` from the PATH, without the outer wrapper's environment)
/// is not the installed one, and with no installed entry the override runs
/// `own_program`. Returns what it did, for the log.
pub fn sync(dirs: &Dirs, class: Option<&str>, own_program: &str) -> Vec<String> {
    let mut log = Vec::new();
    let apps = dirs.home.join("applications");
    let mut remove = |path: &Path, what: &str| match std::fs::remove_file(path) {
        Ok(()) => log.push(format!("removed {what} {}", path.display())),
        Err(e) => log.push(format!("removing {what} {}: {e}", path.display())),
    };
    let mut old: Vec<PathBuf> = std::fs::read_dir(&apps)
        .into_iter()
        .flatten()
        .filter_map(Result::ok)
        .map(|f| f.path())
        .filter(|path| {
            let name = path.file_name().unwrap_or_default().to_string_lossy();
            !path.is_symlink() && std::fs::read_to_string(path).is_ok_and(|e| is_old_generated(&name, &e))
        })
        .collect();
    old.sort();
    for path in old {
        remove(&path, "the old taskbar entry");
    }
    let path = apps.join(ENTRY_ID);
    // A link is the user's (Home Manager's xdg.dataFile, say): the app
    // writes files.
    let existing = if path.is_symlink() { Some(String::new()) } else { std::fs::read_to_string(&path).ok() };
    let ours = existing.as_deref().is_some_and(is_ours);
    let Some(class) = class else {
        if ours {
            remove(&path, "prigh's launcher entry for the browser window");
        }
        return log;
    };
    if existing.is_some() && !ours {
        log.push(format!("{} is not prigh's: leaving it (the browser window may not show as prigh)", path.display()));
        return log;
    }
    let real = |p: &Path| std::fs::canonicalize(p).ok();
    let own_package = Path::new(own_program).parent().and_then(Path::parent).and_then(real);
    let installed = dirs.system.iter().find_map(|dir| {
        if own_package.is_some() && dir.parent().and_then(real) == own_package {
            return None;
        }
        let entry = std::fs::read_to_string(dir.join("applications").join(ENTRY_ID)).ok()?;
        (!is_ours(&entry)).then_some((dir, entry))
    });
    let (base, program) = match installed {
        Some((dir, entry)) => {
            let Some(program) = exec_program(&entry) else {
                log.push(format!(
                    "{}/applications/{ENTRY_ID} has no Exec: no launcher entry for the window",
                    dir.display()
                ));
                return log;
            };
            let program = match profile_program(dir, &program) {
                Some(p) if program.starts_with('/') && real(&p).is_some() && real(&p) == real(Path::new(&program)) => {
                    p.to_string_lossy().into_owned()
                }
                _ => program,
            };
            (entry, program)
        }
        None => (packaged_entry(own_program), own_program.to_string()),
    };
    let entry = override_entry(&base, class, &program);
    if existing.as_deref() == Some(entry.as_str()) {
        return log;
    }
    let tmp = apps.join(format!(".{ENTRY_ID}.prigh-tmp"));
    let written = std::fs::create_dir_all(&apps)
        .and_then(|()| std::fs::write(&tmp, &entry))
        .and_then(|()| std::fs::rename(&tmp, &path));
    match written {
        Ok(()) => log.push(format!("the launcher entry for the browser window ({class}): {}", path.display())),
        Err(e) => log.push(format!("writing the launcher entry {}: {e}", path.display())),
    }
    log
}

fn main_group(entry: &str) -> impl Iterator<Item = &str> {
    entry.lines().skip_while(|l| l.trim_end() != "[Desktop Entry]").skip(1).take_while(|l| !l.starts_with('['))
}

/// An `Exec` value's program and the rest of it, both unescaped.
fn split_exec(exec: &str) -> Option<(String, String)> {
    let exec = unescape_string(exec);
    let exec = exec.trim_start();
    let (program, rest) = match exec.strip_prefix('"') {
        Some(quoted) => {
            let mut program = String::new();
            let mut chars = quoted.char_indices();
            loop {
                match chars.next()? {
                    (_, '\\') => program.push(chars.next()?.1),
                    (i, '"') => break (program, &quoted[i + 1..]),
                    (_, c) => program.push(c),
                }
            }
        }
        None => {
            let end = exec.find([' ', '\t']).unwrap_or(exec.len());
            (exec[..end].to_string(), &exec[end..])
        }
    };
    let program = program.replace("%%", "%");
    (!program.is_empty()).then(|| (program, rest.to_string()))
}

/// The string type's escapes (`\s`, `\n`, `\t`, `\r`, `\\`); others are
/// left for `Exec`'s quoting.
fn unescape_string(s: &str) -> String {
    let mut out = String::new();
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }
        match chars.next() {
            Some('s') => out.push(' '),
            Some('n') => out.push('\n'),
            Some('t') => out.push('\t'),
            Some('r') => out.push('\r'),
            Some('\\') => out.push('\\'),
            Some(c) => out.extend(['\\', c]),
            None => out.push('\\'),
        }
    }
    out
}

fn escape_string(s: &str) -> String {
    s.replace('\\', "\\\\").replace('\n', "\\n").replace('\t', "\\t").replace('\r', "\\r")
}

/// `program` as `Exec`'s first word: `%` doubled, quoted when it has a
/// reserved character, then the string escapes.
fn quote_exec(program: &str) -> String {
    let program = program.replace('%', "%%");
    let word = if program.contains(|c| " \t\n\"'\\><~|&;$*?#()`".contains(c)) {
        let mut q = String::from("\"");
        for c in program.chars() {
            if matches!(c, '"' | '`' | '$' | '\\') {
                q.push('\\');
            }
            q.push(c);
        }
        q.push('"');
        q
    } else {
        program
    };
    escape_string(&word)
}

#[cfg(test)]
mod tests {
    use super::*;

    const INSTALLED: &str = "[Desktop Entry]\nType=Application\nName=prigh\nGenericName=Coding agent\n\
        Comment=prigh, the coding agent\nExec=/nix/store/abc-prigh-desktop-0.1.0/bin/prigh-desktop\n\
        Icon=prigh-desktop\nTerminal=false\nCategories=Development;\nStartupWMClass=prigh-desktop\n\
        StartupNotify=true\nKeywords=agent;ai;coding;llm;\n";

    const OLD: &str = "[Desktop Entry]\nType=Application\nName=prigh\nComment=prigh's window in the browser\n\
        Exec=/nix/store/x-prigh-desktop-unwrapped-0.1.0/bin/prigh-desktop\nIcon=prigh-desktop\n\
        Terminal=false\nNoDisplay=true\nStartupWMClass=chrome-127.0.0.1__-Default\n";

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
    fn window_classes() {
        let url = "http://127.0.0.1:7780/";
        assert_eq!(window_class("chromium", url, true).as_deref(), Some("chrome-127.0.0.1__-Default"));
        assert_eq!(window_class("chromium", url, false).as_deref(), Some("prigh"));
        assert_eq!(window_class("chromium", "nope", true), None);
        assert!(is_wayland(Some("wayland-1"), None));
        assert!(is_wayland(None, Some("wayland")));
        assert!(!is_wayland(Some(""), Some("x11")));
        assert!(!is_wayland(None, None));
    }

    #[test]
    fn the_override_is_the_installed_entry_with_the_class_and_the_program() {
        let program = "/home/u/.nix-profile/bin/prigh-desktop";
        let entry = override_entry(INSTALLED, "chrome-127.0.0.1__-Default", program);
        assert_eq!(
            entry,
            "[Desktop Entry]\nType=Application\nName=prigh\nGenericName=Coding agent\n\
             Comment=prigh, the coding agent\nExec=/home/u/.nix-profile/bin/prigh-desktop\n\
             Icon=prigh-desktop\nTerminal=false\nCategories=Development;\n\
             StartupWMClass=chrome-127.0.0.1__-Default\nStartupNotify=true\nKeywords=agent;ai;coding;llm;\n\
             TryExec=/home/u/.nix-profile/bin/prigh-desktop\nX-Prigh-Generated=true\n"
        );
        assert!(is_ours(&entry));
        assert!(!is_ours(INSTALLED));
        assert!(!is_ours("[Desktop Action a]\nX-Prigh-Generated=true\n"));
        // Its own output again: the same.
        assert_eq!(override_entry(&entry, "chrome-127.0.0.1__-Default", program), entry);
    }

    #[test]
    fn the_override_drops_what_hides_it_and_keeps_other_groups_and_arguments() {
        let installed = "# comment\n[Desktop Entry]\nName=prigh\nExec=\"/opt/my apps/prigh\" --x\\s%U\nTryExec=/old\n\
                         NoDisplay=true\nHidden=true\nDBusActivatable=true\n\
                         [Desktop Action new]\nName=New\nExec=/opt/prigh --new\n";
        assert_eq!(exec_program(installed).as_deref(), Some("/opt/my apps/prigh"));
        assert_eq!(
            override_entry(installed, "prigh", "/opt/my apps/prigh"),
            "# comment\n[Desktop Entry]\nName=prigh\nExec=\"/opt/my apps/prigh\" --x %U\n\
             TryExec=/opt/my apps/prigh\nStartupWMClass=prigh\nX-Prigh-Generated=true\n\
             [Desktop Action new]\nName=New\nExec=/opt/prigh --new\n"
        );
    }

    #[test]
    fn exec_quoting_round_trips() {
        for program in ["/nix/store/x/bin/prigh-desktop", "/opt/my apps/p", "/a\"b$c`d\\e", "/50%/p", "prigh-desktop"] {
            let entry = format!("[Desktop Entry]\nExec={} %U\n", quote_exec(program));
            assert_eq!(exec_program(&entry).as_deref(), Some(program), "{entry}");
        }
        assert_eq!(quote_exec("/opt/my apps/p"), "\"/opt/my apps/p\"");
        // `\"` inside the quotes, its backslash escaped as a string's.
        assert_eq!(quote_exec("/a\"b"), "\"/a\\\\\"b\"");
        // The string's escapes come first: `\s` is a space between words.
        assert_eq!(exec_program("[Desktop Entry]\nExec=/opt/a\\sb\n").as_deref(), Some("/opt/a"));
        assert_eq!(exec_program("[Desktop Entry]\nName=x\n"), None);
        assert_eq!(exec_program("[Desktop Entry]\nExec=\"/unterminated\n"), None);
        assert_eq!(exec_program("[Desktop Action a]\nExec=/a\n[Desktop Entry]\nExec=/b\n").as_deref(), Some("/b"));
    }

    #[test]
    fn the_packaged_entry_is_the_installed_one() {
        assert_eq!(packaged_entry("/nix/store/abc-prigh-desktop-0.1.0/bin/prigh-desktop"), INSTALLED);
    }

    #[test]
    fn old_generated_entries() {
        assert!(is_old_generated("chrome-127.0.0.1__-Default.desktop", OLD));
        for name in ["brave-a__-Default.desktop", "msedge-a__x-Default.desktop", "vivaldi-::1__-Default.desktop"] {
            assert!(is_old_generated(name, OLD), "{name}");
        }
        assert!(!is_old_generated("prigh.desktop", OLD));
        assert!(!is_old_generated("chrome-a__-Profile_1.desktop", OLD));
        assert!(!is_old_generated("firefox-a__-Default.desktop", OLD));
        // The user's own entries for sites.
        let other = OLD.replace("Name=prigh", "Name=Mail");
        assert!(!is_old_generated("chrome-127.0.0.1__-Default.desktop", &other));
        let other = OLD.replace("Comment=prigh's window in the browser", "Comment=mine");
        assert!(!is_old_generated("chrome-127.0.0.1__-Default.desktop", &other));
    }

    #[test]
    fn dirs() {
        let d = Dirs::new(None, Some("/home/u"), None).unwrap();
        assert_eq!(d.home, PathBuf::from("/home/u/.local/share"));
        assert_eq!(d.system, [PathBuf::from("/usr/local/share/"), PathBuf::from("/usr/share/")]);
        let d = Dirs::new(Some("/d"), Some("/home/u"), Some("/d:/home/u/.nix-profile/share::rel:/usr/share")).unwrap();
        assert_eq!(d.home, PathBuf::from("/d"));
        assert_eq!(d.system, [PathBuf::from("/home/u/.nix-profile/share"), PathBuf::from("/usr/share")]);
        assert_eq!(Dirs::new(Some("relative"), Some("/h"), None).unwrap().home, PathBuf::from("/h/.local/share"));
        assert!(Dirs::new(None, None, None).is_none());
        assert_eq!(
            profile_program(Path::new("/home/u/.nix-profile/share"), "/nix/store/x/bin/prigh-desktop"),
            Some(PathBuf::from("/home/u/.nix-profile/bin/prigh-desktop"))
        );
    }

    /// The files themselves (links need Unix).
    #[cfg(unix)]
    mod files {
        use super::*;

        /// A temporary directory, removed when dropped.
        struct Temp(PathBuf);
        impl Temp {
            fn new(name: &str) -> Temp {
                let dir = std::env::temp_dir().join(format!("prigh-taskbar-{name}-{}", std::process::id()));
                let _ = std::fs::remove_dir_all(&dir);
                std::fs::create_dir_all(&dir).unwrap();
                Temp(dir)
            }
            fn sync(&self, dirs: &Dirs, class: Option<&str>) -> Vec<String> {
                let root = self.0.to_string_lossy().into_owned();
                sync(dirs, class, "/build/prigh-desktop").into_iter().map(|l| l.replace(&root, "T")).collect()
            }
        }
        impl Drop for Temp {
            fn drop(&mut self) {
                let _ = std::fs::remove_dir_all(&self.0);
            }
        }

        fn write(path: &Path, content: &str) {
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, content).unwrap();
        }

        fn read(path: &Path) -> String {
            std::fs::read_to_string(path).unwrap()
        }

        /// The files in `dir/applications`, sorted.
        fn listing(dir: &Path) -> Vec<String> {
            let mut names: Vec<String> = std::fs::read_dir(dir.join("applications"))
                .map(|r| r.filter_map(Result::ok).map(|f| f.file_name().to_string_lossy().into_owned()).collect())
                .unwrap_or_default();
            names.sort();
            names
        }

        /// Nix's layout: a package in the store with its program and entry, and
        /// a profile that links to it, in the data directories.
        fn nix_layout(t: &Temp) -> Dirs {
            let store = t.0.join("store/abc-prigh-desktop");
            let program = store.join("bin/prigh-desktop");
            write(&program, "#!/bin/sh\n");
            let entry =
                INSTALLED.replace("/nix/store/abc-prigh-desktop-0.1.0/bin/prigh-desktop", &program.to_string_lossy());
            write(&store.join("share/applications/prigh.desktop"), &entry);
            std::os::unix::fs::symlink(&store, t.0.join("profile")).unwrap();
            Dirs { home: t.0.join("home"), system: vec![t.0.join("nowhere"), t.0.join("profile/share")] }
        }

        #[test]
        fn the_browser_window_gets_one_entry_shadowing_the_installed_one() {
            let t = Temp::new("browser");
            let dirs = nix_layout(&t);
            let apps = dirs.home.join("applications");
            write(&apps.join("chrome-127.0.0.1__-Default.desktop"), OLD);
            write(&apps.join("brave-ai.example.com__-Default.desktop"), OLD);
            write(&apps.join("chrome-mail.example.com__-Default.desktop"), &OLD.replace("Name=prigh", "Name=Mail"));
            assert_eq!(
                t.sync(&dirs, Some("chrome-127.0.0.1__-Default")),
                [
                    "removed the old taskbar entry T/home/applications/brave-ai.example.com__-Default.desktop",
                    "removed the old taskbar entry T/home/applications/chrome-127.0.0.1__-Default.desktop",
                    "the launcher entry for the browser window (chrome-127.0.0.1__-Default): T/home/applications/prigh.desktop",
                ]
            );
            assert_eq!(listing(&dirs.home), ["chrome-mail.example.com__-Default.desktop", "prigh.desktop"]);
            // Exec through the profile, not the store.
            let program = t.0.join("profile/bin/prigh-desktop").to_string_lossy().into_owned();
            assert_eq!(
                read(&apps.join("prigh.desktop")),
                override_entry(INSTALLED, "chrome-127.0.0.1__-Default", &program)
            );
            // The same again: nothing written.
            assert!(t.sync(&dirs, Some("chrome-127.0.0.1__-Default")).is_empty());
            assert_eq!(
                t.sync(&dirs, Some("chrome-ai.example.com__-Default")),
                ["the launcher entry for the browser window (chrome-ai.example.com__-Default): T/home/applications/prigh.desktop"]
            );
            // The app's own window: the installed entry shows again.
            assert_eq!(
                t.sync(&dirs, None),
                ["removed prigh's launcher entry for the browser window T/home/applications/prigh.desktop"]
            );
            assert_eq!(listing(&dirs.home), ["chrome-mail.example.com__-Default.desktop"]);
            assert!(t.sync(&dirs, None).is_empty());
        }

        #[test]
        fn exec_stays_the_installed_ones_when_the_profile_runs_another() {
            let t = Temp::new("store-exec");
            let program = t.0.join("store/abc-prigh-desktop/bin/prigh-desktop").to_string_lossy().into_owned();
            write(Path::new(&program), "#!/bin/sh\n");
            write(&t.0.join("profile/bin/prigh-desktop"), "another build");
            let entry = INSTALLED.replace("/nix/store/abc-prigh-desktop-0.1.0/bin/prigh-desktop", &program);
            write(&t.0.join("profile/share/applications/prigh.desktop"), &entry);
            let dirs = Dirs { home: t.0.join("home"), system: vec![t.0.join("profile/share")] };
            t.sync(&dirs, Some("prigh"));
            assert_eq!(exec_program(&read(&dirs.home.join("applications/prigh.desktop"))), Some(program));
        }

        #[test]
        fn without_an_installed_entry_the_packaged_one_runs_the_app() {
            let t = Temp::new("none-installed");
            let dirs = Dirs { home: t.0.join("home"), system: vec![t.0.join("nowhere")] };
            t.sync(&dirs, Some("prigh"));
            let written = read(&dirs.home.join("applications/prigh.desktop"));
            assert_eq!(
                written,
                override_entry(&packaged_entry("/build/prigh-desktop"), "prigh", "/build/prigh-desktop")
            );
            assert!(written.contains("\nExec=/build/prigh-desktop\n"));
        }

        #[test]
        fn the_users_own_entries_are_never_touched() {
            let t = Temp::new("users");
            let dirs = nix_layout(&t);
            let path = dirs.home.join("applications/prigh.desktop");
            let mine = "[Desktop Entry]\nName=my prigh\nExec=/opt/p\n";
            write(&path, mine);
            assert_eq!(
                t.sync(&dirs, Some("prigh")),
                ["T/home/applications/prigh.desktop is not prigh's: leaving it (the browser window may not show as prigh)"]
            );
            assert!(t.sync(&dirs, None).is_empty());
            assert_eq!(read(&path), mine);
            // A link (Home Manager's xdg.dataFile), even to one that looks like ours.
            std::fs::remove_file(&path).unwrap();
            let target = t.0.join("ours.desktop");
            write(&target, &override_entry(INSTALLED, "prigh", "/p"));
            std::os::unix::fs::symlink(&target, &path).unwrap();
            assert_eq!(t.sync(&dirs, Some("x")).len(), 1);
            assert!(t.sync(&dirs, None).is_empty());
            assert!(path.is_symlink());
            // An old entry that is a link.
            let link = dirs.home.join("applications/chrome-a__-Default.desktop");
            write(&t.0.join("old.desktop"), OLD);
            std::os::unix::fs::symlink(t.0.join("old.desktop"), &link).unwrap();
            assert!(t.sync(&dirs, None).is_empty());
            assert!(link.is_symlink());
        }

        #[test]
        fn the_apps_own_packages_entry_is_not_the_installed_one() {
            let t = Temp::new("own-package");
            let mut dirs = nix_layout(&t);
            // Nix's unwrapped build: its GApps wrapper puts its share first.
            let unwrapped = t.0.join("store/def-prigh-desktop-unwrapped");
            let own = unwrapped.join("bin/prigh-desktop");
            write(&own, "");
            write(
                &unwrapped.join("share/applications/prigh.desktop"),
                "[Desktop Entry]\nName=prigh\nExec=prigh-desktop\n",
            );
            dirs.system.insert(0, unwrapped.join("share"));
            sync(&dirs, Some("prigh"), &own.to_string_lossy());
            let program = exec_program(&read(&dirs.home.join("applications/prigh.desktop"))).unwrap();
            assert_eq!(program, t.0.join("profile/bin/prigh-desktop").to_string_lossy());
            // Nothing else installed: the packaged entry, running the app.
            dirs.system.truncate(1);
            sync(&dirs, Some("prigh"), &own.to_string_lossy());
            let written = read(&dirs.home.join("applications/prigh.desktop"));
            assert_eq!(
                written,
                override_entry(&packaged_entry(&own.to_string_lossy()), "prigh", &own.to_string_lossy())
            );
        }

        #[test]
        fn an_override_among_the_data_dirs_is_not_the_installed_entry() {
            let t = Temp::new("ours-in-system");
            let mut dirs = nix_layout(&t);
            let stale = t.0.join("stale/share");
            write(&stale.join("applications/prigh.desktop"), &override_entry(INSTALLED, "prigh", "/gone"));
            dirs.system.insert(0, stale);
            t.sync(&dirs, Some("prigh"));
            let program = exec_program(&read(&dirs.home.join("applications/prigh.desktop"))).unwrap();
            assert_eq!(program, t.0.join("profile/bin/prigh-desktop").to_string_lossy());
        }
    }
}
