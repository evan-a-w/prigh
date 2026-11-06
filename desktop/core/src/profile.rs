//! The browser's profile of prigh's own (see [crate::browser]): where its
//! downloads go.

/// The downloads directory: `XDG_DOWNLOAD_DIR` in `user_dirs` (the
/// contents of `~/.config/user-dirs.dirs`, as `xdg-user-dirs` writes it),
/// else `~/Downloads`.
pub fn downloads_dir(home: &str, user_dirs: Option<&str>) -> String {
    let configured = user_dirs.and_then(|text| {
        text.lines().filter_map(|l| l.trim().strip_prefix("XDG_DOWNLOAD_DIR=")).last().map(|v| {
            let v = v.trim().trim_matches('"');
            match v.strip_prefix("$HOME") {
                Some(rest) => format!("{home}{rest}"),
                None => v.to_string(),
            }
        })
    });
    // `$HOME/` alone means none (xdg-user-dirs' way to turn one off).
    configured.filter(|d| d.starts_with('/') && d.trim_end_matches('/') != home).unwrap_or(format!("{home}/Downloads"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_downloads_directory() {
        let home = "/home/me";
        assert_eq!(downloads_dir(home, None), "/home/me/Downloads");
        let dirs = "# written by xdg-user-dirs-update\nXDG_DESKTOP_DIR=\"$HOME/Desktop\"\nXDG_DOWNLOAD_DIR=\"$HOME/Téléchargements\"\n";
        assert_eq!(downloads_dir(home, Some(dirs)), "/home/me/Téléchargements");
        assert_eq!(downloads_dir(home, Some("XDG_DOWNLOAD_DIR=\"/data/dl\"")), "/data/dl");
        assert_eq!(downloads_dir(home, Some("XDG_DOWNLOAD_DIR=\"$HOME/\"")), "/home/me/Downloads");
        assert_eq!(downloads_dir(home, Some("XDG_DOWNLOAD_DIR=\"relative\"")), "/home/me/Downloads");
        assert_eq!(downloads_dir(home, Some("XDG_MUSIC_DIR=\"$HOME/Music\"")), "/home/me/Downloads");
    }
}
