//! The browser's profile of prigh's own (see [crate::browser]): its
//! settings, merged into its `Default/Preferences` and `Local State` before
//! each launch, so that its window is an app's (the system's title bar, no
//! browser bubbles or prompts), and where its downloads go.

use serde_json::{json, Map, Value};

/// The profile's settings (`Default/Preferences`), `existing` (its file,
/// if any) with prigh's merged in: the system's title bar and borders; no
/// "Restore pages?" (its last run ended cleanly, as far as it knows); no
/// offers to translate, save passwords or fill forms, to sign in or sync,
/// to make it the default browser, or about ad privacy; downloads to
/// `downloads`, without asking where, nor a bubble showing them.
pub fn preferences(existing: Option<&str>, downloads: &str) -> String {
    merge(
        existing,
        &[
            ("browser.custom_chrome_frame", json!(false)),
            ("browser.check_default_browser", json!(false)),
            ("browser.has_seen_welcome_page", json!(true)),
            ("profile.exit_type", json!("Normal")),
            ("profile.exited_cleanly", json!(true)),
            ("profile.password_manager_enabled", json!(false)),
            ("credentials_enable_service", json!(false)),
            ("credentials_enable_autosignin", json!(false)),
            ("autofill.enabled", json!(false)),
            ("autofill.profile_enabled", json!(false)),
            ("autofill.credit_card_enabled", json!(false)),
            ("translate.enabled", json!(false)),
            ("signin.allowed", json!(false)),
            ("sync_promo.show_on_first_run_allowed", json!(false)),
            ("privacy_sandbox.m1.row_notice_acknowledged", json!(true)),
            ("privacy_sandbox.m1.eea_notice_acknowledged", json!(true)),
            ("download.default_directory", json!(downloads)),
            ("download.prompt_for_download", json!(false)),
            ("download.directory_upgrade", json!(true)),
            ("download_bubble.partial_view_enabled", json!(false)),
            ("savefile.default_directory", json!(downloads)),
        ],
    )
}

/// The browser's own settings (`Local State`), `existing` with prigh's
/// merged in: it does not keep running in the background once its last
/// window has closed (closing prigh's window ends the app).
pub fn local_state(existing: Option<&str>) -> String {
    merge(existing, &[("background_mode.enabled", json!(false))])
}

/// `existing` (a JSON object; anything else is replaced) with each
/// dotted path set to its value.
fn merge(existing: Option<&str>, settings: &[(&str, Value)]) -> String {
    let mut root = existing
        .and_then(|t| serde_json::from_str::<Value>(t).ok())
        .filter(Value::is_object)
        .unwrap_or_else(|| Value::Object(Map::new()));
    for (path, value) in settings {
        let mut at = &mut root;
        let mut keys = path.split('.').peekable();
        while let Some(key) = keys.next() {
            if !at.is_object() {
                *at = Value::Object(Map::new());
            }
            let map = at.as_object_mut().unwrap();
            if keys.peek().is_none() {
                map.insert(key.to_string(), value.clone());
                break;
            }
            at = map.entry(key).or_insert_with(|| Value::Object(Map::new()));
        }
    }
    root.to_string()
}
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

    fn pretty(text: &str) -> String {
        serde_json::to_string_pretty(&serde_json::from_str::<Value>(text).unwrap()).unwrap()
    }

    #[test]
    fn a_new_profiles_settings() {
        assert_eq!(
            pretty(&preferences(None, "/home/me/Downloads")),
            pretty(
                &json!({
                    "autofill": { "credit_card_enabled": false, "enabled": false, "profile_enabled": false },
                    "browser": { "check_default_browser": false, "custom_chrome_frame": false,
                                  "has_seen_welcome_page": true },
                    "credentials_enable_autosignin": false,
                    "credentials_enable_service": false,
                    "download": { "default_directory": "/home/me/Downloads", "directory_upgrade": true,
                                   "prompt_for_download": false },
                    "download_bubble": { "partial_view_enabled": false },
                    "privacy_sandbox": { "m1": { "eea_notice_acknowledged": true, "row_notice_acknowledged": true } },
                    "profile": { "exit_type": "Normal", "exited_cleanly": true, "password_manager_enabled": false },
                    "savefile": { "default_directory": "/home/me/Downloads" },
                    "signin": { "allowed": false },
                    "sync_promo": { "show_on_first_run_allowed": false },
                    "translate": { "enabled": false },
                })
                .to_string()
            )
        );
        assert_eq!(local_state(None), r#"{"background_mode":{"enabled":false}}"#);
    }

    #[test]
    fn the_browsers_own_settings_are_kept_and_ours_win() {
        // What the browser wrote: its last run crashed (killed with the app).
        let existing = r#"{"profile":{"exit_type":"Crashed","name":"Person 1"},"browser":{"window_placement":{"top":10}},
                           "translate":true,"zoom":{"level":1.5}}"#;
        let merged: Value = serde_json::from_str(&preferences(Some(existing), "/d")).unwrap();
        assert_eq!(merged["profile"]["exit_type"], "Normal");
        assert_eq!(merged["profile"]["name"], "Person 1");
        assert_eq!(merged["browser"]["window_placement"], json!({ "top": 10 }));
        assert_eq!(merged["browser"]["custom_chrome_frame"], false);
        // Not an object where ours goes: replaced.
        assert_eq!(merged["translate"], json!({ "enabled": false }));
        assert_eq!(merged["zoom"], json!({ "level": 1.5 }));
        // Unreadable: started again.
        assert_eq!(local_state(Some("{\"background_mode\":")), r#"{"background_mode":{"enabled":false}}"#);
        assert_eq!(local_state(Some("[1]")), r#"{"background_mode":{"enabled":false}}"#);
        assert_eq!(
            local_state(Some(r#"{"background_mode":{"enabled":true},"browser":{"x":1}}"#)),
            r#"{"background_mode":{"enabled":false},"browser":{"x":1}}"#
        );
    }

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
