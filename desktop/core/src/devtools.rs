//! The browser's app window (see [crate::browser]) as an app's window: that
//! browser runs on a profile of prigh's own, so a link it opens itself would
//! open there, in a tabbed window without the user's own browser's sign-ins
//! and extensions, and the browser's own windows (a new tab, history,
//! settings, an incognito window) would show it is a browser. The shell
//! watches its pages through the DevTools protocol on a pipe only it holds
//! (`--remote-debugging-pipe`: JSON messages ended by a NUL byte, commands
//! on the browser's fd 3, replies and events on its fd 4):
//!
//! - a new tab or window on another site (a link with `target=_blank`,
//!   `window.open`) is closed, and the link opened in the system's default
//!   browser, as the built-in window does (see [crate::navigation]);
//! - prigh's own window (the app's, or one opened on prigh's site) going to
//!   another site is sent back to where it was, and the link opened in the
//!   default browser;
//! - a popup on prigh's site (Open in new window) is closed and its page
//!   opened in another app window instead: a popup has an address bar;
//! - the browser's own pages (a new tab or window: Ctrl+T, Ctrl+N; history,
//!   downloads, settings, view-source:, DevTools unless kept) and every page
//!   of another browser context (an incognito window: Ctrl+Shift+N) are
//!   closed, and prigh's window, which they took the focus from, activated;
//! - prigh's sites get their permissions (the clipboard, notifications, the
//!   microphone) up front, so the browser asks nothing, and downloads go to
//!   the downloads directory without a prompt.
//!
//! `about:blank` (a popup before it loads), `chrome-error:` (a page that
//! failed to load) and what the window would refuse are left alone.

use crate::navigation::{decide, origin, Decision};
use serde_json::{json, Value};
use std::collections::{HashMap, HashSet};

/// What prigh's sites may do without the browser asking.
pub const PERMISSIONS: &[&str] = &["clipboardReadWrite", "clipboardSanitizedWrite", "notifications", "audioCapture"];

/// What the shell does about a message.
#[derive(Debug, PartialEq, Eq)]
pub enum Effect {
    /// Write this command to the browser.
    Send(String),
    /// Open this link in the system's default browser.
    OpenExternally(String),
    /// Open this page of prigh's in another app window of the browser.
    OpenWindow(String),
    /// Tell the user (the app's log).
    Log(String),
}

#[derive(Default)]
struct Page {
    /// Where it last was on prigh's sites: it is one of prigh's windows.
    ours: Option<String>,
    /// Sent elsewhere already: its later events say nothing new.
    handled: Option<String>,
    /// Closed by us: its later events say nothing new.
    closing: bool,
}

/// The browser's pages, from its events.
#[derive(Default)]
pub struct Links {
    pages: HashMap<String, Page>,
    next_id: u64,
    /// Attaching to one of prigh's windows to send it back: the request's
    /// id, to the address to go back to.
    attaching: HashMap<u64, String>,
    /// The browser context of prigh's windows: the first page's.
    context: Option<String>,
    keep_devtools: bool,
    /// prigh's origins given their permissions.
    granted: HashSet<String>,
}

/// The browser's own pages, its Chromium-family relatives' too.
/// `chrome-error:` is not one: it is a page of prigh's that failed to load.
fn browser_page(url: &str, keep_devtools: bool) -> bool {
    let lower = url.to_ascii_lowercase();
    let schemes = [
        "chrome:",
        "chrome-search:",
        "chrome-untrusted:",
        "chrome-extension:",
        "view-source:",
        "edge:",
        "brave:",
        "vivaldi:",
        "opera:",
    ];
    schemes.iter().any(|s| lower.starts_with(s)) || (!keep_devtools && lower.starts_with("devtools:"))
}

impl Links {
    /// `keep_devtools`: leave DevTools open (F12, Ctrl+Shift+I).
    pub fn new(keep_devtools: bool) -> Links {
        Links { next_id: 1, keep_devtools, ..Links::default() }
    }

    /// The commands that start it: the events about every page, existing
    /// and new, and every change of its address; downloads to `downloads`.
    pub fn start(&mut self, downloads: &str) -> Vec<String> {
        let (_, discover) = self.command("Target.setDiscoverTargets", json!({ "discover": true }), None);
        let (_, download) = self.command(
            "Browser.setDownloadBehavior",
            json!({ "behavior": "allow", "downloadPath": downloads, "eventsEnabled": true }),
            None,
        );
        vec![discover, download]
    }

    fn command(&mut self, method: &str, params: Value, session: Option<&str>) -> (u64, String) {
        let id = self.next_id;
        self.next_id += 1;
        let mut msg = json!({ "id": id, "method": method, "params": params });
        if let Some(s) = session {
            msg["sessionId"] = json!(s);
        }
        (id, msg.to_string())
    }

    /// A message from the browser; `ours`: the origins of prigh's sites.
    pub fn on_message(&mut self, msg: &str, ours: &[String]) -> Vec<Effect> {
        let Ok(msg) = serde_json::from_str::<Value>(msg) else { return vec![] };
        let mut out = self.grant(ours);
        if let Some(id) = msg["id"].as_u64() {
            out.extend(self.on_reply(id, &msg));
            return out;
        }
        let params = &msg["params"];
        let info = &params["targetInfo"];
        match msg["method"].as_str() {
            Some("Target.targetCreated" | "Target.targetInfoChanged") if info["type"] == "page" => {
                if let (Some(target), Some(url)) = (info["targetId"].as_str(), info["url"].as_str()) {
                    let target = Target {
                        id: target,
                        url,
                        opener: info["openerId"].as_str(),
                        context: info["browserContextId"].as_str(),
                    };
                    out.extend(self.on_page(target, ours));
                }
            }
            Some("Target.targetDestroyed") => {
                if let Some(target) = params["targetId"].as_str() {
                    self.pages.remove(target);
                }
            }
            Some("Browser.downloadProgress") if params["state"] == "completed" => {
                if let Some(path) = params["filePath"].as_str() {
                    out.push(Effect::Log(format!("downloaded {path}")));
                }
            }
            _ => {}
        }
        out
    }

    fn grant(&mut self, ours: &[String]) -> Vec<Effect> {
        let new: Vec<String> = ours.iter().filter(|o| !self.granted.contains(*o)).cloned().collect();
        new.into_iter()
            .map(|o| {
                let (_, grant) =
                    self.command("Browser.grantPermissions", json!({ "origin": o, "permissions": PERMISSIONS }), None);
                self.granted.insert(o);
                Effect::Send(grant)
            })
            .collect()
    }

    fn on_page(&mut self, target: Target, ours: &[String]) -> Vec<Effect> {
        let Target { id, url, opener, context } = target;
        if self.pages.get(id).is_some_and(|p| p.closing) {
            return vec![];
        }
        if let Some(context) = context.filter(|c| !c.is_empty()) {
            let prighs = self.context.get_or_insert_with(|| context.to_string());
            // An incognito window.
            if prighs != context {
                return self.close(id, opener, "closed a page of an incognito window".to_string());
            }
        }
        let page = self.pages.entry(id.to_string()).or_default();
        if browser_page(url, self.keep_devtools) {
            return match page.ours.clone() {
                // One of prigh's windows: back to prigh's page.
                Some(back) if page.handled.as_deref() != Some(url) => {
                    page.handled = Some(url.to_string());
                    vec![Effect::Send(self.send_back(id, back))]
                }
                Some(_) => vec![],
                None => self.close(
                    id,
                    opener,
                    format!("closed the browser's own page {}", url.split(['?', '#']).next().unwrap_or(url)),
                ),
            };
        }
        match decide(url, ours) {
            Decision::Allow => {
                if origin(url).is_some_and(|o| ours.contains(&o)) {
                    if opener.is_some() && page.ours.is_none() {
                        // A popup on prigh's site: an app window instead.
                        page.closing = true;
                        let (_, close) = self.command("Target.closeTarget", json!({ "targetId": id }), None);
                        return vec![Effect::OpenWindow(url.to_string()), Effect::Send(close)];
                    }
                    page.ours = Some(url.to_string());
                    page.handled = None;
                }
                vec![]
            }
            Decision::External if page.handled.as_deref() != Some(url) => {
                page.handled = Some(url.to_string());
                match page.ours.clone() {
                    Some(back) => vec![Effect::OpenExternally(url.to_string()), Effect::Send(self.send_back(id, back))],
                    None => {
                        page.closing = true;
                        let (_, close) = self.command("Target.closeTarget", json!({ "targetId": id }), None);
                        vec![Effect::OpenExternally(url.to_string()), Effect::Send(close)]
                    }
                }
            }
            // A popup before it loads (about:blank, or no address yet), and
            // what the window would refuse.
            _ => vec![],
        }
    }

    /// Attaches to one of prigh's windows, to send it back to `back`
    /// (see [Links::on_reply]).
    fn send_back(&mut self, target: &str, back: String) -> String {
        let (id, attach) = self.command("Target.attachToTarget", json!({ "targetId": target, "flatten": true }), None);
        self.attaching.insert(id, back);
        attach
    }

    /// Closes a page of the browser's own, and activates the window of
    /// prigh's it took the focus from: the one that opened it, or else the
    /// only one there is.
    fn close(&mut self, target: &str, opener: Option<&str>, why: String) -> Vec<Effect> {
        self.pages.entry(target.to_string()).or_default().closing = true;
        let (_, close) = self.command("Target.closeTarget", json!({ "targetId": target }), None);
        let mut out = vec![Effect::Log(why), Effect::Send(close)];
        let prighs: Vec<&String> =
            self.pages.iter().filter(|(_, p)| p.ours.is_some() && !p.closing).map(|(id, _)| id).collect();
        let focus = match opener.filter(|o| prighs.iter().any(|p| p == o)) {
            Some(o) => Some(o.to_string()),
            None if prighs.len() == 1 => Some(prighs[0].clone()),
            None => None,
        };
        if let Some(focus) = focus {
            let (_, activate) = self.command("Target.activateTarget", json!({ "targetId": focus }), None);
            out.push(Effect::Send(activate));
        }
        out
    }

    fn on_reply(&mut self, id: u64, msg: &Value) -> Vec<Effect> {
        let Some(back) = self.attaching.remove(&id) else { return vec![] };
        let Some(session) = msg["result"]["sessionId"].as_str() else { return vec![] };
        let (_, navigate) = self.command("Page.navigate", json!({ "url": back }), Some(session));
        let (_, detach) = self.command("Target.detachFromTarget", json!({ "sessionId": session }), None);
        vec![Effect::Send(navigate), Effect::Send(detach)]
    }
}

struct Target<'a> {
    id: &'a str,
    url: &'a str,
    opener: Option<&'a str>,
    context: Option<&'a str>,
}

#[cfg(test)]
mod tests {
    use super::*;

    const PRIGHS: &str = "CTX";

    fn info(target: &str, url: &str, opener: Option<&str>, context: &str) -> Value {
        let mut info = json!({ "targetId": target, "type": "page", "url": url, "title": "", "attached": false,
                               "canAccessOpener": false, "browserContextId": context });
        if let Some(o) = opener {
            info["openerId"] = json!(o);
            info["canAccessOpener"] = json!(true);
        }
        info
    }

    fn event(method: &str, info: Value) -> String {
        json!({ "method": method, "params": { "targetInfo": info } }).to_string()
    }

    fn created(target: &str, url: &str) -> String {
        event("Target.targetCreated", info(target, url, None, PRIGHS))
    }

    fn changed(target: &str, url: &str) -> String {
        event("Target.targetInfoChanged", info(target, url, None, PRIGHS))
    }

    fn popup(method: &str, target: &str, url: &str) -> String {
        event(method, info(target, url, Some("A"), PRIGHS))
    }

    const OURS: [&str; 2] = ["http://127.0.0.1:7780", "https://ai.example.com"];

    /// A watcher whose origins have their permissions already (ids 1, 2).
    fn links() -> Links {
        let mut links = Links::new(false);
        run(&mut links, &["{}".to_string()]);
        links
    }

    /// The effects as JSON: `{"open": URL}`, `{"window": URL}`, `{"log":
    /// TEXT}`, or the command sent.
    fn run(links: &mut Links, msgs: &[String]) -> Vec<Value> {
        let ours = OURS.map(String::from);
        msgs.iter()
            .flat_map(|m| links.on_message(m, &ours))
            .map(|e| match e {
                Effect::Send(s) => serde_json::from_str(&s).unwrap(),
                Effect::OpenExternally(u) => json!({ "open": u }),
                Effect::OpenWindow(u) => json!({ "window": u }),
                Effect::Log(t) => json!({ "log": t }),
            })
            .collect()
    }

    fn close(id: u64, target: &str) -> Value {
        json!({ "id": id, "method": "Target.closeTarget", "params": { "targetId": target } })
    }

    fn activate(id: u64, target: &str) -> Value {
        json!({ "id": id, "method": "Target.activateTarget", "params": { "targetId": target } })
    }

    #[test]
    fn it_starts_with_discovery_and_downloads_and_grants_prighs_sites_their_permissions() {
        let mut links = Links::new(false);
        let start: Vec<Value> =
            links.start("/home/me/Downloads").iter().map(|c| serde_json::from_str(c).unwrap()).collect();
        assert_eq!(
            start,
            [
                json!({ "id": 1, "method": "Target.setDiscoverTargets", "params": { "discover": true } }),
                json!({ "id": 2, "method": "Browser.setDownloadBehavior",
                        "params": { "behavior": "allow", "downloadPath": "/home/me/Downloads", "eventsEnabled": true } }),
            ]
        );
        let grant = |id: u64, origin: &str| {
            json!({ "id": id, "method": "Browser.grantPermissions", "params": { "origin": origin,
                    "permissions": ["clipboardReadWrite", "clipboardSanitizedWrite", "notifications", "audioCapture"] } })
        };
        // On the first message, each origin once; a new one when it comes.
        let mut out = run(&mut links, &[json!({ "id": 1, "result": {} }).to_string(), "{}".to_string()]);
        out.extend(
            links.on_message("{}", &[OURS[0].to_string(), "https://other.example".to_string()]).into_iter().map(|e| {
                match e {
                    Effect::Send(s) => serde_json::from_str::<Value>(&s).unwrap(),
                    e => panic!("{e:?}"),
                }
            }),
        );
        assert_eq!(out, [grant(3, OURS[0]), grant(4, OURS[1]), grant(5, "https://other.example")]);
    }

    #[test]
    fn a_finished_download_is_logged() {
        let mut links = links();
        let progress = |state: &str, path: Option<&str>| {
            let mut p = json!({ "guid": "g", "totalBytes": 5, "receivedBytes": 5, "state": state });
            if let Some(path) = path {
                p["filePath"] = json!(path);
            }
            json!({ "method": "Browser.downloadProgress", "params": p }).to_string()
        };
        assert_eq!(
            run(&mut links, &[progress("inProgress", None), progress("completed", Some("/home/me/Downloads/a.txt"))]),
            [json!({ "log": "downloaded /home/me/Downloads/a.txt" })]
        );
    }

    #[test]
    fn a_new_tab_elsewhere_is_closed_and_its_link_opened_in_the_default_browser() {
        let mut links = links();
        let app = created("A", "http://127.0.0.1:7780/#desktop=k&shell=browser");
        // A link with target=_blank: the new tab starts blank, then goes.
        let blank = popup("Target.targetCreated", "B", "about:blank");
        let gone = popup("Target.targetInfoChanged", "B", "https://github.com/evan-a-w/prigh");
        assert_eq!(
            run(&mut links, &[app, blank, gone.clone(), gone]),
            [json!({ "open": "https://github.com/evan-a-w/prigh" }), close(3, "B")]
        );
    }

    #[test]
    fn prighs_windows_and_pages_that_failed_to_load_stay() {
        let mut links = links();
        let msgs = [
            created("A", "http://127.0.0.1:7780/?session=s1"),
            // The backend's site, in another app window (a second launch).
            created("B", "https://ai.example.com/?session=s2#app=desktop"),
            changed("A", "chrome-error://chromewebdata/"),
            // A popup before it has an address.
            popup("Target.targetCreated", "C", ""),
            popup("Target.targetInfoChanged", "C", "about:blank"),
            event(
                "Target.targetCreated",
                json!({ "targetId": "E", "type": "service_worker",
                  "url": "https://elsewhere.example/sw.js", "browserContextId": "OTHER" }),
            ),
            event(
                "Target.targetCreated",
                json!({ "targetId": "F", "type": "browser_ui",
                  "url": "chrome://webui-toolbar.top-chrome/", "browserContextId": PRIGHS }),
            ),
            event(
                "Target.targetCreated",
                json!({ "targetId": "G", "type": "iframe",
                  "url": "https://elsewhere.example/embed", "browserContextId": PRIGHS }),
            ),
        ];
        assert_eq!(run(&mut links, &msgs), Vec::<Value>::new());
    }

    #[test]
    fn a_popup_on_prighs_site_is_reopened_in_an_app_window() {
        let mut links = links();
        let url = "http://127.0.0.1:7780/?session=s2#desktop=k&shell=browser";
        let msgs = [
            created("A", "http://127.0.0.1:7780/?session=s1"),
            // Open in new window: no address, then prigh's site.
            popup("Target.targetCreated", "B", ""),
            popup("Target.targetInfoChanged", "B", url),
            popup("Target.targetInfoChanged", "B", url),
            // The backend's site too; and one that has its address at once.
            popup("Target.targetCreated", "C", "https://ai.example.com/?session=s3"),
        ];
        assert_eq!(
            run(&mut links, &msgs),
            [
                json!({ "window": url }),
                close(3, "B"),
                json!({ "window": "https://ai.example.com/?session=s3" }),
                close(4, "C"),
            ]
        );
        // The app window that opens has no opener: it stays.
        assert_eq!(run(&mut links, &[created("D", url)]), Vec::<Value>::new());
    }

    #[test]
    fn the_browsers_own_pages_are_closed_and_prighs_window_activated() {
        let mut links = links();
        let mut out = run(&mut links, &[created("A", "http://127.0.0.1:7780/?session=s1")]);
        // Ctrl+T and Ctrl+N (a tabbed window on the new tab page), Ctrl+H,
        // Ctrl+J, Ctrl+U, F12 (DevTools: first no address), other
        // Chromium-family browsers' and older new tab pages.
        for (target, url) in [
            ("T", "chrome://newtab/"),
            ("H", "chrome://history/"),
            ("J", "chrome://downloads/"),
            ("U", "view-source:http://127.0.0.1:7780/?session=s1"),
            ("D", ""),
            ("D", "devtools://devtools/bundled/devtools_app.html?targetType=tab"),
            ("N", "chrome://new-tab-page/"),
            ("L", "chrome-search://local-ntp/local-ntp.html"),
            ("X", "chrome-untrusted://new-tab-page/one-google-bar"),
            ("S", "brave://settings/"),
            ("E", "edge://newtab/"),
            ("V", "vivaldi://startpage/"),
            ("P", "chrome-extension://abc/options.html"),
        ] {
            out.extend(run(&mut links, &[created(target, url)]));
        }
        // Closed already: its later events change nothing.
        out.extend(run(&mut links, &[changed("T", "chrome://newtab/")]));
        let mut expected = vec![];
        let mut id = 3;
        for (target, page) in [
            ("T", "chrome://newtab/"),
            ("H", "chrome://history/"),
            ("J", "chrome://downloads/"),
            ("U", "view-source:http://127.0.0.1:7780/"),
            ("D", "devtools://devtools/bundled/devtools_app.html"),
            ("N", "chrome://new-tab-page/"),
            ("L", "chrome-search://local-ntp/local-ntp.html"),
            ("X", "chrome-untrusted://new-tab-page/one-google-bar"),
            ("S", "brave://settings/"),
            ("E", "edge://newtab/"),
            ("V", "vivaldi://startpage/"),
            ("P", "chrome-extension://abc/options.html"),
        ] {
            expected.push(json!({ "log": format!("closed the browser's own page {page}") }));
            expected.push(close(id, target));
            expected.push(activate(id + 1, "A"));
            id += 2;
        }
        assert_eq!(out, expected);
    }

    #[test]
    fn devtools_stays_when_kept() {
        let mut links = Links::new(true);
        run(&mut links, &["{}".to_string()]);
        let out = run(
            &mut links,
            &[
                created("A", "http://127.0.0.1:7780/"),
                created("D", "devtools://devtools/bundled/devtools_app.html"),
                created("H", "chrome://history/"),
            ],
        );
        assert_eq!(
            out,
            [json!({ "log": "closed the browser's own page chrome://history/" }), close(3, "H"), activate(4, "A")]
        );
    }

    #[test]
    fn an_incognito_windows_pages_are_closed() {
        let mut links = links();
        let incognito = json!({ "log": "closed a page of an incognito window" });
        let otr = |method: &str, target: &str, url: &str| event(method, info(target, url, None, "INCOGNITO"));
        let out = run(
            &mut links,
            &[
                created("A", "http://127.0.0.1:7780/"),
                otr("Target.targetCreated", "I", "chrome://newtab/"),
                otr("Target.targetInfoChanged", "I", "chrome://newtab/"),
                // Even on prigh's site, or before it has an address.
                otr("Target.targetCreated", "J", "http://127.0.0.1:7780/"),
                otr("Target.targetCreated", "K", ""),
            ],
        );
        assert_eq!(
            out,
            [
                incognito.clone(),
                close(3, "I"),
                activate(4, "A"),
                incognito.clone(),
                close(5, "J"),
                activate(6, "A"),
                incognito,
                close(7, "K"),
                activate(8, "A"),
            ]
        );
    }

    #[test]
    fn with_several_of_prighs_windows_the_opener_is_activated_or_none() {
        let mut links = links();
        let out = run(
            &mut links,
            &[
                created("A", "http://127.0.0.1:7780/"),
                created("B", "http://127.0.0.1:7780/?session=s2"),
                event("Target.targetCreated", info("O", "chrome://history/", Some("B"), PRIGHS)),
                created("T", "chrome://newtab/"),
            ],
        );
        assert_eq!(
            out,
            [
                json!({ "log": "closed the browser's own page chrome://history/" }),
                close(3, "O"),
                activate(4, "B"),
                json!({ "log": "closed the browser's own page chrome://newtab/" }),
                close(5, "T"),
            ]
        );
    }

    #[test]
    fn prighs_window_going_to_a_browser_page_goes_back() {
        let mut links = links();
        let out = run(
            &mut links,
            &[
                created("A", "http://127.0.0.1:7780/?session=s1"),
                changed("A", "chrome://settings/"),
                changed("A", "chrome://settings/"),
            ],
        );
        assert_eq!(
            out,
            [json!({ "id": 3, "method": "Target.attachToTarget", "params": { "targetId": "A", "flatten": true } })]
        );
    }

    #[test]
    fn prighs_window_going_elsewhere_goes_back() {
        let mut links = links();
        let mut out = run(
            &mut links,
            &[created("A", "http://127.0.0.1:7780/?session=s1"), changed("A", "https://example.com/docs")],
        );
        out.extend(run(&mut links, &[json!({ "id": 3, "result": { "sessionId": "S" } }).to_string()]));
        assert_eq!(
            out,
            [
                json!({ "open": "https://example.com/docs" }),
                json!({ "id": 3, "method": "Target.attachToTarget", "params": { "targetId": "A", "flatten": true } }),
                json!({ "id": 4, "method": "Page.navigate", "sessionId": "S",
                        "params": { "url": "http://127.0.0.1:7780/?session=s1" } }),
                json!({ "id": 5, "method": "Target.detachFromTarget", "params": { "sessionId": "S" } }),
            ]
        );
        // Back on prigh's page, it is prigh's again: a later link goes too.
        let again = run(
            &mut links,
            &[changed("A", "http://127.0.0.1:7780/?session=s1"), changed("A", "https://example.com/docs")],
        );
        assert_eq!(again.len(), 2);
    }

    #[test]
    fn mail_links_go_to_the_default_mail_handler() {
        let mut links = links();
        assert_eq!(
            run(&mut links, &[created("B", "mailto:me@example.com")]),
            [json!({ "open": "mailto:me@example.com" }), close(3, "B")]
        );
    }

    #[test]
    fn a_closed_page_is_forgotten_and_noise_is_ignored() {
        let mut links = links();
        let msgs = [
            created("A", "http://127.0.0.1:7780/"),
            json!({ "method": "Target.targetDestroyed", "params": { "targetId": "A" } }).to_string(),
            "not json".to_string(),
            json!({ "id": 1, "result": {} }).to_string(),
            json!({ "id": 99, "result": { "sessionId": "S" } }).to_string(),
        ];
        assert_eq!(run(&mut links, &msgs), Vec::<Value>::new());
        // Forgotten: a page with that id now is a new one.
        assert_eq!(run(&mut links, &[created("A", "https://example.com/")]).len(), 2);
    }
}
