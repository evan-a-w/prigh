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
//! - prigh's own window (the app's, or one opened on prigh's site) following
//!   a link or its page's script to another site (an `anchorClick` or
//!   `scriptInitiated` navigation to that site, seen on a DevTools session
//!   of each of prigh's windows: `Page.frameRequestedNavigation`) is sent
//!   back to where it was, and the link opened in the default browser;
//! - prigh's own window going to another site otherwise (an HTTP redirect,
//!   which the page does not request, a form's, a refresh: a sign-in proxy
//!   in front of the backend's site, then its identity provider) stays
//!   there, through every hop, until it is back on prigh's site: the
//!   profile keeps the proxy's cookie;
//! - whatever a site does, a window opens a site in the default browser at
//!   most [crate::navigation::OPEN_BURST] times within
//!   [crate::navigation::OPEN_GAP]: a link past that stays in the window
//!   (said once);
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

use crate::navigation::{decide, origin, Decision, OpenGuard, Remote};
use serde_json::{json, Value};
use std::collections::{HashMap, HashSet};
use std::time::{Duration, Instant};

/// A window away signing in that goes to this many pages within
/// [ROUND_WINDOW] is going round in circles: it is sent back to prigh's.
pub const ROUND_HOPS: usize = 12;
pub const ROUND_WINDOW: Duration = Duration::from_secs(10);

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
    /// Opened by another page (a link with `target=_blank`, `window.open`),
    /// not one of the app's launches.
    popup: bool,
    /// Sent elsewhere already: its later events say nothing new.
    handled: Option<String>,
    /// Closed by us: its later events say nothing new.
    closing: bool,
    /// Its DevTools session, once attached.
    session: Option<String>,
    attaching: bool,
    /// The last navigation its page asked for: the reason and the address.
    requested: Option<(String, String)>,
    /// The site it went to on its own (a sign-in), until it is back.
    away: Option<String>,
    /// When it went from page to page while away, lately: a sign-in that
    /// goes round and round (a cookie the site does not take) is stopped.
    hops: Vec<Instant>,
    round_told: bool,
    /// The remote site's cookies were given to it since it was last there.
    /// The backend's site's cookies it was given: given again only when
    /// they are new (a sign-in since), so a site that does not take them
    /// sends it to prigh's sign-in page instead of round and round.
    cookies_given: Option<Vec<(String, String)>>,
}

/// The browser's pages, from its events.
#[derive(Default)]
pub struct Links {
    pages: HashMap<String, Page>,
    next_id: u64,
    /// Attaching to one of prigh's windows: the request's id, to the window
    /// and the address to send it back to, if any.
    attaching: HashMap<u64, (String, Option<String>)>,
    /// The DevTools sessions on prigh's windows, to their windows.
    sessions: HashMap<String, String>,
    /// Openings in the default browser, by window (a popup's: its opener).
    opened: OpenGuard,
    /// Setting the remote site's cookies: the request's id, to the windows
    /// to load it in then.
    setting_cookies: HashMap<u64, Vec<(String, String)>>,
    remote: Option<Remote>,
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

    /// A message from the browser at `now`; `ours`: the origins of prigh's
    /// sites.
    pub fn on_message(&mut self, msg: &str, ours: &[String], now: Instant) -> Vec<Effect> {
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
                    out.extend(self.on_page(target, ours, now));
                }
            }
            Some("Target.targetDestroyed") => {
                if let Some(target) = params["targetId"].as_str() {
                    if let Some(Page { session: Some(s), .. }) = self.pages.remove(target) {
                        self.sessions.remove(&s);
                    }
                    self.opened.forget(target);
                }
            }
            Some("Target.detachedFromTarget") => {
                if let Some(target) = params["sessionId"].as_str().and_then(|s| self.sessions.remove(s)) {
                    if let Some(page) = self.pages.get_mut(&target) {
                        page.session = None;
                    }
                }
            }
            // Its main frame's own navigations (not an iframe's, not a new
            // window's).
            Some("Page.frameRequestedNavigation") if params["disposition"] == "currentTab" => {
                let target = msg["sessionId"].as_str().and_then(|s| self.sessions.get(s));
                if let (Some(target), Some(reason), Some(url)) =
                    (target, params["reason"].as_str(), params["url"].as_str())
                {
                    if params["frameId"].as_str() == Some(target.as_str()) {
                        if let Some(page) = self.pages.get_mut(target) {
                            page.requested = Some((reason.to_string(), url.to_string()));
                        }
                    }
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

    fn on_page(&mut self, target: Target, ours: &[String], now: Instant) -> Vec<Effect> {
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
        let new = !self.pages.contains_key(id);
        let page = self.pages.entry(id.to_string()).or_default();
        if new {
            page.popup = opener.is_some();
        }
        if browser_page(url, self.keep_devtools) {
            return match page.ours.clone() {
                // One of prigh's windows: back to prigh's page.
                Some(back) if page.handled.as_deref() != Some(url) => {
                    page.handled = Some(url.to_string());
                    self.send_back(id, back)
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
                    if page.popup && page.ours.is_none() {
                        // A popup on prigh's site: an app window instead.
                        page.closing = true;
                        let (_, close) = self.command("Target.closeTarget", json!({ "targetId": id }), None);
                        return vec![Effect::OpenWindow(url.to_string()), Effect::Send(close)];
                    }
                    page.ours = Some(url.to_string());
                    page.handled = None;
                    page.requested = None;
                    let mut out = vec![];
                    if let Some(away) = page.away.take() {
                        out.push(Effect::Log(format!("prigh's window is back from {away}")));
                    }
                    if page.session.is_none() && !page.attaching {
                        out.extend(self.attach(id, None));
                    }
                    return out;
                }
                vec![]
            }
            Decision::External if page.handled.as_deref() != Some(url) => {
                page.handled = Some(url.to_string());
                if page.popup && page.ours.is_none() {
                    page.closing = true;
                    let (_, close) = self.command("Target.closeTarget", json!({ "targetId": id }), None);
                    let mut out = self.open_externally(opener.unwrap_or(id), url, now);
                    out.push(Effect::Send(close));
                    return out;
                }
                // One of prigh's windows, or the app's launch on its way
                // there; while away, wherever it goes is the sign-in's.
                if let Some(away) = page.away.clone() {
                    page.hops.retain(|at| now.saturating_duration_since(*at) < ROUND_WINDOW);
                    page.hops.push(now);
                    if page.hops.len() < ROUND_HOPS {
                        return vec![];
                    }
                    page.hops.clear();
                    // prigh's sign-in page, which stays put; without one,
                    // prigh's site would only send it round again.
                    let Some(signin) = self.remote.as_ref().and_then(|r| r.signin_page.clone()) else {
                        if std::mem::replace(&mut page.round_told, true) {
                            return vec![];
                        }
                        return vec![Effect::Log(format!(
                            "prigh's window keeps going round {away}: its sign-in does not stick"
                        ))];
                    };
                    let mut out = vec![Effect::Log(format!(
                        "prigh's window keeps going round {away} (its sign-in does not stick): showing prigh's sign-in page"
                    ))];
                    out.extend(self.send_back(id, signin));
                    return out;
                }
                let site = origin(url).unwrap_or_else(|| url.to_string());
                let link = page
                    .requested
                    .take()
                    .is_some_and(|(reason, to)| is_link(&reason) && origin(&to).as_ref() == Some(&site));
                match page.ours.clone() {
                    Some(back) if link => {
                        let mut out = self.open_externally(id, url, now);
                        if out.iter().any(|e| matches!(e, Effect::OpenExternally(_))) {
                            out.extend(self.send_back(id, back));
                        } else {
                            // Too soon again: it stays.
                            self.pages.get_mut(id).unwrap().away = Some(site);
                        }
                        out
                    }
                    ours => {
                        let remote = self.remote.clone().filter(|r| match &ours {
                            Some(o) => origin(o).is_some() && origin(o) == origin(&r.url),
                            // The app's launch, on its way to the site.
                            None => true,
                        });
                        let page = self.pages.get_mut(id).unwrap();
                        match remote {
                            Some(r) if page.cookies_given.as_ref() != Some(&r.cookies) && !r.cookies.is_empty() => {
                                page.cookies_given = Some(r.cookies.clone());
                                let back = ours.unwrap_or_else(|| r.url.clone());
                                let mut out = vec![Effect::Log(format!(
                                    "prigh's window went to {site}: giving it the sign-in to {}",
                                    origin(&r.url).unwrap_or_default()
                                ))];
                                out.extend(self.set_cookies(&r, vec![(id.to_string(), back)]));
                                out
                            }
                            Some(Remote { signin_page: Some(signin), url, .. }) => {
                                // Until it is on prigh's sign-in page.
                                page.away = Some(site.clone());
                                let mut out = vec![Effect::Log(format!(
                                    "prigh's window went to {site}: {} wants a sign-in, showing prigh's",
                                    origin(&url).unwrap_or_default()
                                ))];
                                out.extend(self.send_back(id, signin));
                                out
                            }
                            _ => {
                                page.away = Some(site.clone());
                                vec![Effect::Log(format!(
                                    "prigh's window went to {site} on its own (a sign-in?): it stays there until it is back"
                                ))]
                            }
                        }
                    }
                }
            }
            // A popup before it loads (about:blank, or no address yet), and
            // what the window would refuse.
            _ => vec![],
        }
    }

    /// Opens `url` in the default browser for `window`, unless that window
    /// opened its site less than [crate::navigation::OPEN_GAP] ago.
    fn open_externally(&mut self, window: &str, url: &str, now: Instant) -> Vec<Effect> {
        match self.opened.check(window, url, now) {
            Ok(()) => vec![Effect::OpenExternally(url.to_string())],
            Err(log) => log.into_iter().map(Effect::Log).collect(),
        }
    }

    /// The backend's site, as the sidecar says to show it now. With its
    /// cookies (signed in through the default browser), prigh's windows on
    /// the sidecar's sign-in page get them and go there; `here` (the user
    /// chose to sign in in the window), they go there to sign in.
    pub fn set_remote(&mut self, remote: Remote, here: bool) -> Vec<Effect> {
        let signin = remote.signin_page.as_deref().and_then(origin);
        let give = !remote.cookies.is_empty();
        let windows: Vec<(String, String)> = self
            .pages
            .iter_mut()
            .filter(|(_, p)| (give || here) && !p.closing && !p.popup && p.away.is_none())
            .filter(|(_, p)| signin.is_some() && p.ours.as_deref().and_then(origin) == signin)
            .map(|(id, p)| {
                if give {
                    // On its way there: redirected away, it is not signed in.
                    p.cookies_given = Some(remote.cookies.clone());
                    p.ours = Some(remote.url.clone());
                }
                (id.clone(), remote.url.clone())
            })
            .collect();
        let out = if give {
            self.set_cookies(&remote, windows)
        } else {
            windows.into_iter().flat_map(|(target, url)| self.send_back(&target, url)).collect()
        };
        self.remote = Some(remote);
        out
    }

    fn set_cookies(&mut self, remote: &Remote, then: Vec<(String, String)>) -> Vec<Effect> {
        let secure = remote.url.to_ascii_lowercase().starts_with("https:");
        let site = origin(&remote.url).map(|o| o + "/").unwrap_or_else(|| remote.url.clone());
        let cookies: Vec<Value> = remote
            .cookies
            .iter()
            .map(|(name, value)| {
                json!({ "name": name, "value": value, "url": site, "path": "/", "secure": secure, "httpOnly": true })
            })
            .collect();
        let (id, set) = self.command("Storage.setCookies", json!({ "cookies": cookies }), None);
        self.setting_cookies.insert(id, then);
        vec![Effect::Send(set)]
    }

    /// Sends one of prigh's windows back to `back`: on its session, or once
    /// attached (see [Links::on_reply]).
    fn send_back(&mut self, target: &str, back: String) -> Vec<Effect> {
        match self.pages.get(target).and_then(|p| p.session.clone()) {
            Some(session) => {
                let (_, navigate) = self.command("Page.navigate", json!({ "url": back }), Some(&session));
                vec![Effect::Send(navigate)]
            }
            None => {
                if let Some(entry) = self.attaching.values_mut().find(|(t, _)| t == target) {
                    entry.1 = Some(back);
                    return vec![];
                }
                self.attach(target, Some(back))
            }
        }
    }

    /// A session on one of prigh's windows, for the navigations its page
    /// asks for.
    fn attach(&mut self, target: &str, back: Option<String>) -> Vec<Effect> {
        if let Some(page) = self.pages.get_mut(target) {
            page.attaching = true;
        }
        let (id, attach) = self.command("Target.attachToTarget", json!({ "targetId": target, "flatten": true }), None);
        self.attaching.insert(id, (target.to_string(), back));
        vec![Effect::Send(attach)]
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
        if let Some(then) = self.setting_cookies.remove(&id) {
            return then.into_iter().flat_map(|(target, url)| self.send_back(&target, url)).collect();
        }
        let Some((target, back)) = self.attaching.remove(&id) else { return vec![] };
        let Some(page) = self.pages.get_mut(&target) else { return vec![] };
        page.attaching = false;
        let Some(session) = msg["result"]["sessionId"].as_str() else { return vec![] };
        page.session = Some(session.to_string());
        self.sessions.insert(session.to_string(), target);
        let (_, enable) = self.command("Page.enable", json!({}), Some(session));
        let mut out = vec![Effect::Send(enable)];
        if let Some(back) = back {
            let (_, navigate) = self.command("Page.navigate", json!({ "url": back }), Some(session));
            out.push(Effect::Send(navigate));
        }
        out
    }
}
struct Target<'a> {
    id: &'a str,
    url: &'a str,
    opener: Option<&'a str>,
    context: Option<&'a str>,
}

/// A navigation the page asked for that is a link out, not a sign-in.
fn is_link(reason: &str) -> bool {
    matches!(reason, "anchorClick" | "scriptInitiated")
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

    fn base() -> Instant {
        static BASE: std::sync::OnceLock<Instant> = std::sync::OnceLock::new();
        *BASE.get_or_init(Instant::now)
    }

    fn json_of(effects: Vec<Effect>) -> Vec<Value> {
        effects
            .into_iter()
            .map(|e| match e {
                Effect::Send(s) => serde_json::from_str(&s).unwrap(),
                Effect::OpenExternally(u) => json!({ "open": u }),
                Effect::OpenWindow(u) => json!({ "window": u }),
                Effect::Log(t) => json!({ "log": t }),
            })
            .collect()
    }

    /// The effects as JSON: `{"open": URL}`, `{"window": URL}`, `{"log":
    /// TEXT}`, or the command sent, as the browser gets them.
    fn run_raw(links: &mut Links, msgs: &[String], secs: u64) -> Vec<Value> {
        let ours = OURS.map(String::from);
        let now = base() + std::time::Duration::from_secs(secs);
        json_of(msgs.iter().flat_map(|m| links.on_message(m, &ours, now)).collect())
    }

    /// [run_raw], with each window's session attached as asked (session
    /// `S-TARGET`) and the commands' ids left out: those that attach and
    /// enable its events are not shown.
    fn run_at(links: &mut Links, msgs: &[String], secs: u64) -> Vec<Value> {
        let mut out = vec![];
        let mut queue: Vec<String> = msgs.to_vec();
        queue.reverse();
        while let Some(msg) = queue.pop() {
            let mut replies = vec![];
            for mut effect in run_raw(links, &[msg], secs) {
                if effect["method"] == "Target.attachToTarget" {
                    let session = format!("S-{}", effect["params"]["targetId"].as_str().unwrap());
                    replies.push(json!({ "id": effect["id"], "result": { "sessionId": session } }).to_string());
                    continue;
                }
                if effect["method"] == "Page.enable" {
                    continue;
                }
                if let Some(o) = effect.as_object_mut() {
                    o.remove("id");
                }
                out.push(effect);
            }
            replies.reverse();
            queue.extend(replies);
        }
        out
    }

    fn run(links: &mut Links, msgs: &[String]) -> Vec<Value> {
        run_at(links, msgs, 0)
    }

    fn close(target: &str) -> Value {
        json!({ "method": "Target.closeTarget", "params": { "targetId": target } })
    }

    fn activate(target: &str) -> Value {
        json!({ "method": "Target.activateTarget", "params": { "targetId": target } })
    }

    fn navigate(target: &str, url: &str) -> Value {
        json!({ "method": "Page.navigate", "sessionId": format!("S-{target}"), "params": { "url": url } })
    }

    /// The page of window `target` asks to go to `url`, for `reason`.
    fn requested(target: &str, reason: &str, url: &str) -> String {
        json!({ "method": "Page.frameRequestedNavigation", "sessionId": format!("S-{target}"),
                "params": { "frameId": target, "reason": reason, "url": url, "disposition": "currentTab" } })
        .to_string()
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
        let mut out = run_raw(&mut links, &[json!({ "id": 1, "result": {} }).to_string(), "{}".to_string()], 0);
        out.extend(
            links
                .on_message("{}", &[OURS[0].to_string(), "https://other.example".to_string()], base())
                .into_iter()
                .map(|e| match e {
                    Effect::Send(s) => serde_json::from_str::<Value>(&s).unwrap(),
                    e => panic!("{e:?}"),
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
            [json!({ "open": "https://github.com/evan-a-w/prigh" }), close("B")]
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
                close("B"),
                json!({ "window": "https://ai.example.com/?session=s3" }),
                close("C"),
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
            expected.push(close(target));
            expected.push(activate("A"));
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
            [json!({ "log": "closed the browser's own page chrome://history/" }), close("H"), activate("A")]
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
                close("I"),
                activate("A"),
                incognito.clone(),
                close("J"),
                activate("A"),
                incognito,
                close("K"),
                activate("A"),
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
                close("O"),
                activate("B"),
                json!({ "log": "closed the browser's own page chrome://newtab/" }),
                close("T"),
            ]
        );
    }

    #[test]
    fn prighs_windows_get_a_session_for_their_pages_navigations() {
        let mut links = links();
        let out =
            run_raw(&mut links, &[created("A", "http://127.0.0.1:7780/"), changed("A", "http://127.0.0.1:7780/")], 0);
        assert_eq!(
            out,
            [json!({ "id": 3, "method": "Target.attachToTarget", "params": { "targetId": "A", "flatten": true } })]
        );
        let out = run_raw(&mut links, &[json!({ "id": 3, "result": { "sessionId": "S" } }).to_string()], 0);
        assert_eq!(out, [json!({ "id": 4, "method": "Page.enable", "sessionId": "S", "params": {} })]);
        // Detached (DevTools' own window took it?): attached again when next
        // on prigh's site.
        let detached =
            json!({ "method": "Target.detachedFromTarget", "params": { "sessionId": "S", "targetId": "A" } });
        let out = run_raw(&mut links, &[detached.to_string(), changed("A", "http://127.0.0.1:7780/?session=s")], 0);
        assert_eq!(
            out,
            [json!({ "id": 5, "method": "Target.attachToTarget", "params": { "targetId": "A", "flatten": true } })]
        );
    }

    #[test]
    fn prighs_window_going_to_a_browser_page_goes_back() {
        let mut links = links();
        let out = run_raw(
            &mut links,
            &[
                created("A", "http://127.0.0.1:7780/?session=s1"),
                changed("A", "chrome://settings/"),
                changed("A", "chrome://settings/"),
                json!({ "id": 3, "result": { "sessionId": "S" } }).to_string(),
            ],
            0,
        );
        // Not attached yet: once it is.
        assert_eq!(
            out,
            [
                json!({ "id": 3, "method": "Target.attachToTarget", "params": { "targetId": "A", "flatten": true } }),
                json!({ "id": 4, "method": "Page.enable", "sessionId": "S", "params": {} }),
                json!({ "id": 5, "method": "Page.navigate", "sessionId": "S",
                        "params": { "url": "http://127.0.0.1:7780/?session=s1" } }),
            ]
        );
    }

    #[test]
    fn a_link_in_prighs_window_to_another_site_opens_in_the_default_browser() {
        let mut links = links();
        let out = run(
            &mut links,
            &[
                created("A", "http://127.0.0.1:7780/?session=s1"),
                requested("A", "anchorClick", "https://example.com/docs"),
                changed("A", "https://example.com/docs"),
                changed("A", "https://example.com/docs"),
            ],
        );
        assert_eq!(
            out,
            [json!({ "open": "https://example.com/docs" }), navigate("A", "http://127.0.0.1:7780/?session=s1")]
        );
        // Back on prigh's page, it is prigh's again: a later link goes too
        // (another site, or the same after a while).
        let again = |links: &mut Links, to: &str, secs| {
            run_at(
                links,
                &[
                    changed("A", "http://127.0.0.1:7780/?session=s1"),
                    // The page's script (`location.href = ...`) is one too.
                    requested("A", if secs == 1 { "scriptInitiated" } else { "anchorClick" }, to),
                    changed("A", to),
                ],
                secs,
            )
        };
        assert_eq!(
            again(&mut links, "https://other.example/", 1),
            [json!({ "open": "https://other.example/" }), navigate("A", "http://127.0.0.1:7780/?session=s1")]
        );
        assert_eq!(
            again(&mut links, "https://example.com/more", 6),
            [json!({ "open": "https://example.com/more" }), navigate("A", "http://127.0.0.1:7780/?session=s1")]
        );
        // An iframe's link, or one opening a new window, is not the window's.
        let out = run_at(
            &mut links,
            &[
                changed("A", "http://127.0.0.1:7780/?session=s1"),
                json!({ "method": "Page.frameRequestedNavigation", "sessionId": "S-A",
                        "params": { "frameId": "F", "reason": "anchorClick", "url": "https://x.example/", "disposition": "currentTab" } })
                .to_string(),
                json!({ "method": "Page.frameRequestedNavigation", "sessionId": "S-A",
                        "params": { "frameId": "A", "reason": "anchorClick", "url": "https://x.example/", "disposition": "newTab" } })
                .to_string(),
                changed("A", "https://x.example/"),
            ],
            20,
        );
        assert_eq!(
            out,
            [
                json!({ "log": "prigh's window went to https://x.example on its own (a sign-in?): it stays there until it is back" })
            ]
        );
    }

    #[test]
    fn prighs_window_signs_in_to_a_proxy_in_the_window() {
        let mut links = links();
        // The backend's site redirects to the proxy's login, which goes to an
        // identity provider (a link there), whose form posts back; then the
        // proxy's callback and back to the site.
        let out = run(
            &mut links,
            &[
                created("A", "https://ai.example.com/#app=desktop&host=h&shell=browser"),
                requested("A", "reload", "https://ai.example.com/#app=desktop&host=h&shell=browser"),
                changed("A", "https://login.example.com/flow/?state=1#app=desktop&host=h&shell=browser"),
                requested("A", "anchorClick", "https://accounts.idp.example/o/auth?x=1"),
                changed("A", "https://accounts.idp.example/o/auth?x=1"),
                requested("A", "formSubmissionPost", "https://login.example.com/callback?code=c"),
                changed("A", "https://login.example.com/callback?code=c"),
                requested("A", "scriptInitiated", "https://ai.example.com/"),
                changed("A", "https://ai.example.com/#app=desktop&host=h&shell=browser"),
            ],
        );
        assert_eq!(
            out,
            [
                json!({ "log": "prigh's window went to https://login.example.com on its own (a sign-in?): it stays there until it is back" }),
                json!({ "log": "prigh's window is back from https://login.example.com" }),
            ]
        );
        // The app's launch, redirected before it was ever on the site.
        let out = run(&mut links, &[created("B", "https://login.example.com/flow/?state=2")]);
        assert_eq!(
            out,
            [
                json!({ "log": "prigh's window went to https://login.example.com on its own (a sign-in?): it stays there until it is back" })
            ]
        );
    }

    #[test]
    fn a_site_that_keeps_redirecting_has_a_bounded_number_of_effects() {
        const SIGNIN: &str = "http://127.0.0.1:7780/#desktop=k&signin=1";
        let mut links = self::links();
        let mut out = run(&mut links, &[created("A", "https://ai.example.com/")]);
        // Each time to a fresh address, for a minute: nothing opens, the
        // window is not sent back, the log says it once.
        for i in 0..60 {
            out.extend(run_at(
                &mut links,
                &[
                    changed("A", &format!("https://login.example.com/start?state={i}")),
                    changed("A", &format!("https://login.example.com/flow?state={i}")),
                ],
                i,
            ));
        }
        assert_eq!(
            out,
            [
                json!({ "log": "prigh's window went to https://login.example.com on its own (a sign-in?): it stays there until it is back" }),
                json!({ "log": "prigh's window keeps going round https://login.example.com: its sign-in does not stick" })
            ]
        );
        // With prigh's sign-in page, it goes there (which stays put): here
        // from the sidecar's page, as a page's script would go.
        let mut links = self::links();
        links.set_remote(
            Remote { url: "https://ai.example.com/".into(), cookies: vec![], signin_page: Some(SIGNIN.into()) },
            false,
        );
        let mut out = run(&mut links, &[created("A", "http://127.0.0.1:7780/?session=s1")]);
        for i in 0..=ROUND_HOPS {
            out.extend(run_at(&mut links, &[changed("A", &format!("https://login.example.com/start?state={i}"))], 0));
        }
        let navigates: Vec<Value> =
            out.iter().filter(|e| e["method"] == "Page.navigate").map(|e| e["params"]["url"].clone()).collect();
        assert_eq!(navigates, [SIGNIN], "{out:#?}");
        out.extend(run(&mut links, &[changed("A", SIGNIN)]));
        assert_eq!(
            out.iter().filter_map(|e| e["log"].as_str()).collect::<Vec<_>>(),
            [
                "prigh's window went to https://login.example.com on its own (a sign-in?): it stays there until it is back",
                "prigh's window keeps going round https://login.example.com (its sign-in does not stick): showing prigh's sign-in page",
                "prigh's window is back from https://login.example.com"
            ]
        );
        // A page that keeps clicking links to a site, with fresh addresses,
        // and the site sending it back: two openings per 5s, said once.
        let mut links = self::links();
        let mut out = vec![];
        for i in 0..20 {
            out.extend(run_at(
                &mut links,
                &[
                    changed("A", "https://ai.example.com/"),
                    requested("A", "anchorClick", &format!("https://login.example.com/?state={i}")),
                    changed("A", &format!("https://login.example.com/?state={i}")),
                ],
                i / 4,
            ));
        }
        let opens = out.iter().filter(|e| e.get("open").is_some()).count();
        let navigates = out.iter().filter(|e| e["method"] == "Page.navigate").count();
        let logs: Vec<&Value> = out.iter().filter(|e| e.get("log").is_some()).collect();
        assert_eq!((opens, navigates), (2, 2));
        assert_eq!(
            logs.first().map(|l| l["log"].clone()),
            Some(json!("https://login.example.com again within 5s: not opening it in the default browser again"))
        );
        // Popups to one site, again and again.
        let mut links = self::links();
        let mut out = run(&mut links, &[created("A", "http://127.0.0.1:7780/")]);
        for i in 0..20 {
            let target = format!("P{i}");
            out.extend(run_at(
                &mut links,
                &[popup("Target.targetCreated", &target, &format!("https://spam.example/{i}"))],
                i * 2,
            ));
        }
        let opens: Vec<&Value> = out.iter().filter(|e| e.get("open").is_some()).collect();
        let closes = out.iter().filter(|e| e["method"] == "Target.closeTarget").count();
        assert_eq!(
            opens,
            [
                &json!({ "open": "https://spam.example/0" }),
                &json!({ "open": "https://spam.example/1" }),
                &json!({ "open": "https://spam.example/3" }),
                &json!({ "open": "https://spam.example/4" }),
                &json!({ "open": "https://spam.example/6" }),
                &json!({ "open": "https://spam.example/7" }),
                &json!({ "open": "https://spam.example/9" }),
                &json!({ "open": "https://spam.example/10" }),
                &json!({ "open": "https://spam.example/12" }),
                &json!({ "open": "https://spam.example/13" }),
                &json!({ "open": "https://spam.example/15" }),
                &json!({ "open": "https://spam.example/16" }),
                &json!({ "open": "https://spam.example/18" }),
                &json!({ "open": "https://spam.example/19" }),
            ]
        );
        assert_eq!(closes, 20);
    }

    fn remote(cookies: &[(&str, &str)]) -> Remote {
        Remote {
            url: "https://ai.example.com/#app=desktop&host=h".to_string(),
            cookies: cookies.iter().map(|(n, v)| (n.to_string(), v.to_string())).collect(),
            signin_page: Some("http://127.0.0.1:7780/#desktop=k&signin=1".to_string()),
        }
    }

    fn set_cookies(cookies: &[(&str, &str)]) -> Value {
        let cookies: Vec<Value> = cookies
            .iter()
            .map(|(n, v)| {
                json!({ "name": n, "value": v, "url": "https://ai.example.com/", "path": "/", "secure": true, "httpOnly": true })
            })
            .collect();
        json!({ "method": "Storage.setCookies", "params": { "cookies": cookies } })
    }

    #[test]
    fn with_the_sign_in_from_the_default_browser_the_window_gets_its_cookie() {
        let mut links = links();
        let cookie = [("authentik_proxy", "c00k1e")];
        // The browser has them at once (no window to send there yet).
        let out = json_of(links.set_remote(remote(&cookie), false));
        assert_eq!(out, [json!({ "id": 3, "method": "Storage.setCookies", "params": set_cookies(&cookie)["params"] })]);
        assert_eq!(run(&mut links, &[json!({ "id": 3, "result": {} }).to_string()]), Vec::<Value>::new());
        // Launched on the site before they landed, redirected: them again,
        // then the site again.
        let out = run_raw(&mut links, &[created("A", "https://login.example.com/start?state=1")], 0);
        assert_eq!(
            out,
            [
                json!({ "log": "prigh's window went to https://login.example.com: giving it the sign-in to https://ai.example.com" }),
                json!({ "id": 4, "method": "Storage.setCookies", "params": set_cookies(&cookie)["params"] }),
            ]
        );
        let out = run(&mut links, &[json!({ "id": 4, "result": {} }).to_string()]);
        assert_eq!(out, [navigate("A", "https://ai.example.com/#app=desktop&host=h")]);
        // The cookie did not do: prigh's sign-in page, once.
        let out = run(&mut links, &[changed("A", "https://login.example.com/start?state=2")]);
        assert_eq!(
            out,
            [
                json!({ "log": "prigh's window went to https://login.example.com: https://ai.example.com wants a sign-in, showing prigh's" }),
                navigate("A", "http://127.0.0.1:7780/#desktop=k&signin=1"),
            ]
        );
        let out = run(&mut links, &[changed("A", "http://127.0.0.1:7780/#desktop=k&signin=1")]);
        assert_eq!(out, [json!({ "log": "prigh's window is back from https://login.example.com" })]);
        // Signed in through the default browser: the new cookie, and the
        // site, in the window on the sign-in page.
        let cookie = [("authentik_proxy", "fr3sh"), ("other", "x")];
        let out = json_of(links.set_remote(remote(&cookie), false));
        assert_eq!(out.len(), 1);
        assert_eq!(out[0]["params"], set_cookies(&cookie)["params"]);
        let id = out[0]["id"].clone();
        let out = run(&mut links, &[json!({ "id": id, "result": {} }).to_string()]);
        assert_eq!(out, [navigate("A", "https://ai.example.com/#app=desktop&host=h")]);
        let out = run(&mut links, &[changed("A", "https://ai.example.com/#app=desktop&host=h")]);
        assert_eq!(out, Vec::<Value>::new());
        // It expires later: the sign-in page (the cookie it has is the one
        // it was given), once; never a loop.
        let mut out = vec![];
        for i in 0..10 {
            for e in run_raw(&mut links, &[changed("A", &format!("https://login.example.com/start?state=x{i}"))], 0) {
                if e["method"] == "Storage.setCookies" {
                    out.extend(run(&mut links, &[json!({ "id": e["id"], "result": {} }).to_string()]));
                }
                out.push(e);
            }
        }
        let navigates: Vec<Value> =
            out.iter().filter(|e| e["method"] == "Page.navigate").map(|e| e["params"]["url"].clone()).collect();
        assert_eq!(navigates, ["http://127.0.0.1:7780/#desktop=k&signin=1"], "{out:#?}");
    }

    #[test]
    fn signing_in_in_the_window_from_prighs_sign_in_page() {
        let mut links = links();
        links.set_remote(remote(&[]), false);
        let mut out = run(&mut links, &[created("A", "http://127.0.0.1:7780/#desktop=k&signin=1")]);
        // Sign in here: the window goes to the site, redirected.
        out.extend(json_of(links.set_remote(remote(&[]), true)));
        let out_rest = run(
            &mut links,
            &[
                changed("A", "https://login.example.com/start?state=1"),
                changed("A", "https://login.example.com/flow?state=1"),
                changed("A", "https://ai.example.com/#app=desktop&host=h"),
            ],
        );
        out.extend(out_rest);
        assert_eq!(
            out,
            [
                json!({ "id": 5, "method": "Page.navigate", "sessionId": "S-A", "params": { "url": "https://ai.example.com/#app=desktop&host=h" } }),
                json!({ "log": "prigh's window went to https://login.example.com on its own (a sign-in?): it stays there until it is back" }),
                json!({ "log": "prigh's window is back from https://login.example.com" }),
            ]
        );
    }

    #[test]
    fn mail_links_go_to_the_default_mail_handler() {
        let mut links = links();
        assert_eq!(
            run(
                &mut links,
                &[created("A", "http://127.0.0.1:7780/"), popup("Target.targetCreated", "B", "mailto:me@example.com")]
            ),
            [json!({ "open": "mailto:me@example.com" }), close("B")]
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
        // Forgotten: a page with that id now is a new one (here a popup).
        assert_eq!(run(&mut links, &[popup("Target.targetCreated", "A", "https://example.com/")]).len(), 2);
    }
}
