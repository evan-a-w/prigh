//! Links in the browser's app window (see [crate::browser]). That browser
//! runs on a profile of prigh's own, so a link it opens itself would open
//! there: in a tabbed window of that profile, without the user's own
//! browser's sign-ins and extensions. The shell watches its pages through
//! the DevTools protocol on a pipe only it holds (`--remote-debugging-pipe`:
//! JSON messages ended by a NUL byte, commands on the browser's fd 3, replies
//! and events on its fd 4) and sends web links elsewhere to the system's
//! default browser, as the built-in window does (see [crate::navigation]):
//!
//! - a new tab or window on another site (a link with `target=_blank`,
//!   `window.open`) is closed, and the link opened in the default browser;
//! - prigh's own window (the app's, or one opened on prigh's site) going to
//!   another site is sent back to where it was, and the link opened in the
//!   default browser.
//!
//! prigh's sites, `about:blank` and the browser's own pages (`chrome://`)
//! are left alone.

use crate::navigation::{decide, origin, Decision};
use serde_json::{json, Value};
use std::collections::HashMap;

/// The command that starts the events: every page target, existing and
/// new, and every change of its address.
pub fn discover() -> String {
    json!({ "id": 1, "method": "Target.setDiscoverTargets", "params": { "discover": true } }).to_string()
}

/// What the shell does about a message.
#[derive(Debug, PartialEq, Eq)]
pub enum Effect {
    /// Write this command to the browser.
    Send(String),
    /// Open this link in the system's default browser.
    OpenExternally(String),
}

#[derive(Default)]
struct Page {
    /// Where it last was on prigh's sites: it is one of prigh's windows.
    ours: Option<String>,
    /// Sent elsewhere already: its later events say nothing new.
    handled: Option<String>,
}

/// The browser's pages, from its events.
#[derive(Default)]
pub struct Links {
    pages: HashMap<String, Page>,
    next_id: u64,
    /// Attaching to one of prigh's windows to send it back: the request's
    /// id, to the address to go back to.
    attaching: HashMap<u64, String>,
}

impl Links {
    pub fn new() -> Links {
        Links { next_id: 2, ..Links::default() }
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
        if let Some(id) = msg["id"].as_u64() {
            return self.on_reply(id, &msg);
        }
        let info = &msg["params"]["targetInfo"];
        match msg["method"].as_str() {
            Some("Target.targetCreated" | "Target.targetInfoChanged") if info["type"] == "page" => {
                let (Some(target), Some(url)) = (info["targetId"].as_str(), info["url"].as_str()) else {
                    return vec![];
                };
                self.on_page(target, url, ours)
            }
            Some("Target.targetDestroyed") => {
                if let Some(target) = msg["params"]["targetId"].as_str() {
                    self.pages.remove(target);
                }
                vec![]
            }
            _ => vec![],
        }
    }

    fn on_page(&mut self, target: &str, url: &str, ours: &[String]) -> Vec<Effect> {
        let page = self.pages.entry(target.to_string()).or_default();
        match decide(url, ours) {
            Decision::Allow => {
                if origin(url).is_some_and(|o| ours.contains(&o)) {
                    page.ours = Some(url.to_string());
                    page.handled = None;
                }
                vec![]
            }
            Decision::External if page.handled.as_deref() != Some(url) => {
                page.handled = Some(url.to_string());
                match page.ours.clone() {
                    // One of prigh's windows: back to prigh's page.
                    Some(back) => {
                        let (id, attach) =
                            self.command("Target.attachToTarget", json!({ "targetId": target, "flatten": true }), None);
                        self.attaching.insert(id, back);
                        vec![Effect::OpenExternally(url.to_string()), Effect::Send(attach)]
                    }
                    None => {
                        let (_, close) = self.command("Target.closeTarget", json!({ "targetId": target }), None);
                        vec![Effect::OpenExternally(url.to_string()), Effect::Send(close)]
                    }
                }
            }
            // Its own pages (chrome:), and what the window would refuse.
            _ => vec![],
        }
    }

    fn on_reply(&mut self, id: u64, msg: &Value) -> Vec<Effect> {
        let Some(back) = self.attaching.remove(&id) else { return vec![] };
        let Some(session) = msg["result"]["sessionId"].as_str() else { return vec![] };
        let (_, navigate) = self.command("Page.navigate", json!({ "url": back }), Some(session));
        let (_, detach) = self.command("Target.detachFromTarget", json!({ "sessionId": session }), None);
        vec![Effect::Send(navigate), Effect::Send(detach)]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn event(method: &str, target: &str, kind: &str, url: &str) -> String {
        json!({ "method": method, "params": { "targetInfo": {
            "targetId": target, "type": kind, "url": url, "title": "", "attached": false } } })
        .to_string()
    }

    fn created(target: &str, url: &str) -> String {
        event("Target.targetCreated", target, "page", url)
    }

    fn changed(target: &str, url: &str) -> String {
        event("Target.targetInfoChanged", target, "page", url)
    }

    /// The effects as JSON: `{"open": URL}`, or the command sent.
    fn run(links: &mut Links, msgs: &[String]) -> Vec<Value> {
        let ours = ["http://127.0.0.1:7780".to_string(), "https://ai.example.com".to_string()];
        msgs.iter()
            .flat_map(|m| links.on_message(m, &ours))
            .map(|e| match e {
                Effect::Send(s) => serde_json::from_str(&s).unwrap(),
                Effect::OpenExternally(u) => json!({ "open": u }),
            })
            .collect()
    }

    #[test]
    fn a_new_tab_elsewhere_is_closed_and_its_link_opened_in_the_default_browser() {
        let mut links = Links::new();
        let app = created("A", "http://127.0.0.1:7780/#desktop=k&shell=browser");
        // A link with target=_blank: the new tab starts blank, then goes.
        let blank = created("B", "about:blank");
        let gone = changed("B", "https://github.com/evan-a-w/prigh");
        assert_eq!(
            run(&mut links, &[app, blank, gone.clone(), gone]),
            [
                json!({ "open": "https://github.com/evan-a-w/prigh" }),
                json!({ "id": 2, "method": "Target.closeTarget", "params": { "targetId": "B" } }),
            ]
        );
    }

    #[test]
    fn prighs_own_windows_and_the_browsers_pages_stay() {
        let mut links = Links::new();
        let msgs = [
            created("A", "http://127.0.0.1:7780/?session=s1"),
            // Open in new window: a popup on prigh's site, the backend's too.
            created("B", "about:blank"),
            changed("B", "https://ai.example.com/?session=s2#app=desktop"),
            created("C", "chrome://newtab/"),
            created("D", "chrome-error://chromewebdata/"),
            event("Target.targetCreated", "E", "service_worker", "https://elsewhere.example/sw.js"),
            event("Target.targetCreated", "F", "iframe", "https://elsewhere.example/embed"),
        ];
        assert_eq!(run(&mut links, &msgs), Vec::<Value>::new());
    }

    #[test]
    fn prighs_window_going_elsewhere_goes_back() {
        let mut links = Links::new();
        let mut out = run(
            &mut links,
            &[created("A", "http://127.0.0.1:7780/?session=s1"), changed("A", "https://example.com/docs")],
        );
        out.extend(run(&mut links, &[json!({ "id": 2, "result": { "sessionId": "S" } }).to_string()]));
        assert_eq!(
            out,
            [
                json!({ "open": "https://example.com/docs" }),
                json!({ "id": 2, "method": "Target.attachToTarget", "params": { "targetId": "A", "flatten": true } }),
                json!({ "id": 3, "method": "Page.navigate", "sessionId": "S",
                        "params": { "url": "http://127.0.0.1:7780/?session=s1" } }),
                json!({ "id": 4, "method": "Target.detachFromTarget", "params": { "sessionId": "S" } }),
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
        let mut links = Links::new();
        assert_eq!(
            run(&mut links, &[created("B", "mailto:me@example.com")]),
            [
                json!({ "open": "mailto:me@example.com" }),
                json!({ "id": 2, "method": "Target.closeTarget", "params": { "targetId": "B" } }),
            ]
        );
    }

    #[test]
    fn a_closed_page_is_forgotten_and_noise_is_ignored() {
        let mut links = Links::new();
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

    #[test]
    fn discovery_asks_for_every_target() {
        assert_eq!(
            serde_json::from_str::<Value>(&discover()).unwrap(),
            json!({ "id": 1, "method": "Target.setDiscoverTargets", "params": { "discover": true } })
        );
    }
}
