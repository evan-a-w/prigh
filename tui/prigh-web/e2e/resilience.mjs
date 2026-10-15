// A long session on a connection that cannot keep up, the conditions of a
// reconnect loop seen in production: the page reconnected over and over
// ("Reconnected ×86") and never loaded the session.
//
// 1. A slow network (every message the page receives arrives at 8 kB/s)
//    while the page is shown again and again (each time it checks that its
//    socket still works): the check waits behind the large replies on their
//    way instead of dropping a working connection, and the session comes
//    back after one reconnect.
// 2. A connection that drops each time before the session's messages arrive
//    (a backend that cannot serve the session): the page waits longer each
//    time, never says "Reconnected", then stops and says what to do; once
//    the connection holds, "Retry now" brings the session back.
// Run by prigh_web.sh; prints normalised results (the expected output is in
// prigh_web.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: resilience.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "resilience");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
const sessions = join(home, ".prigh", "sessions");
mkdirSync(sessions, { recursive: true });
mkdirSync(cwd, { recursive: true });
// Nothing may wait without a deadline.
setTimeout(() => { console.error("resilience.mjs: timed out"); process.exit(2); }, 300000).unref();

// Many short turns: a large outline (every prompt is a landmark).
const turns = 400;
const id = "2e5112e5112e5112";
const writeSession = () => {
  let seq = 0;
  let parent = null;
  let at = Math.floor(Date.now() / 1000) - 3600 - turns * 20;
  const created = new Date(at * 1000).toISOString().replace("T", " ").replace(/\.(\d+)Z$/, ".$1000Z");
  const lines = [JSON.stringify(["Header", { id, cwd, created_at: created }])];
  const entry = payload => {
    const eid = `${id.slice(0, 8)}${String(seq++).padStart(8, "0")}`;
    lines.push(JSON.stringify(["Entry", { id: eid, parent, at: (at += 10), payload }]));
    parent = eid;
  };
  const usage = { input: 1200, output: 300, cache_read: 0 };
  entry(["Name", { name: "a long chat" }]);
  for (let t = 1; t <= turns; t++) {
    entry(["Message", ["User", { text: `Question ${t}: what does part ${t} of the design do, and why is it built that way rather than another?` }]]);
    entry(["Message", ["Assistant", {
      content: [["Text", `Answer ${t}. Part ${t} keeps the state of one connection; it is built that way so that one slow peer never holds up the others.`]],
      stop_reason: ["End_turn"], usage, model: "faux",
    }]]);
  }
  writeFileSync(join(sessions, `20261001-100000000_${id}.jsonl`), lines.join("\n") + "\n");
};
writeSession();
writeFileSync(join(dir, "script.json"), "[]");

let server;
const startServer = listen =>
  new Promise((resolve, reject) => {
    server = spawn(
      backend,
      ["serve", "-prigh-web", listen, "-prigh-web-root", site, "-faux-script", join(dir, "script.json"),
        "-token", token, "-cwd", cwd],
      { env: { ...process.env, HOME: home }, stdio: ["ignore", "ignore", "pipe"] },
    );
    let err = "";
    const timer = setTimeout(() => reject(new Error(`backend did not start:\n${err}`)), 30000);
    server.stderr.on("data", data => {
      err += data;
      const m = err.match(/^prigh: prigh-web on (\S+)$/m);
      if (m) { clearTimeout(timer); resolve(m[1]); }
    });
    server.on("exit", code => { clearTimeout(timer); reject(new Error(`backend exited (${code}):\n${err}`)); });
  });
const stopServer = () =>
  new Promise(resolve => {
    server.removeAllListeners("exit");
    if (server.exitCode !== null || server.signalCode !== null) return resolve();
    const timer = setTimeout(() => { server.kill("SIGKILL"); }, 5000);
    server.on("exit", () => { clearTimeout(timer); resolve(); });
    server.kill();
  });

const url = await startServer("127.0.0.1:0");

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
const context = await browser.newContext({ viewport: { width: 390, height: 760 } });
context.setDefaultTimeout(30000);
const page = await context.newPage();
// The page's sockets, as the network delivers them: with [e2e-ws-rate]
// (bytes a millisecond) each message arrives after the ones before it, at
// that rate; with [e2e-ws-drop] the socket closes as the session's messages
// arrive, before the page sees them. [__wsOpened] counts the sockets.
await page.addInitScript(() => {
  window.__wsOpened = 0;
  window.__reconnectedToasts = 0;
  new MutationObserver(records => {
    for (const r of records) for (const n of r.addedNodes)
      if (n.nodeType === 1 && n.matches?.(".toast") && n.innerText.includes("Reconnected")) window.__reconnectedToasts++;
  }).observe(document, { childList: true, subtree: true });
  const Real = window.WebSocket;
  window.WebSocket = class extends Real {
    constructor(url, protocols) {
      super(url, protocols);
      if (!String(url).includes("/ws")) return;
      this.addEventListener("open", () => { window.__wsOpened++; });
      const methods = new Map();
      const send = this.send.bind(this);
      this.send = data => {
        try { const m = JSON.parse(data); if (m.id) methods.set(m.id, m.method); } catch {}
        send(data);
      };
      let handler = null;
      let delivered = Promise.resolve();
      super.onmessage = ev => {
        if (sessionStorage.getItem("e2e-ws-drop")) {
          const m = JSON.parse(ev.data);
          if (m.type === "response" && methods.get(m.id) === "get_messages_page") { this.close(); return; }
        }
        const rate = Number(sessionStorage.getItem("e2e-ws-rate") || 0);
        const delay = rate ? ev.data.length / rate : 0;
        delivered = delivered
          .then(() => new Promise(resolve => setTimeout(resolve, delay)))
          .then(() => handler?.call(this, ev));
      };
      Object.defineProperty(this, "onmessage", { get: () => handler, set: h => { handler = h; } });
    }
  };
});
const errors = [];
page.on("console", message => {
  if (message.type() === "error"
    && !/favicon\.ico|WebSocket|establish a connection/.test(message.text()))
    errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

const section = label => console.log(`=== ${engineName}: resilience: ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-resilience-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const opened = () => page.evaluate(() => window.__wsOpened);
const banner = () => page.evaluate(() => document.querySelector(".banner")?.innerText.replace(/\s+/g, " ") ?? "none");
const status = () => page.evaluate(() => document.querySelector(".status")?.innerText.split("\n")[0] ?? "");
const lastAnswer = () => page.evaluate(() =>
  [...document.querySelectorAll("#chat .msg")].some(el => el.innerText.includes(`Answer ${400}.`)));
const back = async () => {
  await page.waitForFunction(() => !document.querySelector(".banner") && !document.querySelector(".app.offline"), null, { timeout: 60000 });
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("Answer 400."), null, { timeout: 60000 });
};

try {
  await page.goto(`${url}?session=${id}`);
  await page.waitForSelector("form input[type=password]");
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("Answer 400."));
  section("a long chat");
  console.log(`newest answer on show: ${await lastAnswer()}`);
  await screenshot("loaded");

  // 1. A slow network; the connection goes once, and while the session comes
  //    back over it the page is shown again every 2.5s.
  const before = await opened();
  await page.evaluate(() => {
    sessionStorage.setItem("e2e-ws-rate", "8");
    window.__reconnectedToasts = 0;
    window.dispatchEvent(new Event("offline"));
  });
  for (let i = 0; i < 10; i++) {
    await page.waitForTimeout(2500);
    await page.evaluate(() => window.dispatchEvent(new Event("online")));
  }
  await back();
  section("a slow network, the page shown again and again");
  console.log(`sockets opened: ${(await opened()) - before}`);
  console.log(`banner: ${await banner()}`);
  console.log(`newest answer on show: ${await lastAnswer()}`);
  console.log(`"Reconnected" said: ${await page.evaluate(() => window.__reconnectedToasts)}`);
  await screenshot("slow network");
  await page.evaluate(() => sessionStorage.removeItem("e2e-ws-rate"));

  // 2. The connection drops each time before the session's messages come.
  const before2 = await opened();
  await page.evaluate(() => {
    sessionStorage.setItem("e2e-ws-drop", "1");
    window.__reconnectedToasts = 0;
    window.dispatchEvent(new Event("offline"));
  });
  await page.waitForSelector(".banner.gave-up", { timeout: 90000 });
  section("a connection that drops while the session loads");
  console.log(`sockets opened: ${(await opened()) - before2}`);
  console.log(`banner: ${await banner()}`);
  console.log(`status: ${await status()}`);
  console.log(`"Reconnected" said: ${await page.evaluate(() => window.__reconnectedToasts)}`);
  console.log(`transcript still on show: ${await lastAnswer()}`);
  await screenshot("gave up");
  // It stays stopped: no more sockets, also when the network comes back.
  await page.evaluate(() => window.dispatchEvent(new Event("online")));
  await page.waitForTimeout(3000);
  console.log(`sockets opened since: ${(await opened()) - before2}`);

  // 3. The connection holds again: Retry now.
  await page.evaluate(() => sessionStorage.removeItem("e2e-ws-drop"));
  await page.locator(".banner.gave-up button", { hasText: "Retry now" }).click();
  await back();
  section("retry now");
  console.log(`banner: ${await banner()}`);
  console.log(`newest answer on show: ${await lastAnswer()}`);
  await page.waitForFunction(() => window.__reconnectedToasts > 0);
  console.log(`"Reconnected" said: ${await page.evaluate(() => window.__reconnectedToasts)}`);
  await screenshot("retry now");

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  await screenshot("failure");
  console.error(errors.join("\n"));
  throw error;
} finally {
  await browser.close();
  await stopServer();
}
