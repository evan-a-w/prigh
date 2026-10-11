// Losing the connection on a phone: a long session, scrolled halfway up,
// with a draft and an image in the composer. The network goes away and comes
// back, then the backend restarts: each time the page reconnects in place
// (same session, same scroll position, the draft and the image kept, the
// transcript's DOM untouched, never the sign-in page). Then a reload returns
// to the same session, scroll position and draft, also when the backend is
// out of reach as it loads. Run by prigh_web.sh after
// prigh_web.mjs; prints normalised results (the expected output is in
// prigh_web.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { png } from "./png.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: reconnect.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "reconnect");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });

const turns = 10;
const answer = n =>
  [`Answer ${n}.`, ...Array.from({ length: 8 }, (_, i) =>
    `Paragraph ${i + 1} of answer ${n}: enough words to wrap over a couple of lines on a phone screen.`)]
    .join("\n\n");
writeFileSync(join(dir, "script.json"),
  JSON.stringify(Array.from({ length: turns }, (_, i) => ({ text: answer(i + 1) }))));

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
const port = new URL(url).port;

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
// A phone.
const context = await browser.newContext({ viewport: { width: 390, height: 760 } });
context.setDefaultTimeout(20000);
const page = await context.newPage();
// While the tab's [e2e-ws-down] is set, the backend is out of reach.
await page.addInitScript(() => {
  const Real = window.WebSocket;
  window.WebSocket = class extends Real {
    constructor(url, protocols) {
      super(sessionStorage.getItem("e2e-ws-down") ? "ws://127.0.0.1:9/" : url, protocols);
    }
  };
});
const errors = [];
page.on("console", message => {
  if (message.type() === "error"
    && !/favicon\.ico|WebSocket|ERR_CONNECTION_REFUSED|ERR_INTERNET_DISCONNECTED|establish a connection/.test(message.text()))
    errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));
// Every page the tab loads: reconnecting must not load one.
let loads = 0;
page.on("load", () => { loads++; });

const section = label => console.log(`=== ${engineName}: reconnect: ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-reconnect-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};

const composer = () => page.locator(".composer textarea");
const idle = () => page.waitForFunction(() => !document.querySelector(".streaming, .btn.stop"));
const bodyHas = text => page.waitForFunction(t => document.body.innerText.includes(t), text);
const sessionId = () => new URL(page.url()).searchParams.get("session");
const scrollTop = () => page.evaluate(() => document.getElementById("chat").scrollTop);
// Its first line, without the attempt count (that depends on timing).
const banner = () => page.locator(".banner").innerText()
  .then(text => text.split("\n")[0].replace(/ \(attempt \d+, next in [\d.]+s\)/, ""));

// What must survive: where we are and what we were writing.
const place = () => page.evaluate(() => ({
  session: new URL(location.href).searchParams.get("session"),
  top: document.getElementById("chat").scrollTop,
  draft: document.querySelector(".composer textarea").value,
  images: document.querySelectorAll(".composer .pending-image").length,
  // Set on the transcript's first message before going offline: still there
  // when the page kept its DOM.
  kept: !!document.querySelector("#chat .msg[data-e2e-mark]"),
  signIn: !!document.getElementById("signin-form"),
}));
const compare = (label, before, after) => {
  section(label);
  console.log(`same session: ${after.session === before.session}`);
  const moved = Math.abs(after.top - before.top);
  console.log(`scroll position kept: ${moved <= 4}${moved <= 4 ? "" : ` (moved ${moved}px)`}`);
  console.log(`draft: ${after.draft}`);
  console.log(`images in the composer: ${after.images}`);
  console.log(`transcript DOM kept: ${after.kept}`);
  console.log(`sign-in page: ${after.signIn}`);
  console.log(`pages loaded since: ${loads - 1}`);
};

// Chromium takes [navigator.onLine] from the host's network interfaces: with
// only a loopback (the Nix sandbox) it starts offline, so going offline and
// back fires no events. Then they are fired here, as a phone's browser would;
// [setOffline] still makes every connection attempt fail meanwhile.
let browserSeesNetwork;
const setOffline = async offline => {
  browserSeesNetwork ??= await page.evaluate(() => navigator.onLine);
  await context.setOffline(offline);
  if (!browserSeesNetwork)
    await page.evaluate(offline => window.dispatchEvent(new Event(offline ? "offline" : "online")), offline);
};

const reconnected = async () => {
  await page.waitForFunction(() => !document.querySelector(".banner"));
  await page.waitForFunction(() => !document.querySelector(".app.offline"));
  // The merge of the session's messages lands after the banner goes.
  await page.waitForTimeout(500);
};

try {
  await page.goto(url);
  await page.waitForSelector("form input[type=password]");
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await composer().waitFor();
  loads = 1;

  for (let n = 1; n <= turns; n++) {
    await composer().fill(`Question ${n}?`);
    await page.locator(".btn.send").click();
    await bodyHas(`Answer ${n}.`);
    await idle();
  }
  // Halfway up, a draft with an image.
  await page.evaluate(() => {
    const chat = document.getElementById("chat");
    chat.scrollTop = Math.round((chat.scrollHeight - chat.clientHeight) / 2);
  });
  await page.locator(".jump-to-bottom").waitFor();
  await composer().fill("half-written thought");
  await page.evaluate(bytes => {
    const data = new DataTransfer();
    data.items.add(new File([new Uint8Array(bytes)], "flag.png", { type: "image/png" }));
    const event = new ClipboardEvent("paste", { bubbles: true, cancelable: true });
    Object.defineProperty(event, "clipboardData", { value: data });
    document.querySelector(".composer textarea").dispatchEvent(event);
  }, [...png(20, 10, () => [30, 160, 90])]);
  await page.waitForFunction(() => document.querySelector(".composer .pending-image"));
  await page.evaluate(() => document.querySelector("#chat .msg").setAttribute("data-e2e-mark", ""));
  const start = await place();
  section("a long session, halfway up");
  console.log(`scrolled: ${start.top > 200}`);
  console.log(`draft: ${start.draft}`);
  console.log(`images in the composer: ${start.images}`);
  await screenshot("halfway up");

  // 1. The network goes away and comes back.
  await setOffline(true);
  await page.locator(".banner").waitFor();
  section("offline");
  console.log(await banner());
  console.log(`composer: ${await composer().getAttribute("placeholder")}`);
  await screenshot("offline");
  await page.waitForTimeout(1500);
  await setOffline(false);
  await reconnected();
  compare("network back", start, await place());
  await screenshot("network back");

  // 2. The backend restarts (on the same port).
  await stopServer();
  await page.locator(".banner").waitFor();
  section("backend gone");
  console.log(await banner());
  await screenshot("backend gone");
  await page.waitForTimeout(1000);
  await startServer(`127.0.0.1:${port}`);
  await reconnected();
  compare("backend back", start, await place());
  await screenshot("backend back");

  // 3. A reload (or the browser bringing a discarded tab back): the same
  //    session, at the same place.
  const before = await place();
  await page.reload();
  await page.waitForSelector("#chat .msg");
  await page.waitForTimeout(1000);
  const after = await place();
  section("reloaded");
  console.log(`same session: ${after.session === before.session}`);
  const moved = Math.abs(after.top - before.top);
  console.log(`scroll position kept: ${moved <= 4}${moved <= 4 ? "" : ` (moved ${moved}px)`}`);
  console.log(`draft: ${after.draft}`);
  console.log(`sign-in page: ${after.signIn}`);
  console.log(`jump button: ${await page.locator(".jump-to-bottom").count()}`);
  await screenshot("reloaded");

  // 4. Reloaded while the backend is out of reach (a phone bringing back a
  //    discarded tab on a dead network): the page waits for it, then goes
  //    back to the same place.
  await page.evaluate(() => sessionStorage.setItem("e2e-ws-down", "1"));
  await page.reload();
  await page.locator(".banner").waitFor();
  section("reloaded out of reach");
  console.log(await banner());
  console.log(`sign-in page: ${await page.locator("#signin-form").count() > 0}`);
  await screenshot("reloaded out of reach");
  await page.evaluate(() => {
    sessionStorage.removeItem("e2e-ws-down");
    window.dispatchEvent(new Event("online"));
  });
  await reconnected();
  await page.waitForSelector("#chat .msg");
  await page.waitForTimeout(1000);
  const back = await place();
  section("back in reach");
  console.log(`same session: ${back.session === before.session}`);
  const moved2 = Math.abs(back.top - before.top);
  console.log(`scroll position kept: ${moved2 <= 4}${moved2 <= 4 ? "" : ` (moved ${moved2}px)`}`);
  console.log(`draft: ${back.draft}`);
  console.log(`sign-in page: ${back.signIn}`);
  await screenshot("back in reach");

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
