// Links to sessions in a reply: the model writes [title](?session=ID) and a
// bare prefix of an id; both are in-app links. A click switches to the
// session in place (the URL's ?session= follows, the page is not reloaded),
// Ctrl/⌘+click opens it in a new window, and a link to a session the user
// does not have shows a toast. On a desktop and on phones (390x844,
// 320x640, by touch: a 44px target). Run by prigh_web.sh; prints normalised
// results (the expected output is in ENGINE.expected) and, with SHOTS,
// screenshots.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";
import { normalVerbosity } from "./verbose_prefs.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: session_links.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "session_links");
const home = join(dir, "home");
normalVerbosity(home);
const cwd = join(dir, "cwd");
const sessions = join(home, ".prigh", "sessions");
mkdirSync(sessions, { recursive: true });
mkdirSync(cwd, { recursive: true });
setTimeout(() => { console.error("session_links.mjs: timed out"); process.exit(2); }, 240000).unref();

const taskbar = "6cad9f6e0123abcd";
const links = "3b5c0d1e2f3a4b5c";
let at = Math.floor(Date.now() / 1000) - 7200;
const write = (id, name, user, answer) => {
  let seq = 0;
  let parent = null;
  const created = new Date(at * 1000).toISOString().replace("T", " ").replace(/\.(\d+)Z$/, ".$1000Z");
  const lines = [JSON.stringify(["Header", { id, cwd, created_at: created }])];
  const entry = payload => {
    const eid = `${id.slice(0, 8)}${String(seq++).padStart(8, "0")}`;
    lines.push(JSON.stringify(["Entry", { id: eid, parent, at: (at += 7), payload }]));
    parent = eid;
  };
  entry(["Name", { name }]);
  entry(["Message", ["User", { text: user }]]);
  entry(["Message", ["Assistant", {
    content: [["Text", answer]],
    stop_reason: ["End_turn"], usage: { input: 100, output: 30, cache_read: 0 }, model: "faux",
  }]]);
  writeFileSync(join(sessions, `20261001-10000${seq}000_${id}.jsonl`), lines.join("\n") + "\n");
};
write(taskbar, "Fix the taskbar entry", "The taskbar entry is blank.", "Fixed: the entry has its title now.");
write(links, "Session links", "Hello.", "Hello! What can I do?");
// What the model answers each prompt (one per page: the desktop, then each
// phone).
const reply = `It was [Fix the taskbar entry](?session=${taskbar}), session 6cad9f6e; not [this one](?session=0badc0de).`;
writeFileSync(join(dir, "script.json"), JSON.stringify([{ text: reply }, { text: reply }, { text: reply }]));

let server;
const url = await new Promise((resolve, reject) => {
  server = spawn(
    backend,
    ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
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

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
const errors = [];
let page;
const open = async options => {
  const context = await browser.newContext(options);
  context.setDefaultTimeout(20000);
  page = await context.newPage();
  page.on("console", message => {
    if (message.type() === "error" && !/favicon\.ico|interactive-widget/.test(message.text()))
      errors.push(`console: ${message.text()}`);
  });
  page.on("pageerror", error => errors.push(`page: ${error.message}`));
};

const section = label => console.log(`=== ${engineName}: session_links: ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-session_links-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};

const composer = () => page.locator(".composer textarea");
const theReply = () => page.locator("#chat .msg.assistant", { hasText: "It was" }).last();
const link = text => theReply().locator("a.session-link", { hasText: text }).first();
const signIn = async () => {
  await page.goto(url);
  await page.waitForSelector("form input[type=password]");
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await composer().waitFor();
};
const openSession = async id => {
  if (await page.evaluate(() => document.querySelector(".app.narrow:not(.sidebar-open)") !== null))
    await page.locator(".chat-chrome .toggle-sidebar").click();
  await page.locator(`[data-session="${id}"]`).click();
  await page.waitForFunction(id => location.search.includes(id), id);
  await page.waitForFunction(() => document.querySelector(".sidebar").getBoundingClientRect().right <= 0
    || !document.querySelector(".app.narrow"));
};
const ask = async () => {
  await composer().fill("Which session fixed the taskbar?");
  await composer().press("Enter");
  await theReply().locator("a.session-link").first().waitFor();
  await page.waitForFunction(() => !document.querySelector("#chat .msg.assistant.streaming"));
};
// Where the page is: the session in the URL, the transcript's first
// question, and whether the page was loaded again (the marker set before
// the click is gone).
const where = () => page.evaluate(() =>
  `session=${new URLSearchParams(location.search).get("session")}; `
  + `showing "${document.querySelector("#chat .msg.user")?.innerText.trim().split("\n")[0]}"; `
  + `reloaded: ${window.sessionLinksMarker !== true}`);
const mark = () => page.evaluate(() => { window.sessionLinksMarker = true; });
const inTaskbar = () => page.waitForFunction(id =>
  location.search.includes(id) && document.querySelector("#chat")?.innerText.includes("The taskbar entry is blank."), taskbar);
const describe = () => theReply().evaluate(el =>
  [...el.querySelectorAll("a.session-link")].map(a =>
    `${a.innerText} -> ${a.getAttribute("href")} (${a.dataset.sessionLink}; "${a.title}")`).join("\n"));

try {
  await open({ viewport: { width: 1280, height: 900 } });
  await signIn();
  await openSession(links);
  await ask();
  section("drawn");
  console.log(await describe());
  await theReply().scrollIntoViewIfNeeded();
  await screenshot("desktop reply");

  section("a click on the link switches in place");
  await mark();
  await link("Fix the taskbar entry").click();
  await inTaskbar();
  console.log(await where());

  section("a click on the bare id too");
  await openSession(links);
  await mark();
  await link("6cad9f6e").click();
  await inTaskbar();
  console.log(await where());

  section("Ctrl/⌘+click opens a new window; this one stays");
  await openSession(links);
  await mark();
  const [popup] = await Promise.all([
    page.context().waitForEvent("page"),
    link("Fix the taskbar entry").click({ modifiers: ["ControlOrMeta"] }),
  ]);
  await popup.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("The taskbar entry is blank."));
  console.log(`new window: session=${await popup.evaluate(() => new URLSearchParams(location.search).get("session"))}`);
  await popup.close();
  console.log(await where());

  section("a session the user does not have: a toast");
  await theReply().locator("a.session-link", { hasText: "this one" }).click();
  const toast = page.locator(".toast.error", { hasText: "0badc0de" });
  await toast.waitFor();
  console.log((await toast.innerText()).replace(/\s+/g, " "));
  console.log(await where());
  await screenshot("desktop toast");
  await page.context().close();

  for (const [w, h] of [[390, 844], [320, 640]]) {
    const options = { viewport: { width: w, height: h }, hasTouch: true };
    if (engineName !== "firefox") options.isMobile = true;
    await open(options);
    await signIn();
    await openSession(links);
    await ask();
    section(`${w}x${h}`);
    console.log(await page.evaluate(() => {
      const a = [...document.querySelectorAll("#chat .msg.assistant a.session-link")].pop();
      const target = parseFloat(getComputedStyle(a, "::after").height);
      const doc = document.documentElement;
      return `tap target: ${target >= 44 ? "at least 44px" : `${target}px`} tall; `
        + `page overflows sideways: ${doc.scrollWidth > doc.clientWidth}`;
    }));
    await theReply().scrollIntoViewIfNeeded();
    await screenshot(`${w}x${h} reply`);
    await mark();
    await link("Fix the taskbar entry").tap();
    await inTaskbar();
    console.log(await where());
    await screenshot(`${w}x${h} switched`);
    await page.context().close();
  }

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  await screenshot("failure").catch(() => {});
  console.error(errors.join("\n"));
  throw error;
} finally {
  await end(() => server.kill(), () => browser.close());
}
