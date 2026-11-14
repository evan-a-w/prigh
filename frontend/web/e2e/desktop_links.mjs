// The desktop app's browser window (#shell=browser) hands links to other
// sites to the app (window.prighOpenExternal, a DevTools binding there; a
// stand-in here) instead of letting the browser open a window of its own:
// a click, Ctrl+click, a middle click and, on a phone, a tap; links to
// prigh's own site, a page without the binding and a page that is not in
// the browser shell are left to the browser.
// Run by prigh_web.sh; prints normalised results (the expected output is in
// ENGINE.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: desktop_links.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "desktop_links");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });
setTimeout(() => { console.error("desktop_links.mjs: timed out"); process.exit(2); }, 240000).unref();

const reply = "See [the docs](https://example.com/docs) or [write](mailto:me@example.com).";
writeFileSync(join(dir, "script.json"), JSON.stringify([{ text: reply }, { text: reply }, { text: reply }]));

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
    "-token", token, "-cwd", cwd],
  { env: { ...process.env, HOME: home }, stdio: ["ignore", "ignore", "pipe"] },
);
const url = await new Promise((resolve, reject) => {
  let err = "";
  server.stderr.on("data", data => {
    err += data;
    const m = err.match(/^prigh: prigh-web on (\S+)$/m);
    if (m) resolve(m[1]);
  });
  server.on("exit", code => reject(new Error(`backend exited (${code}):\n${err}`)));
});
const origin = new URL(url).origin;
const stopServer = () =>
  new Promise(resolve => {
    server.removeAllListeners("exit");
    if (server.exitCode !== null || server.signalCode !== null) return resolve();
    const timer = setTimeout(() => { server.kill("SIGKILL"); }, 5000);
    server.on("exit", () => { clearTimeout(timer); resolve(); });
    server.kill();
  });

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
const errors = [];
const section = label => console.log(`=== ${engineName}: desktop_links: ${label} ===`);
const normalize = s => s.replaceAll(origin, "ORIGIN");

// A context whose windows the browser opens are noted (and closed); the
// binding's stand-in notes what it is called with.
const newPage = async options => {
  const context = await browser.newContext({ viewport: { width: 1280, height: 800 }, ...options });
  context.setDefaultTimeout(20000);
  await context.route("https://example.com/**", route =>
    route.fulfill({ contentType: "text/html", body: "<title>elsewhere</title>" }));
  const page = await context.newPage();
  page.on("console", message => {
    if (message.type() === "error" && !/favicon\.ico|interactive-widget/.test(message.text()))
      errors.push(`console: ${message.text()}`);
  });
  page.on("pageerror", error => errors.push(`page: ${error.message}`));
  const opened = [];
  context.on("page", async popup => {
    await popup.waitForLoadState("domcontentloaded").catch(() => {});
    opened.push(normalize(popup.url()));
    await popup.close();
  });
  return { context, page, opened };
};

const bind = page => page.evaluate(() => {
  window.handed = [];
  window.prighOpenExternal = payload => window.handed.push(payload);
});
let handedSeen = 0;
const handed = async page => {
  await page.waitForFunction(n => window.handed.length > n, handedSeen);
  const all = await page.evaluate(() => window.handed);
  const fresh = all.slice(handedSeen);
  handedSeen = all.length;
  return fresh.join(" ");
};
const windowOpened = async (opened, before) => {
  for (let i = 0; i < 200 && opened.length === before; i++) await new Promise(r => setTimeout(r, 50));
  return opened.slice(before).join(" ") || "none";
};

const signIn = async (page, hash) => {
  await page.goto(url + hash);
  await page.waitForSelector("form input[type=password], .composer textarea");
  if (await page.locator("form input[type=password]").count()) {
    await page.locator("form input[type=password]").fill(token);
    await page.locator("form button[type=submit]").click();
  }
  const composer = page.locator(".composer textarea");
  await composer.waitFor();
  await composer.fill("links please");
  await composer.press("Enter");
  await page.locator("#chat .msg.assistant a[href^=mailto]").last().waitFor();
  // A link to prigh's own site, as the file viewer's are.
  await page.evaluate(() => {
    const a = document.createElement("a");
    a.href = "/?own=1";
    a.target = "_blank";
    a.id = "own-link";
    a.textContent = "prigh's own";
    [...document.querySelectorAll("#chat .msg.assistant a[href^=mailto]")].at(-1).after(" ", a);
  });
};
const docs = page => page.locator("#chat .msg.assistant a[href='https://example.com/docs']").last();
const mail = page => page.locator("#chat .msg.assistant a[href^=mailto]").last();
const ctrl = process.platform === "darwin" ? "Meta" : "Control";

let current;
try {
  section("the browser shell, with the app's binding");
  current = await newPage();
  const { page, opened } = current;
  await signIn(page, "#shell=browser");
  await bind(page);
  await docs(page).click();
  console.log(`a click: handed over ${await handed(page)}`);
  await docs(page).click({ modifiers: [ctrl] });
  console.log(`Ctrl+click: handed over ${await handed(page)}`);
  await docs(page).click({ button: "middle" });
  console.log(`a middle click: handed over ${await handed(page)}`);
  await mail(page).click();
  console.log(`a mail link: handed over ${await handed(page)}`);
  // Enter on a focused link is a click too.
  await docs(page).focus();
  await page.keyboard.press("Enter");
  console.log(`Enter on it: handed over ${await handed(page)}`);
  console.log(`the browser's windows: ${opened.length ? opened.join(" ") : "none"}; still on prigh${new URL(page.url()).pathname}`);
  let before = opened.length;
  await page.locator("#own-link").click();
  console.log(`prigh's own link: the browser opened ${await windowOpened(opened, before)}; handed over: ${await page.evaluate(() => window.handed.length) - handedSeen}`);

  section("the browser shell, without the binding (a browser tab, an older app)");
  await page.evaluate(() => { delete window.prighOpenExternal; });
  before = opened.length;
  await docs(page).click();
  console.log(`a click: the browser opened ${await windowOpened(opened, before)}`);
  await current.context.close();

  section("not in the browser shell, with a binding");
  current = await newPage();
  handedSeen = 0;
  await signIn(current.page, "");
  await bind(current.page);
  await docs(current.page).click();
  console.log(`a click: the browser opened ${await windowOpened(current.opened, 0)}; handed over: ${await current.page.evaluate(() => window.handed.length)}`);
  await current.context.close();

  // A phone's tap (the browser shell is a desktop's, but a touch screen
  // taps).
  section("a tap");
  current = await newPage({
    viewport: { width: 390, height: 844 },
    hasTouch: true,
    ...(engineName === "firefox" ? {} : { isMobile: true }),
  });
  handedSeen = 0;
  await signIn(current.page, "#shell=browser");
  await bind(current.page);
  await docs(current.page).tap();
  console.log(`a tap: handed over ${await handed(current.page)}; the browser's windows: ${current.opened.length || "none"}`);
  await current.context.close();
  current = undefined;

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  if (current && process.env.SHOTS)
    await current.page.screenshot({ path: join(process.env.SHOTS, `${engineName}-desktop_links-failure.png`) });
  console.error(errors.join("\n"));
  throw error;
} finally {
  await end(() => stopServer(), () => browser.close());
}
