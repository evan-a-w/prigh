// Installing prigh-web as an app, on a phone: the manifest and its icons, the
// service worker taking the page over and keeping the app shell (and nothing
// else: the RPC, file transfers and the sign-in link always go to the
// backend), a reload with the backend stopped still opening the app,
// reconnecting, and a new build's worker announced by the update notice,
// whose Reload moves the page to it. Run by prigh_web.sh; prints normalised
// results (the expected output is in ENGINE.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { chmodSync, cpSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: pwa.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "pwa");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
// A copy of the site, to deploy a new build into.
const site = join(dir, "site");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });
cpSync(process.env.SITE, site, { recursive: true, dereference: true });

writeFileSync(join(dir, "script.json"),
  JSON.stringify([{ text: "First answer." }, { text: "Second answer." }]));

let server;
const startServer = listen =>
  new Promise((resolve, reject) => {
    server = spawn(
      backend,
      ["serve", "-web", listen, "-web-root", site, "-faux-script", join(dir, "script.json"),
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
const errors = [];
page.on("console", message => {
  if (message.type() === "error"
    && !/favicon\.ico|WebSocket|ERR_CONNECTION_REFUSED|NS_ERROR_CONNECTION_REFUSED|establish a connection|Failed to fetch|NetworkError|Failed to load resource/.test(message.text()))
    errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

const section = label => console.log(`=== ${engineName}: pwa: ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-pwa-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};

const composer = () => page.locator(".composer textarea");
// textContent, not innerText: a headless Chromium without fonts (no
// FONTCONFIG_FILE) renders no text, and innerText is only what is rendered.
const bodyHas = text => page.waitForFunction(t => document.body.textContent.includes(t), text);
const idle = () => page.waitForFunction(() => !document.querySelector(".streaming, .btn.stop"));
// The caches the page's origin has: their names (the build hash normalised)
// and the paths in them.
const caches = () => page.evaluate(async () => {
  const out = [];
  for (const name of await caches.keys()) {
    const cache = await caches.open(name);
    const paths = (await cache.keys()).map(r => new URL(r.url).pathname + new URL(r.url).search);
    out.push({ name, paths: paths.sort() });
  }
  return out;
});
const swVersion = () => readFileSync(join(site, "sw.js"), "utf8").match(/const VERSION = "([^"]+)"/)[1];
const shell = JSON.parse(readFileSync(join(site, "sw.js"), "utf8").match(/const SHELL = (\[[^\]]*\])/)[1]);
// A request from the page, as [status] or the error it failed with.
const request = (path, init) => page.evaluate(async ([path, init]) => {
  try { return String((await fetch(path, init)).status); } catch (e) { return `failed (${e.name})`; }
}, [path, init]);

try {
  // 1. The manifest and what it names.
  section("manifest");
  await page.goto(url);
  const head = await page.evaluate(() => ({
    manifest: document.querySelector("link[rel=manifest]")?.getAttribute("href"),
    themeColor: document.querySelector("meta[name=theme-color]")?.content,
    appleIcon: document.querySelector("link[rel=apple-touch-icon]")?.getAttribute("href"),
    appleCapable: document.querySelector("meta[name=apple-mobile-web-app-capable]")?.content,
    appleTitle: document.querySelector("meta[name=apple-mobile-web-app-title]")?.content,
  }));
  console.log(JSON.stringify(head));
  const manifest = await page.request.get(new URL(head.manifest, url).href);
  console.log(`status: ${manifest.status()}`);
  console.log(`content-type: ${manifest.headers()["content-type"]}`);
  const m = JSON.parse(await manifest.text());
  for (const key of ["name", "short_name", "id", "start_url", "scope", "display", "theme_color", "background_color"])
    console.log(`${key}: ${m[key]}`);
  // The theme's background, as the page paints it before signing in.
  const bg = await page.evaluate(() => getComputedStyle(document.body).backgroundColor);
  console.log(`page background: ${bg}`);
  for (const icon of m.icons) {
    const response = await page.request.get(new URL(icon.src, url).href);
    let size = "";
    if (icon.type === "image/png") {
      const png = await response.body();
      size = ` ${png.readUInt32BE(16)}x${png.readUInt32BE(20)}`;
    }
    console.log(`icon ${icon.src} ${icon.sizes} ${icon.purpose}: ${response.status()} ${response.headers()["content-type"]}${size}`);
  }
  const touch = await page.request.get(new URL(head.appleIcon, url).href);
  console.log(`apple-touch-icon: ${touch.status()} ${touch.headers()["content-type"]}`);

  // 2. Signed in, the worker installs and takes the page over.
  section("service worker");
  await page.waitForSelector("form input[type=password]");
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await composer().waitFor();
  // Installing fetches the whole shell: the dev build's main.bc.js is ~50 MB.
  await page.waitForFunction(() => navigator.serviceWorker.controller?.state === "activated",
    null, { timeout: 90000 });
  console.log(`controls the page: ${await page.evaluate(() => !!navigator.serviceWorker.controller)}`);
  console.log(`script: ${await page.evaluate(() => new URL(navigator.serviceWorker.controller.scriptURL).pathname)}`);
  console.log(`scope: ${await page.evaluate(async () => new URL((await navigator.serviceWorker.ready).scope).pathname)}`);
  const first = swVersion();
  let cached = await caches();
  console.log(`caches: ${cached.map(c => c.name.replace(first, "<BUILD>")).join(", ")}`);
  const expected = shell.map(name => `/${name}`).sort();
  console.log(`cached the shell: ${JSON.stringify(cached[0].paths) === JSON.stringify(expected)}`);
  for (const name of ["index.html", "main.bc.js", "theme.css", "recursive-sans.woff2", "katex.min.js", "KaTeX_Main-Regular.woff2", "xterm.js", "icon-192.png", "manifest.webmanifest"])
    console.log(`  ${name}: ${cached[0].paths.includes(`/${name}`)}`);

  // The title bar follows the theme.
  const themeColor = () => page.evaluate(() =>
    `${document.querySelector("meta[name=theme-color]").content} (page ${getComputedStyle(document.body).backgroundColor})`);
  console.log(`theme-color: ${await themeColor()}`);
  await composer().fill("/theme light");
  await composer().press("Enter");
  await page.waitForFunction(() => document.documentElement.getAttribute("data-theme") === "light"
    && document.querySelector("meta[name=theme-color]").content === getComputedStyle(document.body).backgroundColor);
  console.log(`theme-color, light: ${await themeColor()}`);
  await composer().fill("/theme original");
  await composer().press("Enter");
  await page.waitForFunction(() => document.querySelector("meta[name=theme-color]").content === "rgb(27, 30, 35)");

  // 3. The RPC (a WebSocket) works under it, and nothing it or the other
  //    endpoints answer is kept.
  section("not cached");
  await composer().fill("Hello?");
  await page.locator(".btn.send").click();
  await bodyHas("First answer.");
  await idle();
  console.log(`a turn over the RPC: ${await page.locator("#chat").textContent().then(t => t.includes("First answer."))}`);
  console.log(`file download: ${await request("files/download?ticket=nope")}`);
  console.log(`sign-in link: ${await request("signin-handoff", { method: "POST", body: "nope" })}`);
  console.log(`unknown path: ${await request("no-such-thing")}`);
  cached = await caches();
  console.log(`only the shell cached: ${cached.length === 1 && cached[0].paths.every(p => expected.includes(p))}`);
  await screenshot("installed");

  // 4. The backend stopped: a reload (an installed app launched offline)
  //    opens the app, reconnecting; the endpoints fail as the network does.
  await stopServer();
  await page.reload();
  await page.waitForFunction(() => document.querySelector(".banner")?.textContent.includes("reconnecting"));
  section("reloaded with the backend stopped");
  console.log(`banner: ${(await page.locator(".banner").textContent()).replace(/Retry now$/, "").replace(/ \(attempt \d+, next in [\d.]+s\)/, "")}`);
  console.log(`sign-in page: ${await page.locator("#signin-form").count() > 0}`);
  console.log(`composer: ${await composer().count()}`);
  console.log(`file download: ${await request("files/download?ticket=nope")}`);
  console.log(`sign-in link: ${await request("signin-handoff", { method: "POST", body: "nope" })}`);
  console.log(`main.bc.js: ${await request("main.bc.js")}`);
  await screenshot("offline");

  // 5. Back, with a new build deployed: its worker installs, waits, and the
  //    page says so; Reload moves it over.
  chmodSync(join(site, "sw.js"), 0o644);
  writeFileSync(join(site, "sw.js"),
    readFileSync(join(site, "sw.js"), "utf8").replace(`const VERSION = "${first}"`, `const VERSION = "${first}-next"`));
  await startServer(`127.0.0.1:${port}`);
  await page.evaluate(() => window.dispatchEvent(new Event("online")));
  await page.waitForFunction(() => !document.querySelector(".banner"));
  await bodyHas("First answer.");
  section("a new build");
  console.log(`notice before it: ${await page.locator(".update-notice").count()}`);
  await page.evaluate(async () => (await navigator.serviceWorker.getRegistration()).update());
  await page.locator(".update-notice").waitFor({ timeout: 90000 });
  console.log(`notice: ${await page.locator(".update-notice span").first().textContent()}`);
  console.log(`buttons: ${(await page.locator(".update-notice button").evaluateAll(bs => bs.map(b => `${b.textContent || b.getAttribute("aria-label")} (${b.title})`))).join(", ")}`);
  console.log(`a worker waiting: ${await page.evaluate(async () => !!(await navigator.serviceWorker.getRegistration()).waiting)}`);
  await screenshot("update notice");
  await composer().fill("a draft");
  const reloaded = page.waitForEvent("load");
  await page.locator(".update-notice .toast-undo").click();
  await reloaded;
  await composer().waitFor();
  await bodyHas("First answer.");
  await page.waitForFunction(async () =>
    (await caches.keys()).length === 1 && (await caches.keys())[0].endsWith("-next"));
  cached = await caches();
  console.log(`caches: ${cached.map(c => c.name.replace(first, "<BUILD>")).join(", ")}`);
  console.log(`notice after: ${await page.locator(".update-notice").count()}`);
  console.log(`draft: ${await composer().inputValue()}`);
  await screenshot("updated");

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  await screenshot("failure");
  console.error(errors.join("\n"));
  throw error;
} finally {
  await end(() => stopServer(), () => browser.close());
}
