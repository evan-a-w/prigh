// `prigh desktop`, the desktop app's process, as its window runs it: with no
// saved connection the page is the connect form, whose mistakes say what to
// do; once a backend accepts it the page is signed in and its tools run on
// this machine; /quit stops the process. Started again it uses the saved
// connection (a phone-sized window: Quit prigh is in the ⋯ sheet), and
// without one on a small phone the form fits; closing its stdin (the window
// gone) stops it too. Run by prigh_web.sh; prints normalised snapshots (the
// expected output is in ENGINE.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { existsSync, mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: desktop.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site } = process.env;
const dir = join(process.env.TEST_DIR, "desktop");
const dirs = {};
for (const name of ["server-home", "server-cwd", "laptop-home", "laptop-cwd", "fresh-home"]) {
  dirs[name] = join(dir, name);
  mkdirSync(dirs[name], { recursive: true });
}
writeFileSync(join(dirs["laptop-cwd"], "on-the-laptop.txt"), "");
writeFileSync(
  join(dir, "script.json"),
  JSON.stringify([
    { text: "looking", tool_calls: [{ id: "c1", name: "bash", arguments: { command: "pwd; ls" } }] },
    { text: "found it" },
  ]),
);

const waitFor = (proc, pattern, what) => new Promise((resolve, reject) => {
  let err = "";
  proc.stderr.on("data", data => {
    err += data;
    const m = err.match(pattern);
    if (m) resolve(m[1]);
  });
  proc.on("exit", code => reject(new Error(`${what} exited (${code}):\n${err}`)));
});

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
    "-token", "sekrit", "-cwd", dirs["server-cwd"]],
  { env: { ...process.env, HOME: dirs["server-home"] }, stdio: ["ignore", "ignore", "pipe"] },
);
const serverAddress = await waitFor(server, /^prigh: prigh-web on http:\/\/(\S+)\/$/m, "the backend");

// `prigh desktop` in [home], its stdin a pipe the test holds as the window
// would. Resolves with the process, its URL (its one stdout line) and a
// promise of its exit.
const desktops = [];
const startDesktop = home => new Promise((resolve, reject) => {
  quitting = false;
  const env = { ...process.env, HOME: home };
  for (const name of ["PRIGH_CONNECT", "PRIGH_TOKEN", "PRIGH_USER"]) delete env[name];
  const proc = spawn(
    backend,
    ["desktop", "-ui", "local", "-listen", "127.0.0.1:0", "-web-root", site, "-cwd", dirs["laptop-cwd"],
      "-name", "laptop", "-host-id", "host-laptop"],
    { env, stdio: ["pipe", "pipe", "pipe"] },
  );
  desktops.push(proc);
  let out = "";
  let err = "";
  proc.stderr.on("data", data => { err += data; });
  const exited = new Promise(done => proc.on("exit", (code, signal) => done({ code, signal, out, err })));
  proc.stdout.on("data", data => {
    out += data;
    const line = out.split("\n")[0];
    if (out.includes("\n")) resolve({ proc, url: JSON.parse(line).url, line, exited });
  });
  proc.on("exit", code => reject(new Error(`prigh desktop exited (${code}) before its URL:\n${err}`)));
});

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
// Once it quits, the page cannot reach it: what it logs then is expected.
let quitting = false;
const errors = [];
const watch = p => {
  p.on("console", message => {
    if (message.type() === "error" && !quitting && !/favicon\.ico/.test(message.text()))
      errors.push(`console: ${message.text()}`);
  });
  p.on("pageerror", error => errors.push(`page: ${error.message}`));
};
const phone = (width, height) => {
  const options = { viewport: { width, height }, hasTouch: true };
  if (engineName !== "firefox") options.isMobile = true;
  return options;
};

const clean = text => text
  .replaceAll(dir, "$DIR")
  .replace(/127\.0\.0\.1:\d+/g, "127.0.0.1:<PORT>")
  .replace(/#desktop=[0-9a-f]+/g, "#desktop=<key>")
  .replace(/session=[0-9a-f]+/g, "session=<id>")
  .split("\n")
  .map(line => line.trim())
  .filter(line => line !== "")
  .join("\n");
const section = label => console.log(`=== ${engineName}: desktop: ${label} ===`);
let shot = 0;
const screenshot = async (p, label) => {
  if (!process.env.SHOTS) return;
  shot++;
  await p.screenshot({
    path: join(process.env.SHOTS, `${engineName}-desktop-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const exitOf = async ({ exited }) => {
  const { code, signal, out, err } = await exited;
  console.log(`exit: ${code ?? signal}; more stdout: ${JSON.stringify(out.split("\n").slice(1).join(""))}`);
  console.log(clean(err.split("\n").filter(l => /quit|stdin closed/.test(l)).join("\n")));
};
// Nothing sticks out sideways, and the form's controls are on screen.
const fits = p => p.evaluate(() => {
  const width = document.documentElement.clientWidth;
  const off = [...document.querySelectorAll(".signin input, .signin button")]
    .filter(e => { const r = e.getBoundingClientRect(); return r.left < 0 || r.right > width; })
    .map(e => e.id);
  return `overflow: ${document.documentElement.scrollWidth > width}; off screen: ${off.join(" ") || "none"}`;
});

let page;
try {
  // 1. No saved connection: the connect form.
  const first = await startDesktop(dirs["laptop-home"]);
  section("its stdout");
  console.log(clean(first.line));
  page = await (await browser.newContext({ viewport: { width: 1100, height: 760 } })).newPage();
  watch(page);
  await page.goto(first.url);
  await page.locator("#desktop-form").waitFor();
  section("the connect form");
  console.log(clean(page.url()));
  console.log(clean(await page.locator(".signin").innerText()));
  await screenshot(page, "connect form");

  // Unencrypted to another machine: the risk, the alternatives, and a
  // second, explicit step (0.0.0.0 reaches nothing here, at once).
  await page.locator("#address").fill("0.0.0.0:1");
  await page.locator("#password").fill("sekrit");
  await page.locator("#desktop-connect").click();
  await page.locator("#desktop-connect.danger").waitFor();
  section("unencrypted");
  console.log(clean(await page.locator(".signin-error").innerText()));
  console.log(`button: ${await page.locator("#desktop-connect").innerText()}`);
  await screenshot(page, "unencrypted");
  await page.locator("#password").fill("sekrit");
  await page.locator("#desktop-connect").click();
  await page.waitForFunction(() => document.querySelector(".signin-error")?.innerText.includes("cannot reach"));
  section("unencrypted, confirmed");
  console.log(clean(await page.locator(".signin-error").innerText()));
  console.log(`button: ${await page.locator("#desktop-connect").innerText()}`);

  // A wrong password says what to do, and keeps the address.
  await page.locator("#address").fill(serverAddress);
  await page.locator("#password").fill("wrong");
  await page.locator("#desktop-connect").click();
  await page.locator(".signin-error:not(:empty)").waitFor();
  section("a wrong password");
  console.log(clean(await page.locator(".signin-error").innerText()));
  console.log(`address kept: ${(await page.locator("#address").inputValue()) === serverAddress}`);
  console.log(`saved: ${existsSync(join(dirs["laptop-home"], ".prigh", "tui-connection.sexp"))}`);
  await screenshot(page, "wrong password");

  // The right one: signed in, the tools on this machine, the connection saved.
  await page.locator("#password").fill("sekrit");
  await page.locator("#desktop-connect").click();
  await page.locator(".composer textarea").waitFor();
  section("connected");
  console.log(clean(page.url()));
  console.log(`saved: ${existsSync(join(dirs["laptop-home"], ".prigh", "tui-connection.sexp"))}`);
  await page.locator(".composer textarea").fill("where am I?");
  await page.locator(".btn.send").click();
  await page.waitForFunction(() => document.body.innerText.includes("found it"));
  await page.locator(".tool button.tool-head").last().click();
  await page.getByText("on-the-laptop.txt").first().waitFor();
  const chat = await page.locator("#chat").innerText();
  console.log(`in the laptop's directory: ${chat.includes(dirs["laptop-cwd"])}`);
  await screenshot(page, "connected");

  // A reload stays signed in, still the desktop app's page.
  await page.reload();
  await page.locator(".composer textarea").waitFor();
  section("reloaded");
  console.log(`connect form: ${await page.locator("#desktop-form").count()}`);

  // /quit stops it.
  quitting = true;
  await page.locator(".composer textarea").fill("/quit");
  await page.locator(".composer textarea").press("Enter");
  await page.getByText("Quitting prigh").waitFor();
  section("/quit");
  console.log(clean(await page.locator(".desktop-quitting").innerText()));
  await exitOf(first);
  await page.context().close();

  // 2. Started again: the saved connection, straight in. On a phone the ⋯
  // sheet has Quit prigh.
  const second = await startDesktop(dirs["laptop-home"]);
  page = await (await browser.newContext(phone(390, 844))).newPage();
  watch(page);
  await page.goto(second.url);
  await page.locator(".composer textarea").waitFor();
  section("again, on a phone: the saved connection");
  console.log(`connect form: ${await page.locator("#desktop-form").count()}`);
  await page.locator(".chrome-more .more-toggle").tap();
  await page.locator("#chrome-menu .quit-row").waitFor();
  await page.locator("#chrome-menu .quit-row").scrollIntoViewIfNeeded();
  console.log(clean(await page.locator("#chrome-menu .quit-row").innerText()));
  await screenshot(page, "phone sheet");
  quitting = true;
  await page.locator("#chrome-menu .quit-row").tap();
  await exitOf(second);
  await page.context().close();

  // 3. A small phone with no saved connection: the form fits; the window
  // closing (stdin's end) stops it.
  const third = await startDesktop(dirs["fresh-home"]);
  page = await (await browser.newContext(phone(320, 640))).newPage();
  watch(page);
  await page.goto(third.url);
  await page.locator("#desktop-form").waitFor();
  section("a small phone");
  console.log(await fits(page));
  const sizes = await page.evaluate(() =>
    [...document.querySelectorAll(".signin input, .signin button")]
      .map(e => `${e.id} ${getComputedStyle(e).fontSize} ${Math.round(e.getBoundingClientRect().height) >= 44 || e.tagName === "INPUT" ? "ok" : "small"}`));
  console.log(sizes.join("\n"));
  await screenshot(page, "small phone form");
  await page.locator("#address").tap();
  await screenshot(page, "small phone focused");
  quitting = true;
  third.proc.stdin.end();
  section("its window gone");
  await exitOf(third);

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  if (page) await screenshot(page, "failure").catch(() => {});
  console.error(errors.join("\n"));
  throw error;
} finally {
  await end(...desktops.map(d => () => d.kill()), () => server.kill(), () => browser.close());
}
