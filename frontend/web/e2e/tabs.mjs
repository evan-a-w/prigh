// Session tabs (/tabs, off by default): the settings' switch turns them on
// for the account (saved in ~/.prigh/prefs.json); sessions opened get tabs
// that say how they stand (failed, waiting for you); tabs switch from the
// keyboard, move by dragging and close; a reload keeps them in their order,
// another browser signed in as the same user has them on, and a phone shows
// none. Run by prigh_web.sh; prints normalised snapshots (the expected
// output is in ENGINE.expected) and, with SHOTS, screenshots.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: tabs.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "tabs");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });
writeFileSync(
  join(dir, "script.json"),
  JSON.stringify([
    { text: "Read it: nothing to change." },
    { text: "", stop_reason: "error", error: "HTTP 400: the request was malformed" },
    { text: "Listing.", tool_calls: [{ id: "b1", name: "bash", arguments: { command: "ls" } }] },
    { text: "Listed." },
  ]),
);

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
    "-token", token, "-cwd", cwd],
  { env: { ...process.env, HOME: home, TMUX_TMPDIR: dir, SHELL: "/bin/sh" }, stdio: ["ignore", "ignore", "pipe"] },
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

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
const errors = [];
const open = async (viewport = { width: 1280, height: 800 }) => {
  const page = await (await browser.newContext({ viewport })).newPage();
  page.on("console", message => {
    if (message.type() === "error" && !/favicon\.ico|WebSocket/.test(message.text())) errors.push(`console: ${message.text()}`);
  });
  page.on("pageerror", error => errors.push(`page: ${error.message}`));
  return page;
};
const page = await open();

const section = label => console.log(`=== ${engineName}: tabs: ${label} ===`);
let shot = 0;
const screenshot = async (label, p = page) => {
  if (!process.env.SHOTS) return;
  shot++;
  await p.screenshot({
    path: join(process.env.SHOTS, `${engineName}-tabs-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const composer = p => p.locator(".composer textarea");
const signIn = async p => {
  await p.goto(url);
  await p.locator("form input[type=password]").fill(token);
  await p.locator("form button[type=submit]").click();
  await composer(p).waitFor();
};
const command = async (p, text) => {
  await p.waitForSelector(".modal-backdrop", { state: "detached" });
  await composer(p).fill(text);
  await composer(p).press(text.includes(" ") || !text.startsWith("/") ? "Enter" : "Control+Enter");
  await p.waitForFunction(() => document.querySelector(".composer textarea").value === "");
};
// The strip: [*] before the active tab, then each tab's title and what its
// mark says.
const strip = p => p.evaluate(() => {
  const nav = document.querySelector(".session-tabs");
  if (!nav) return "(no tabs)";
  return [...nav.querySelectorAll(".session-tab")].map(tab => {
    const title = tab.querySelector(".session-tab-title").textContent;
    const mark = tab.querySelector(".lamp, .mark");
    const state = mark ? ` (${mark.getAttribute("aria-label") ?? mark.getAttribute("title")})` : "";
    return `${tab.classList.contains("active") ? "*" : " "} ${title}${state}`;
  }).join("\n");
});
const stripIs = (p, expected) => p.waitForFunction(expected => {
  const nav = document.querySelector(".session-tabs");
  const titles = nav ? [...nav.querySelectorAll(".session-tab")].map(t =>
    (t.classList.contains("active") ? "*" : "") + t.querySelector(".session-tab-title").textContent) : [];
  return titles.join("|") === expected;
}, expected.join("|"));
const prefsFile = () => {
  try {
    const prefs = JSON.parse(readFileSync(join(home, ".prigh/prefs.json"), "utf8"));
    return `web.tabs=${JSON.stringify(prefs["web.tabs"])} web.open-tabs=${(prefs["web.open-tabs"] ?? []).length}`;
  } catch {
    return "(none)";
  }
};
const idle = p => p.waitForFunction(() => !document.querySelector(".streaming, .btn.stop"));

try {
  await signIn(page);
  section("off by default");
  console.log(await strip(page));

  // The settings page's switch turns them on.
  await page.locator(".settings-toggle").click();
  await page.locator(".settings-dialog").waitFor();
  const toggle = page.locator(".setting.tabs [role=switch]");
  console.log(`switch: ${await toggle.getAttribute("aria-checked")}`);
  await toggle.click();
  await page.waitForSelector(".setting.tabs [role=switch][aria-checked=true]");
  await page.keyboard.press("Escape");
  await page.locator(".session-tabs").waitFor();
  section("on");
  console.log(await strip(page));

  // Three sessions: one that answers, one whose reply fails, one waiting
  // for a confirmation.
  await command(page, "Read the README");
  await page.locator(".msg.assistant", { hasText: "nothing to change" }).waitFor();
  await idle(page);
  await stripIs(page, ["*Read the README"]);
  await command(page, "/new");
  await command(page, "Refactor the parser");
  await page.locator(".session-tab.active.failed").waitFor();
  await idle(page);
  await stripIs(page, ["Read the README", "*Refactor the parser"]);
  await command(page, "/confirm on");
  await command(page, "/new");
  await command(page, "List the files");
  await page.locator("#confirm").waitFor();
  await page.locator(".session-tab.active.waiting").waitFor();
  await stripIs(page, ["Read the README", "Refactor the parser", "*List the files"]);
  section("three sessions");
  console.log(await strip(page));
  await screenshot("a confirmation");

  // Another browser signed in as the same user: tabs on, the same ones, and
  // the one waiting for a confirmation says so.
  const other = await open();
  await signIn(other);
  await other.locator(".session-tab.waiting").waitFor();
  await other.locator(".session-tab.failed").waitFor();
  section("another browser");
  console.log(await strip(other));
  await screenshot("another browser", other);
  // Answered in the first: it stops waiting in the other.
  await page.locator("#confirm .confirm-answer.allow").click();
  await page.locator(".msg.assistant", { hasText: "Listed." }).waitFor();
  await idle(page);
  await other.waitForSelector(".session-tab.waiting", { state: "detached" });
  await other.context().close();

  // The keyboard: the Nth, the next, the previous (wrapping).
  await composer(page).click();
  await page.keyboard.press("Alt+Shift+Digit1");
  await stripIs(page, ["*Read the README", "Refactor the parser", "List the files"]);
  await page.keyboard.press("Alt+Shift+BracketRight");
  await stripIs(page, ["Read the README", "*Refactor the parser", "List the files"]);
  await page.keyboard.press("Alt+Shift+BracketLeft");
  await page.keyboard.press("Alt+Shift+BracketLeft");
  await stripIs(page, ["Read the README", "Refactor the parser", "*List the files"]);
  section("switched from the keyboard");
  console.log(await strip(page));

  // Dragging the last tab before the first.
  await page.locator(".session-tab").nth(2).dragTo(page.locator(".session-tab").nth(0));
  await stripIs(page, ["*List the files", "Read the README", "Refactor the parser"]);
  section("dragged");
  console.log(await strip(page));
  for (let i = 0; i < 50 && !prefsFile().endsWith("=3"); i++) await page.waitForTimeout(100);
  console.log(`saved: ${prefsFile()}`);

  // A reload keeps them, in that order.
  await page.reload();
  await composer(page).waitFor();
  await stripIs(page, ["*List the files", "Read the README", "Refactor the parser"]);
  section("reloaded");
  console.log(await strip(page));
  await page.locator(".msg.assistant", { hasText: "Listed." }).waitFor();
  await page.locator(".session-tab").nth(2).hover();
  await screenshot("reloaded");

  // Closing: another tab (its ×), then the active one (Alt+W) moves to its
  // neighbour.
  await page.locator(".session-tab").nth(2).hover();
  await page.locator(".session-tab").nth(2).locator(".session-tab-close").click();
  await stripIs(page, ["*List the files", "Read the README"]);
  await composer(page).click();
  await page.keyboard.press("Alt+KeyW");
  await stripIs(page, ["*Read the README"]);
  section("closed");
  console.log(await strip(page));
  // The sidebar still has every session.
  console.log(`sidebar: ${await page.locator(".sidebar .session").count()} sessions`);

  // A phone: no strip.
  const phone = await open({ width: 390, height: 844 });
  await signIn(phone);
  await phone.locator(".composer").waitFor();
  await phone.waitForFunction(() => document.getAnimations().every(a => a.playState !== "running"));
  section("phone");
  console.log(await strip(phone));
  await screenshot("phone", phone);
  await phone.context().close();

  // Off again: the page as it was.
  await command(page, "/tabs off");
  await page.waitForSelector(".session-tabs", { state: "detached" });
  section("off");
  for (let i = 0; i < 50 && !prefsFile().startsWith("web.tabs=undefined"); i++) await page.waitForTimeout(100);
  console.log(`${await strip(page)}; saved: ${prefsFile()}`);

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  await screenshot("failure");
  console.error(errors.join("\n"));
  throw error;
} finally {
  await end(() => server.kill(), () => browser.close());
}
