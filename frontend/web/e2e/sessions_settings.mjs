// The settings page's Sessions section's "Archive after": a session used
// three days ago moves to Archived at once (no restart) when it is set to
// two days, a value it cannot read is refused; and the section on phones
// (390x844, 320x640). Run by prigh_web.sh; prints normalised snapshots (the
// expected output is in ENGINE.expected) and, with SHOTS, screenshots.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { execFileSync, spawn } from "node:child_process";
import { mkdirSync, readFileSync, readdirSync, realpathSync, utimesSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";
import { normalVerbosity } from "./verbose_prefs.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: sessions_settings.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
mkdirSync(join(process.env.TEST_DIR, "sessions_settings"), { recursive: true });
const dir = realpathSync(join(process.env.TEST_DIR, "sessions_settings"));
const home = join(dir, "home");
normalVerbosity(home);
const repo = join(dir, "repo");
for (const d of [home, repo]) mkdirSync(d, { recursive: true });

const git = (cwd, ...args) =>
  execFileSync("git", ["-c", "user.name=e2e", "-c", "user.email=e2e@example.com", ...args], { cwd, stdio: "ignore" });
writeFileSync(join(repo, "README.md"), "# Repo\n");
git(repo, "init", "-q", "-b", "main");
git(repo, "add", "README.md");
git(repo, "commit", "-q", "-m", "start");

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux", "-token", token, "-cwd", repo],
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
const open = async (viewport, phone = false) => {
  const context = await browser.newContext({
    viewport,
    reducedMotion: "reduce",
    ...(phone ? { isMobile: engineName !== "firefox", hasTouch: true } : {}),
  });
  const page = await context.newPage();
  page.on("console", message => {
    if (message.type() === "error" && !/favicon\.ico|interactive-widget|WebSocket/.test(message.text())) errors.push(`console: ${message.text()}`);
  });
  page.on("pageerror", error => errors.push(`page: ${error.message}`));
  return page;
};

const section = label => console.log(`=== ${engineName}: sessions settings: ${label} ===`);
let shot = 0;
const screenshot = async (p, label) => {
  if (!process.env.SHOTS) return;
  shot++;
  await p.screenshot({
    path: join(process.env.SHOTS, `${engineName}-sessions-settings-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const composer = p => p.locator(".composer textarea");
const signIn = async p => {
  await p.goto(url);
  await p.locator("form input[type=password]").fill(token);
  await p.locator("form button[type=submit]").click();
  await composer(p).waitFor();
};
// As typed: a plain Enter may accept the completion popup instead (a /cd
// path's trailing slash) when it opens first.
const send = async (p, text) => {
  await composer(p).fill(text);
  await composer(p).press("Control+Enter");
};
const session = (p, title) =>
  p.locator(".session-item", { has: p.locator(".session-title", { hasText: title }) });
// Each session's title, archived ones marked.
const sidebar = async p => {
  const rows = await p.locator(".sessions .session-item").evaluateAll(items => items.map(item => {
    const title = item.querySelector(".session-title")?.textContent.trim();
    return `${title}${item.querySelector(".session.archived") ? " (archived)" : ""}`;
  }));
  console.log(rows.join("\n"));
};
const toast = (p, text) => p.locator(".toast", { hasText: text }).last().waitFor();
const sessions = p => p.locator(".settings-group", { has: p.locator(".setting.auto-archive") });
const openSettings = async p => {
  await p.keyboard.press("Control+,");
  await p.locator(".settings-dialog").waitFor();
  await p.locator(".settings-tab-sessions").click();
  await p.locator(".setting.auto-archive .switch:not([disabled])").waitFor();
  await sessions(p).scrollIntoViewIfNeeded();
};
const showSettings = async p => {
  console.log((await sessions(p).locator(".setting.auto-archive").allInnerTexts())
    .map(t => t.replace(/\n+/g, " | ")).join("\n"));
  console.log(`field: ${JSON.stringify(await p.locator("#auto-archive").inputValue())} days`);
};
const config = () => {
  const c = JSON.parse(readFileSync(join(home, ".prigh/config.json"), "utf8"));
  return `archive_after_days=${JSON.stringify(c.archive_after_days)}`;
};
// Ages a session by three days: its entries' times and its file's.
const age = title => {
  const days = 3;
  const files = [];
  const walk = d => {
    for (const e of readdirSync(d, { withFileTypes: true })) {
      if (e.isDirectory()) walk(join(d, e.name));
      else if (e.name.endsWith(".jsonl")) files.push(join(d, e.name));
    }
  };
  walk(join(home, ".prigh"));
  const file = files.find(f => readFileSync(f, "utf8").includes(title));
  if (!file) throw new Error(`no session file has ${title}`);
  const shift = days * 86400;
  const text = readFileSync(file, "utf8").replace(/"at":(\d+(?:\.\d+)?)/g, (_, at) => `"at":${Number(at) - shift}`);
  writeFileSync(file, text);
  const then = Date.now() / 1000 - shift;
  utimesSync(file, then, then);
};

const page = await open({ width: 1280, height: 800 });
try {
  await signIn(page);
  // Two sessions, the first used days ago.
  await send(page, "Old work");
  await session(page, "Old work").waitFor();
  await page.locator(".new-session").click();
  await page.waitForFunction(() =>
    !document.querySelector(".new-session.opening, .session-item.selected") && document.querySelector(".hero"));
  await send(page, "Fresh work");
  await session(page, "Fresh work").waitFor();
  // Its agent let go of, so the list reads the file again.
  await page.reload();
  await composer(page).waitFor();
  age("Old work");
  await page.reload();
  await composer(page).waitFor();
  await session(page, "Old work").waitFor();
  section("two sessions, one used days ago");
  await sidebar(page);

  await openSettings(page);
  section("the settings");
  await showSettings(page);
  await screenshot(page, "settings");

  // Refused, then archiving after two days: the old session moves to
  // Archived without a restart.
  await openSettings(page);
  await page.locator("#auto-archive").fill("a fortnight");
  await page.locator("#auto-archive").press("Enter");
  await page.locator(".setting.auto-archive .setting-note").waitFor();
  section("refused");
  console.log(await page.locator(".setting.auto-archive .setting-note").innerText());
  await page.locator("#auto-archive").fill("2");
  await page.locator("#auto-archive").press("Enter");
  await toast(page, "after 2 days");
  section("archive after 2 days");
  await showSettings(page);
  console.log(config());
  await page.keyboard.press("Escape");
  await page.locator(".settings-dialog").waitFor({ state: "detached" });
  await page.waitForFunction(() =>
    ![...document.querySelectorAll(".sessions .session-title")].some(t => t.textContent.includes("Old work")));
  await sidebar(page);
  console.log(`archived: ${(await page.locator(".sessions-divider").innerText()).replace(/\s+/g, " ")}`);
  await screenshot(page, "archived");

  // Back to a week, then the section on phones, at 390x844 and 320x640.
  await openSettings(page);
  await page.locator("#auto-archive").fill("7");
  await page.locator("#auto-archive").press("Enter");
  await toast(page, "after 7 days");
  for (const [width, height] of [[390, 844], [320, 640]]) {
    const phone = await open({ width, height }, true);
    await signIn(phone);
    await openSettings(phone);
    section(`phone ${width}x${height}: settings`);
    console.log(`fits: ${await phone.evaluate(() => {
      const s = document.querySelector(".settings-group:has(.auto-archive)");
      const inside = el => { const r = el.getBoundingClientRect(); return r.left >= 0 && r.right <= innerWidth; };
      return s.scrollWidth <= s.clientWidth
        && [...s.querySelectorAll("input, button")].every(inside);
    })}`);
    await screenshot(phone, `phone-${width}-settings`);
    await phone.context().close();
  }

  section("errors");
  console.log(errors.length ? errors.join("\n") : "(none)");
} catch (e) {
  console.log(`FAILED: ${e.stack ?? e}`);
  await screenshot(page, "failed");
  process.exitCode = 1;
} finally {
  await end(() => browser.close(), () => server.kill());
}
