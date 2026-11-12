// The sidebar's worktrees by how recently their sessions were used, and the
// settings page's Sessions rows for it and for archiving: a session used
// three days ago has its worktrees folded behind ⑂, one used now shows
// them; "Show worktrees" widens the window, "Archive after" moves the old
// session to Archived at once (no restart); both on a desktop and on
// phones (390x844, 320x640). Run by prigh_web.sh; prints normalised
// snapshots (the expected output is in ENGINE.expected) and, with SHOTS,
// screenshots.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { execFileSync, spawn } from "node:child_process";
import { mkdirSync, readFileSync, readdirSync, realpathSync, utimesSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: sessions_settings.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
mkdirSync(join(process.env.TEST_DIR, "sessions_settings"), { recursive: true });
const dir = realpathSync(join(process.env.TEST_DIR, "sessions_settings"));
const home = join(dir, "home");
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
const send = async (p, text) => {
  await composer(p).fill(text);
  await composer(p).press("Enter");
};
const session = (p, title) =>
  p.locator(".session-item", { has: p.locator(".session-title", { hasText: title }) });
// Each session's title, its fold (+ shown, - folded) and the worktrees shown.
const sidebar = async p => {
  const rows = await p.locator(".sessions .session-item").evaluateAll(items => items.map(item => {
    const title = item.querySelector(".session-title")?.textContent.trim();
    const fold = item.querySelector(".session-fold");
    const mark = fold ? (fold.classList.contains("folded") ? "-" : "+") : " ";
    const wts = [...item.querySelectorAll(".worktree-branch")].map(b => b.textContent.trim());
    const archived = item.querySelector(".session.archived") ? " (archived)" : "";
    return `${mark} ${title}${archived}${wts.length ? `: ${wts.join(", ")}` : ""}`;
  }));
  console.log(rows.join("\n"));
};
const toast = (p, text) => p.locator(".toast", { hasText: text }).last().waitFor();
const sessions = p => p.locator(".settings-group", { has: p.locator(".setting.worktree-hours") });
const openSettings = async p => {
  await p.keyboard.press("Control+,");
  await p.locator(".settings-dialog").waitFor();
  await p.locator(".settings-tab-sessions").click();
  await p.locator("#auto-archive:not([disabled])").waitFor();
  await sessions(p).scrollIntoViewIfNeeded();
};
const showSettings = async p => {
  console.log((await sessions(p).locator(".setting.worktree-hours, .setting.auto-archive").allInnerTexts())
    .map(t => t.replace(/\n+/g, " | ")).join("\n"));
  console.log(`fields: ${JSON.stringify(await p.locator("#worktree-hours").inputValue())} hours, \
${JSON.stringify(await p.locator("#auto-archive").inputValue())} days`);
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
  // Two sessions, each in worktrees of its own; the first used days ago.
  await send(page, "Old work");
  await session(page, "Old work").waitFor();
  await send(page, "/worktree new feat/old-a");
  await page.locator(".worktree-branch", { hasText: "feat/old-a" }).waitFor();
  await send(page, "/worktree new feat/old-b");
  await page.locator(".worktree-branch", { hasText: "feat/old-b" }).waitFor();
  await page.locator(".new-session").click();
  await page.waitForFunction(() => !document.querySelector(".new-session.opening"));
  await send(page, `/cd ${repo}`);
  await page.locator(".toast", { hasText: "repo" }).first().waitFor().catch(() => {});
  await send(page, "Fresh work");
  await session(page, "Fresh work").waitFor();
  await send(page, "/worktree new fix/fresh");
  await page.locator(".worktree-branch", { hasText: "fix/fresh" }).waitFor();
  // Its agent let go of, so the list reads the file again.
  await page.reload();
  await composer(page).waitFor();
  age("Old work");
  await page.reload();
  await composer(page).waitFor();
  await session(page, "Old work").locator(".session-fold.folded").waitFor();
  await page.mouse.move(1000, 400);
  section("used days ago: folded, used now: shown");
  await sidebar(page);
  await screenshot(page, "sidebar");

  await openSettings(page);
  section("the settings");
  await showSettings(page);
  await screenshot(page, "settings");

  // A wider window shows the old session's too; Off folds them all.
  await page.locator("#worktree-hours").fill("100");
  await page.locator("#worktree-hours").press("Enter");
  await page.locator(".setting.worktree-hours .setting-note, .setting.worktree-hours", { hasText: "100 hours" }).first().waitFor();
  await page.keyboard.press("Escape");
  await page.locator(".settings-dialog").waitFor({ state: "detached" });
  await session(page, "Old work").locator(".worktree-branch", { hasText: "feat/old-a" }).waitFor();
  section("100 hours");
  await sidebar(page);
  await openSettings(page);
  await sessions(page).locator(".setting.worktree-hours button", { hasText: "Off" }).click();
  await page.keyboard.press("Escape");
  await page.locator(".settings-dialog").waitFor({ state: "detached" });
  await session(page, "Fresh work").locator(".session-fold.folded").waitFor();
  section("off");
  await sidebar(page);
  // The user's own unfold wins over the setting and the age.
  await session(page, "Old work").locator(".session-fold").click();
  await session(page, "Old work").locator(".worktree-branch", { hasText: "feat/old-a" }).waitFor();
  section("off, Old work unfolded by hand");
  await sidebar(page);

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

  // Phones: the sidebar, then the settings section, at 390x844 and 320x640.
  await openSettings(page);
  await page.locator("#worktree-hours").fill("100");
  await page.locator("#worktree-hours").press("Enter");
  await page.locator("#auto-archive").fill("7");
  await page.locator("#auto-archive").press("Enter");
  await toast(page, "after 7 days");
  for (const [width, height] of [[390, 844], [320, 640]]) {
    const phone = await open({ width, height }, true);
    await signIn(phone);
    await phone.locator(".chat-chrome .toggle-sidebar").click();
    await session(phone, "Old work").locator(".worktree-branch", { hasText: "feat/old-a" }).waitFor();
    await phone.waitForTimeout(400); // the sidebar slides in
    section(`phone ${width}x${height}: sidebar`);
    await sidebar(phone);
    await screenshot(phone, `phone-${width}-sidebar`);
    await phone.locator(".chat-chrome .toggle-sidebar, .sidebar-close").first().click().catch(() => {});
    await phone.evaluate(() => document.activeElement?.blur());
    await openSettings(phone);
    section(`phone ${width}x${height}: settings`);
    console.log(`fits: ${await phone.evaluate(() => {
      const s = document.querySelector(".settings-group:has(.worktree-hours)");
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
