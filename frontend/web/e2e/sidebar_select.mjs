// The sidebar's selection by click and the Archive row's menu: Ctrl (Cmd)
// clicks sessions into a selection, Shift selects a range from the session
// on show, Esc ends it, the bar archives them; the Archive row's right-click
// menu shows the archived sessions and deletes them all, after asking. Run
// by prigh_web.sh; prints normalised snapshots (the expected output is in
// ENGINE.expected) and, with SHOTS, screenshots.
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
if (!engine) throw new Error("usage: sidebar_select.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
mkdirSync(join(process.env.TEST_DIR, "sidebar_select"), { recursive: true });
const dir = realpathSync(join(process.env.TEST_DIR, "sidebar_select"));
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

const section = label => console.log(`=== ${engineName}: sidebar select: ${label} ===`);
let shot = 0;
const screenshot = async (p, label) => {
  if (!process.env.SHOTS) return;
  shot++;
  await p.screenshot({
    path: join(process.env.SHOTS, `${engineName}-sidebar-select-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
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
// Each session's title, selected and archived ones marked ([sort]: sessions
// archived together, whose order is the backend's).
const sidebar = async (p, sort = false) => {
  const rows = await p.locator(".sessions .session-item").evaluateAll(items => items.map(item => {
    const row = item.querySelector(".session");
    const open = item.querySelector(".session-open");
    const title = item.querySelector(".session-title")?.textContent.trim();
    return `${open?.getAttribute("aria-checked") === "true" ? "[x] " : ""}${title}${row?.classList.contains("archived") ? " (archived)" : ""}`;
  }));
  console.log((sort ? rows.sort() : rows).join("\n"));
};
const bar = async p => console.log(`bar: ${(await p.locator(".selection-bar").innerText()).replace(/\s+/g, " ")}`);
const newSession = async (p, text) => {
  await p.locator(".new-session").click();
  await p.waitForFunction(() =>
    !document.querySelector(".new-session.opening, .session-item.selected") && document.querySelector(".hero"));
  await send(p, text);
  await session(p, text).waitFor();
};

const page = await open({ width: 1280, height: 800 });
try {
  await signIn(page);
  await send(page, "First");
  await session(page, "First").waitFor();
  await newSession(page, "Second");
  await newSession(page, "Third");
  await newSession(page, "Fourth");
  section("four sessions, the newest on show");
  await sidebar(page);

  // Ctrl (Cmd on a Mac) toggles a session into the selection; the session
  // on show stays on show.
  await session(page, "Second").locator(".session-open").click({ modifiers: ["ControlOrMeta"] });
  await page.locator(".selection-bar").waitFor();
  await session(page, "First").locator(".session-open").click({ modifiers: ["ControlOrMeta"] });
  await page.waitForFunction(() => document.querySelectorAll(".session-open[aria-checked=true]").length === 2);
  section("Ctrl+click two");
  await sidebar(page);
  await bar(page);
  console.log(`still on show: ${await page.locator(".session-item.selected .session-title").innerText()}`);
  await screenshot(page, "ctrl-click");
  await page.keyboard.press("Escape");
  await page.locator(".selection-bar").waitFor({ state: "detached" });
  section("Esc");
  await sidebar(page);

  // Shift: the range from the session on show.
  await session(page, "Second").locator(".session-open").click({ modifiers: ["Shift"] });
  await page.waitForFunction(() => document.querySelectorAll(".session-open[aria-checked=true]").length === 3);
  section("Shift+click: from the one on show");
  await sidebar(page);
  await bar(page);
  // Archive them: the bar's Archive.
  await session(page, "Fourth").locator(".session-open").click({ modifiers: ["ControlOrMeta"] });
  await page.waitForFunction(() => document.querySelectorAll(".session-open[aria-checked=true]").length === 2);
  await page.locator(".selection-bar .archive-marked").click();
  await page.locator(".selection-bar").waitFor({ state: "detached" });
  await page.waitForFunction(() => document.querySelector(".archive-toggle")?.innerText.includes("2"));
  section("two archived from the bar");
  await sidebar(page);
  console.log(`archive row: ${(await page.locator(".archive-toggle").innerText()).replace(/\s+/g, " ")}`);

  // The Archive row's menu: show them, then delete them all (asked first;
  // Esc cancels).
  await page.locator(".archive-toggle").click({ button: "right" });
  await page.waitForFunction(() =>
    document.querySelector(".context-menu.placed [data-item=delete_archived]")?.innerText.includes("Delete"));
  section("the Archive row's menu");
  console.log((await page.locator(".context-menu.placed :is(.context-menu-head, [role=menuitem])").allInnerTexts())
    .map(t => t.replace(/\s*\n+\s*/g, " ")).join(" | "));
  await screenshot(page, "archive-menu");
  await page.locator(".context-menu.placed [data-item=show_archived]").click();
  await page.locator(".session.archived").first().waitFor();
  section("shown");
  await sidebar(page, true);
  await page.locator(".archive-toggle").click({ button: "right" });
  await page.locator(".context-menu.placed [data-item=delete_archived]").click();
  await page.locator(".modal").waitFor();
  section("delete all archived: asked");
  console.log((await page.locator(".modal").innerText())
    .replace(/\d+(\.\d+)? ([KMG]?B)\b/g, "<SIZE>").replace(/\n+/g, " | "));
  await screenshot(page, "delete-archived");
  await page.keyboard.press("Escape");
  await page.locator(".modal").waitFor({ state: "detached" });
  console.log(`after Esc, archived: ${await page.locator(".session.archived").count()}`);
  await page.locator(".archive-toggle").click({ button: "right" });
  await page.locator(".context-menu.placed [data-item=delete_archived]").click();
  await page.locator(".modal .btn.danger").click();
  await page.locator(".archive-toggle").waitFor({ state: "detached" });
  section("deleted");
  await sidebar(page);

  section("errors");
  console.log(errors.length ? errors.join("\n") : "(none)");
} catch (e) {
  console.log(`FAILED: ${e.stack ?? e}`);
  await screenshot(page, "failed");
  process.exitCode = 1;
} finally {
  await end(() => browser.close(), () => server.kill());
}
