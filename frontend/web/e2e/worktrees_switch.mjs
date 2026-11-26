// A session's worktrees are in the session card as soon as the user is back
// in it from another session (in another repository), with no frame where
// they are gone: subagents of a pinned session make worktrees, the user goes
// to a session in another repository and back, more are made, and away and
// back again; on a desktop and on a 390x844 phone (the card a sheet there). Run by
// prigh_web.sh; prints normalised snapshots (the expected output is in
// ENGINE.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { execFileSync, spawn } from "node:child_process";
import { mkdirSync, realpathSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { animationsOver } from "./animations.mjs";
import { end } from "./end.mjs";
import { normalVerbosity } from "./verbose_prefs.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: worktrees_switch.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
mkdirSync(join(process.env.TEST_DIR, "worktrees_switch"), { recursive: true });
const dir = realpathSync(join(process.env.TEST_DIR, "worktrees_switch"));
const home = join(dir, "home");
normalVerbosity(home);
const repo = join(dir, "prigh");
const web = join(dir, "site");
for (const d of [home, repo, web]) mkdirSync(d, { recursive: true });
// The work session's prompts ask for subagents in worktrees of their own:
// the first for one, the second for two more. Everything else is answered
// "noted".
const subagents = (id, tasks) => ({
  text: "handing it over",
  tool_calls: tasks.map((task, i) => ({
    id: `${id}${i}`, name: "subagent", arguments: { task, worktree: "auto" },
  })),
});
writeFileSync(join(dir, "script.json"), JSON.stringify([
  subagents("a", ["Tidy the docs"]),
  // The subagent, its caller after it and again on its report; the other
  // session's prompt.
  ...Array(4).fill({ text: "noted" }),
  subagents("b", ["More docs", "More tests"]),
  ...Array(30).fill({ text: "noted" }),
]));

const git = (cwd, ...args) =>
  execFileSync("git", ["-c", "user.name=e2e", "-c", "user.email=e2e@example.com", ...args], { cwd, stdio: "ignore" });
for (const [d, branch] of [[repo, "main"], [web, "trunk"]]) {
  writeFileSync(join(d, "README.md"), "# Repo\n");
  git(d, "init", "-q", "-b", branch);
  git(d, "add", "README.md");
  git(d, "commit", "-q", "-m", "start");
}

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
    "-token", token, "-cwd", repo],
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

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
const errors = [];
const frames = [];
// Each page's list_worktrees calls not answered yet, by id, and what waits
// for none to be.
const listing = new Map();
const parse = payload => { try { return JSON.parse(payload); } catch { return {}; } };
const watch = (page, tag) => {
  const calls = { pending: new Set(), waiting: [] };
  listing.set(page, calls);
  page.on("websocket", ws => {
    ws.on("framesent", f => {
      frames.push(`${tag} sent ${f.payload}`);
      const call = parse(f.payload);
      if (call.method === "list_worktrees") calls.pending.add(call.id);
    });
    ws.on("framereceived", f => {
      frames.push(`${tag} received ${f.payload}`);
      const reply = parse(f.payload);
      if (calls.pending.delete(reply.id) && calls.pending.size === 0) calls.waiting.splice(0).forEach(go => go());
    });
  });
  page.on("console", message => {
    if (message.type() === "error" && !/favicon\.ico|interactive-widget/.test(message.text())) errors.push(`${tag} console: ${message.text()}`);
  });
  page.on("pageerror", error => errors.push(`${tag} page: ${error.message}`));
};

const clean = text => text
  .split(dir).join("<DIR>")
  .replace(/\b(just now|\d+[smhd] ago)\b/g, "<age>")
  .replace(/agent\/(more|tidy)[\w-]*/g, (m, w) => `agent/${w}…`)
  .split("\n")
  .map(line => line.trim())
  .filter(line => line !== "")
  .join("\n");
const section = label => console.log(`=== ${engineName}: worktrees_switch: ${label} ===`);
let shot = 0;
const screenshot = async (label, target) => {
  if (!process.env.SHOTS) return;
  shot++;
  await target.screenshot({
    path: join(process.env.SHOTS, `${engineName}-worktrees-switch-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
// The sessions, a line each (which is open marked), then the session
// card's worktrees.
const tree = async (label, target) => {
  section(label);
  const rows = await target.evaluate(() => {
    const out = [...document.querySelectorAll(".sessions .session")].map(row =>
      `${row.querySelector(".session-title")?.innerText.trim()}${row.classList.contains("selected") ? " [selected]" : ""}`);
    const rows = [...document.querySelectorAll(".session-card .session-card-worktree")];
    out.push(`the card: ${rows.length ? "" : "no worktrees"}`);
    for (const r of rows)
      out.push(`│ ${r.innerText.split("\n")[0].trim()}${r.getAttribute("aria-current") ? " [current]" : ""}`);
    return out;
  });
  console.log(clean(rows.join("\n")));
  await screenshot(label, target);
};
// The pinned session whose subagents make worktrees, and the one in the
// other repository.
const work = "Tidy the docs in a worktree";
const other = "A landing page";
const titled = (target, title) =>
  target.locator(".session-item", { has: target.locator(".session-title", { hasText: title }) });

// Every frame from now until [stop] in which the session [title] is open
// (its row selected, done opening), the worktrees in the session card: how
// many fewer than [want] there ever were.
const startCounting = (target, title, want) => target.evaluate(([title, want]) => {
  const seen = window.__worktreesSeen = { frames: 0, short: 0, least: Infinity };
  const count = () => {
    const open = document.querySelector(".session.selected .session-title")?.innerText.trim() === title
      && !document.querySelector(".session.opening");
    if (!open) return;
    const n = document.querySelectorAll(".session-card .session-card-worktree").length;
    seen.frames++;
    seen.least = Math.min(seen.least, n);
    if (n < want) seen.short++;
  };
  // After each change to the page as well as each frame: a change undone
  // within a frame is not drawn, but one left until the next is.
  const observer = new MutationObserver(count);
  observer.observe(document.body, { subtree: true, childList: true, attributes: true });
  const frame = () => { count(); if (!seen.stopped) requestAnimationFrame(frame); };
  requestAnimationFrame(frame);
  seen.stop = () => { seen.stopped = true; observer.disconnect(); };
}, [title, want]);
const stopCounting = async target => {
  const seen = await target.evaluate(() => {
    const s = window.__worktreesSeen;
    s.stop();
    return { short: s.short, least: s.least, counted: s.frames > 0 };
  });
  return seen.counted
    ? `counted: true; checks with fewer: ${seen.short}; fewest: ${seen.least}`
    : "counted: false (not in it)";
};

const desktop = await (await browser.newContext({ viewport: { width: 1280, height: 800 }, deviceScaleFactor: 2 })).newPage();
watch(desktop, "desktop");
const editor = p => p.locator(".composer textarea");
const idle = p => p.waitForFunction(() => !document.querySelector(".streaming, .btn.stop"));
const send = async (p, text) => {
  await editor(p).fill(text);
  await editor(p).press("Enter");
  await p.locator(".chat", { hasText: text }).waitFor();
  await idle(p);
};
const run = async (p, command) => {
  await editor(p).fill(command);
  await editor(p).press("Control+Enter");
};
const selected = (p, title) => p.locator(".session.selected", { hasText: title }).waitFor();
const signIn = async p => {
  await p.locator("form input[type=password]").fill(token);
  await p.locator("form button[type=submit]").click();
  await editor(p).waitFor();
};
// The session card's worktrees: [n] of them (with the session they belong
// to open).
const worktreesInCard = (p, n) => p.waitForFunction(n =>
  document.querySelectorAll(".session-card .session-card-worktree").length === n, n);
// The card's worktrees unfolded (they start folded; it stays so after).
const unfold = async p => {
  const fold = p.locator(".session-card [aria-controls=session-card-worktrees-content]");
  await fold.waitFor();
  if (await fold.getAttribute("aria-expanded") !== "true") await fold.click();
};
// A phone's session card: a sheet from ⋯, its worktrees unfolded.
const sheet = async p => {
  await p.locator(".more-toggle").tap();
  await p.locator(".session-card-row").tap();
  await p.locator(".session-card.sheet").waitFor();
  await unfold(p);
  await animationsOver(p);
};
const closeSheet = async p => {
  await p.keyboard.press("Escape");
  await p.waitForSelector(".session-card.sheet", { state: "detached" });
};
// A phone's sidebar, open and slid in.
const drawer = async p => {
  if (!(await p.locator(".sidebar-drawer").count())) await p.locator(".chat-chrome .toggle-sidebar").tap();
  await p.locator(".sidebar-drawer").waitFor();
  await animationsOver(p);
};
// Into the session [title] by its row, counting [watched]'s worktrees in
// the card while it is open, until the list has settled (the listings
// answered). On a phone the card is a sheet, closed while switching: its
// worktrees are looked at once it opens.
const switchTo = async (p, title, watched, want, label, { phone = false } = {}) => {
  if (phone) await drawer(p);
  if (!phone) await startCounting(p, watched, want);
  const row = titled(p, title).locator(".session-open");
  if (phone) await row.tap(); else await row.click();
  await selected(p, title);
  await p.waitForFunction(() => !document.querySelector(".session.opening"));
  // The listings asked for on the way answered, and drawn.
  const calls = listing.get(p);
  if (calls.pending.size) await new Promise(go => calls.waiting.push(go));
  await p.evaluate(() => new Promise(r => requestAnimationFrame(() => requestAnimationFrame(r))));
  if (title === watched) {
    if (phone) await sheet(p);
    await worktreesInCard(p, want);
  }
  if (!phone) {
    section(`${label}: ${watched}'s worktrees while switching`);
    console.log(await stopCounting(p));
  }
  await p.mouse.move(1000, 400).catch(() => {});
  await tree(label, p);
  if (phone && title === watched) await closeSheet(p);
};

try {
  await desktop.goto(url);
  await signIn(desktop);

  // The work session, pinned: a subagent makes a worktree.
  await send(desktop, work);
  await unfold(desktop);
  await worktreesInCard(desktop, 1);
  await run(desktop, "/pin");
  await desktop.locator(".sessions", { hasText: "Pinned" }).waitFor();

  // Another session, in another repository.
  await desktop.locator(".new-session").click();
  await desktop.waitForFunction(() => !document.querySelector(".new-session.opening"));
  await run(desktop, `/cd ${web}`);
  await desktop.waitForFunction(d => document.body.innerText.includes(d), "site");
  await send(desktop, "A landing page");
  // Its row is listed once the backend has the session: wait for it.
  await desktop.locator(".sessions .session.selected", { hasText: "A landing page" }).waitFor();
  await worktreesInCard(desktop, 0);
  await desktop.mouse.move(1000, 400);
  await tree("in the other session", desktop);

  // A phone, also first in the other session, then in the work session:
  // both have the work repository's listing from before the next worktrees.
  const phone = await (await browser.newContext({
    viewport: { width: 390, height: 844 }, isMobile: engineName !== "firefox", hasTouch: true,
  })).newPage();
  watch(phone, "phone");
  await phone.goto(desktop.url());
  await signIn(phone);
  await drawer(phone);
  await tree("on a phone, in the other session", phone);
  await switchTo(phone, work, work, 1, "on a phone: into the work session", { phone: true });
  await switchTo(desktop, work, work, 1, "back in the work session");

  // Two more subagents, two more worktrees, while both are in it.
  await send(desktop, "More subagents");
  await worktreesInCard(desktop, 3);
  await tree("more worktrees", desktop);
  await sheet(phone);
  await worktreesInCard(phone, 3);
  await tree("on a phone: more worktrees", phone);
  await closeSheet(phone);

  // Away and back, and away again: they stay throughout.
  await switchTo(desktop, other, work, 3, "away again");
  await switchTo(desktop, work, work, 3, "and back");
  await switchTo(desktop, other, work, 3, "and away");
  await switchTo(phone, other, work, 3, "on a phone: away", { phone: true });
  await switchTo(phone, work, work, 3, "on a phone: and back", { phone: true });
  await switchTo(phone, other, work, 3, "on a phone: and away", { phone: true });

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  await screenshot("failure", desktop);
  console.error(frames.filter(f => /worktree|sessions/.test(f)).map(f => f.slice(0, 600)).join("\n"));
  console.error(errors.join("\n"));
  throw error;
} finally {
  await end(() => server.kill(), () => browser.close());
}
