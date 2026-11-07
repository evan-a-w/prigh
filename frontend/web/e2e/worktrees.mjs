// Sessions and the worktrees that matter to them, against real git
// repositories: a flat list of sessions, each with its repository and
// branch, its worktrees under it (one made for a subagent, one made from the
// session's menu, one moved into that has uncommitted changes, one shared
// with another session), their context menus (right-click, Shift+F10, a
// long press on a phone) and what they do (a new worktree, a new session
// there, a terminal there, switching back where the session came from,
// removal: refused with changes, then forced),
// folding, searching, /worktree's picker, the list on a phone, and deleting
// the session we are in (on a desktop and on phones). Run by
// prigh_web.sh; prints normalised snapshots (the expected output is in
// ENGINE.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { execFileSync, spawn } from "node:child_process";
import { mkdirSync, realpathSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: worktrees.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
mkdirSync(join(process.env.TEST_DIR, "worktrees"), { recursive: true });
const dir = realpathSync(join(process.env.TEST_DIR, "worktrees"));
const home = join(dir, "home");
const repo = join(dir, "prigh");
const docs = join(dir, "prigh-docs");
const web = join(dir, "site");
const notes = join(dir, "notes");
for (const d of [home, repo, web, notes]) mkdirSync(d, { recursive: true });
// The first prompt asks for a subagent in a worktree of its own; everything
// else is answered "noted" (the main agent and the subagent share the script,
// in whichever order they ask).
writeFileSync(join(dir, "script.json"), JSON.stringify([
  {
    text: "handing it over",
    tool_calls: [{ id: "s1", name: "subagent", arguments: { task: "Tidy the docs", worktree: "auto" } }],
  },
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
git(repo, "worktree", "add", "-q", "-b", "docs", docs);
writeFileSync(join(docs, "README.md"), "# Repo\n\nDocs.\n");
writeFileSync(join(docs, "guide.md"), "# Guide\n");

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
    "-token", token, "-cwd", repo],
  // The terminal's tmux server is ours, so its shell is $SHELL.
  { env: { ...process.env, HOME: home, TMUX_TMPDIR: dir, SHELL: "/bin/sh" },
    stdio: ["ignore", "ignore", "pipe"] },
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
const page = await (await browser.newContext({ viewport: { width: 1280, height: 800 }, deviceScaleFactor: 2 })).newPage();
// The calls and their answers, shown when a step fails.
const frames = [];
page.on("websocket", ws => {
  ws.on("framesent", f => frames.push(`sent ${f.payload}`));
  ws.on("framereceived", f => frames.push(`received ${f.payload}`));
});
const errors = [];
page.on("console", message => {
  if (message.type() === "error" && !/favicon\.ico|interactive-widget/.test(message.text())) errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

const clean = text => text
  .split(dir).join("<DIR>")
  .replace(/\b(just now|\d+[smhd] ago)\b/g, "<age>")
  .split("\n")
  .map(line => line.trim())
  .filter(line => line !== "")
  .join("\n");
const section = label => console.log(`=== ${engineName}: worktrees: ${label} ===`);
let shot = 0;
const screenshot = async (label, target = page) => {
  if (!process.env.SHOTS) return;
  shot++;
  await target.screenshot({
    path: join(process.env.SHOTS, `${engineName}-worktrees-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const show = async (label, selector) => {
  section(label);
  console.log(clean(await page.locator(selector).first().innerText()));
  await page.waitForTimeout(200); // its fade in
  await screenshot(label);
};
// The list, a line per row: its text (worktrees indented under their
// session) and its state.
const tree = async (label, target = page) => {
  section(label);
  const rows = await target.locator(".sessions").evaluate(list =>
    [...list.querySelectorAll(".session, .worktree")].map(row => {
      const worktree = row.classList.contains("worktree");
      const text = row.innerText.split("\n").map(s => s.trim()).filter(s => s).join(" | ");
      const marks = [...row.classList].filter(c =>
        ["here", "selected", "dirty", "agent", "running", "gone"].includes(c));
      if (!worktree && row.parentElement.querySelector(".session-fold.folded")) marks.push("folded");
      return `${worktree ? "│ " : ""}${text}${marks.length ? ` [${marks.sort().join(" ")}]` : ""}`;
    }));
  console.log(clean(rows.join("\n")));
  await target.waitForTimeout(200);
  await screenshot(label, target);
};
// The open context menu: its heading and items.
const menuText = async (label, target = page) => {
  section(label);
  const menu = target.locator(".context-menu.placed");
  await menu.waitFor();
  console.log(clean(await menu.evaluate(m =>
    [m.querySelector(".context-menu-head").innerText.replace(/\n/g, " — "),
      ...[...m.querySelectorAll(".menu-item")].map(i =>
        `${i.classList.contains("selected") ? "▸ " : "  "}${i.innerText.split("\n").join("  ")}`)].join("\n"))));
  await target.waitForTimeout(150); // its fade in
  await screenshot(label, target);
};
const toastHas = text => page.locator(".toast", { hasText: text }).first().waitFor();
const editor = () => page.locator(".composer textarea");
const idle = () => page.waitForFunction(() => !document.querySelector(".streaming, .btn.stop"));
const send = async text => {
  await editor().fill(text);
  await editor().press("Enter");
  await page.locator(".chat", { hasText: text }).waitFor();
  await idle();
};
// Ctrl+Enter: as typed, whatever the completion popup offers.
const run = async command => {
  await editor().fill(command);
  await editor().press("Control+Enter");
};
const session = title => page.locator(".session-item", { has: page.locator(".session-title", { hasText: title }) });
const worktree = (branch, within = page) =>
  within.locator(".worktree", { has: page.locator(".worktree-branch", { hasText: new RegExp(`^${branch}$`) }) });
const choose = async item => {
  await page.locator(`.context-menu.placed [data-item=${item}]`).click();
  await page.waitForSelector(".context-menu", { state: "detached" });
};
const newSession = async cwd => {
  await page.locator(".new-session").click();
  await page.waitForFunction(() => !document.querySelector(".new-session.opening"));
  await run(`/cd ${cwd}`);
  await page.waitForFunction(d => document.body.innerText.includes(d), cwd.split("/").pop());
};
const closeMenu = async () => {
  await page.keyboard.press("Escape");
  await page.waitForSelector(".context-menu", { state: "detached" });
};

try {
  await page.goto(url);
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await editor().waitFor();

  // A subagent asked to work in a worktree of its own.
  await send("Tidy the docs in a worktree");
  await worktree("agent/tidy-the-docs").waitFor();
  await page.locator(".chat", { hasText: "agent/tidy-the-docs" }).first().waitFor();
  const first = session("Tidy the docs");

  // The session's menu, by right-click: New worktree… moves it there.
  await first.locator(".session").click({ button: "right", position: { x: 60, y: 12 } });
  await menuText("a session's menu");
  await choose("new_worktree");
  await page.locator(".worktree-dialog").waitFor();
  await page.waitForTimeout(300); // its fade in
  await show("the new worktree dialog", ".worktree-dialog");
  await page.locator("#worktree-branch").fill("feat/login");
  await page.keyboard.press("Enter");
  await page.waitForSelector(".worktree-dialog", { state: "detached" });
  await page.waitForFunction(() =>
    document.querySelector(".session-item.selected .worktree.here")?.innerText.includes("feat/login"));
  // Into another worktree of the repository, which has changes.
  await run("/worktree open docs");
  await page.waitForFunction(() =>
    document.querySelector(".session-item.selected .worktree.here")?.innerText.includes("docs"));

  // Another repository, and a plain folder.
  await newSession(web);
  await send("A landing page");
  await newSession(notes);
  await send("Shopping list");

  // From a worktree's menu: a new session there, which shares it.
  await worktree("feat/login", first).click();
  await menuText("a worktree's menu");
  await choose("new_session_here");
  await toastHas("New session in");
  await send("The login form");
  await worktree("feat/login", session("The login form")).waitFor();

  // Back to the first session.
  await first.locator(".session-open").click();
  await page.locator(".session.selected", { hasText: "Tidy the docs" }).waitFor();
  await page.waitForFunction(() => document.querySelector(".worktree-changes"));
  await page.mouse.move(1000, 400);
  await tree("sessions and their worktrees");
  // How it looks, the menus open, in the dark and light themes
  // (screenshots only).
  const theme = () => page.evaluate(() => document.documentElement.dataset.theme);
  const started = await theme();
  for (const look of ["dark", "light"]) {
    await run(`/theme ${look}`);
    await page.waitForFunction(t => document.documentElement.dataset.theme === t, look);
    await first.locator(".session").hover();
    await page.waitForTimeout(200);
    await screenshot(`sessions, ${look}, a session pointed at`);
    await first.locator(".session").click({ button: "right", position: { x: 80, y: 14 } });
    await page.locator(".context-menu.placed").waitFor();
    await page.waitForTimeout(200);
    await screenshot(`sessions, ${look}, a session's menu`);
    await closeMenu();
    await worktree("agent/tidy-the-docs", first).click({ button: "right" });
    await page.locator(".context-menu.placed").waitFor();
    await page.waitForTimeout(200);
    await screenshot(`sessions, ${look}, a subagent's worktree's menu`);
    await closeMenu();
  }
  await run(`/theme ${started}`);
  await page.waitForFunction(t => document.documentElement.dataset.theme === t, started);
  await page.mouse.move(1000, 400);

  // The keyboard: Shift+F10 on a row opens its menu, ↓ moves, Esc closes it
  // and changes nothing.
  await session("A landing page").locator(".session-open").focus();
  await page.keyboard.press("Shift+F10");
  await page.locator(".context-menu.placed").waitFor();
  await page.keyboard.press("ArrowDown");
  await page.locator(".context-menu .menu-item").nth(1).and(page.locator(".selected")).waitFor();
  await menuText("from the keyboard");
  await closeMenu();
  section("Esc changed nothing");
  console.log(await page.locator(".session.selected .session-title").textContent());

  // A terminal in a worktree: a shell of its own there.
  await worktree("feat/login", first).click({ button: "right" });
  await choose("terminal");
  // Connected, the shell drawn: closing earlier would cut the connection.
  const shellUp = () => page.waitForFunction(() =>
    !/connecting/.test(document.querySelector(".terminal-state")?.textContent ?? "")
    && document.querySelector(".terminal-panel .xterm-rows")?.textContent.trim());
  await page.locator(".terminal-panel .terminal-cwd", { hasText: "feat-login" }).waitFor();
  await shellUp();
  section("a terminal in the worktree");
  console.log(clean(await page.locator(".terminal-head .terminal-cwd").innerText()));
  await screenshot("a terminal in the worktree");
  await run("/terminal");
  await page.locator(".terminal-panel .terminal-cwd", { hasText: "docs" }).waitFor();
  await shellUp();
  await page.keyboard.press("Control+`");
  await page.locator(".terminal-panel").waitFor({ state: "detached" });

  // Folding a session's worktrees: their count stays on its row.
  await first.locator(".session-fold").click();
  await first.locator(".session-fold.folded").waitFor();
  await page.mouse.move(1000, 400);
  await tree("folded");
  await first.locator(".session-fold").click();

  // Search finds sessions by their worktrees' branches.
  await page.locator("#session-search").fill("tidy-the-docs");
  await page.locator(".session", { hasText: "Shopping list" }).waitFor({ state: "detached" });
  section("searching");
  console.log((await page.locator(".sessions .session-title").allTextContents()).join("\n"));
  await screenshot("searching");
  await page.locator("#session-search").fill("");
  await session("Shopping list").waitFor();

  // Switched into feat/login, then docs: its home is still main, and
  // Switch back, first in its menu, goes there.
  await first.locator(".session").click({ button: "right", position: { x: 60, y: 12 } });
  await menuText("a switched session's menu");
  await choose("switch_back");
  await toastHas("Back in main");
  await page.waitForFunction(() => !document.querySelector(".session-item.selected .worktree.here"));
  await first.locator(".session-home").waitFor({ state: "detached" });
  await page.mouse.move(1000, 400);
  await tree("switched back");

  // Removing the one with changes, from its menu: refused, then forced
  // (once the session has moved out of it).
  await worktree("docs", first).click({ button: "right" });
  await choose("remove");
  await page.locator(".remove-worktree-dialog").waitFor();
  await page.waitForTimeout(300); // its fade in
  await show("remove: asked first", ".remove-worktree-dialog");
  await page.locator(".remove-worktree-dialog").getByRole("button", { name: "Remove", exact: true }).click();
  await page.locator(".remove-worktree-dialog .dialog-error").waitFor();
  await show("remove: refused with changes", ".remove-worktree-dialog");
  await page.getByRole("button", { name: "Discard changes and remove" }).click();
  await toastHas("Removed the worktree docs");
  await worktree("docs").waitFor({ state: "detached" });
  // The subagent's, with Delete on its row: its session forgets it.
  await worktree("agent/tidy-the-docs", first).focus();
  await page.keyboard.press("Delete");
  await page.locator(".remove-worktree-dialog").waitFor();
  await page.keyboard.press("Enter");
  await toastHas("Removed the worktree agent/tidy-the-docs");
  await worktree("agent/tidy-the-docs").waitFor({ state: "detached" });
  await page.mouse.move(1000, 400);
  await tree("removed");

  // /worktree: a picker of all the repository's worktrees.
  await run("/worktree");
  await page.locator(".picker-dialog").waitFor();
  await page.waitForTimeout(300); // its fade in
  await show("the /worktree picker", ".picker-dialog");
  await page.keyboard.press("Escape");
  await page.waitForSelector(".picker-dialog", { state: "detached" });

  // Deleting the session we are in, from its menu: the confirmation says
  // where we go (the next one down), and we land there as it goes.
  await first.locator(".session").click({ button: "right", position: { x: 60, y: 12 } });
  await choose("delete");
  await page.locator(".delete-dialog").waitFor();
  await page.waitForTimeout(300); // its fade in
  section("deleting the session we are in: asked");
  console.log(clean(await page.locator(".delete-dialog").innerText()).replace(/\d+(\.\d+)? (B|KB|MB)\b/g, "<SIZE>"));
  await screenshot("deleting the session we are in: asked");
  await page.locator(".delete-dialog .btn.danger").click();
  await toastHas("Deleted 1 session");
  await first.waitFor({ state: "detached" });
  await page.waitForFunction(() =>
    document.querySelector(".session-item.selected") && !document.querySelector(".session.opening"));
  section("deleting the session we are in: where we land");
  console.log(clean(await page.evaluate(() => {
    const id = document.querySelector(".session-item.selected [data-session]")?.dataset.session;
    return `in: ${document.querySelector(".session-item.selected .session-title")?.innerText}; ` +
      `the address names it: ${new URL(location.href).searchParams.get("session") === id}`;
  })));
  await page.mouse.move(1000, 400);
  await tree("the session we were in deleted");

  // On a phone: the sidebar over the chat; a long press opens a row's menu,
  // a sheet at the foot of the screen.
  const phone = await (await browser.newContext({
    viewport: { width: 390, height: 844 },
    isMobile: engineName !== "firefox",
    hasTouch: true,
  })).newPage();
  phone.on("pageerror", error => errors.push(`phone: ${error.message}`));
  await phone.goto(page.url());
  await phone.locator("form input[type=password]").fill(token);
  await phone.locator("form button[type=submit]").click();
  await phone.locator(".composer textarea").waitFor();
  await phone.locator(".chat-chrome .toggle-sidebar").click();
  await phone.locator(".sidebar .worktree").first().waitFor();
  await phone.waitForTimeout(400); // the sidebar slides in
  await tree("on a phone", phone);
  const row = phone.locator(".sidebar .worktree").first();
  const box = await row.boundingBox();
  await row.dispatchEvent("pointerdown", {
    pointerType: "touch", isPrimary: true, clientX: box.x + 30, clientY: box.y + 10, bubbles: true,
  });
  await phone.waitForTimeout(700);
  await row.dispatchEvent("pointerup", { pointerType: "touch", bubbles: true });
  await menuText("a long press on a phone", phone);
  await phone.context().close();

  // Deleting the session we are in on a phone, at 390x844 and 320x640 (and
  // in Chromium as an installed PWA on a notched phone): a long press on
  // its row, Delete… in the sheet, the dialog, and where we land. Another
  // session first, at the top, so that the last one deleted has one to go
  // to (up: none is below it).
  const phoneStart = page.url();
  await newSession(notes);
  await send("A todo list");
  // What is wrong on the screen: sideways overflow, and controls of the
  // dialog (or the sheet) off screen, in the notch's or home indicator's
  // inset, covered, or smaller than a thumb.
  const audit = (target, scope) => target.evaluate(scope => {
    const out = [];
    if (document.scrollingElement.scrollWidth > innerWidth + 1)
      out.push(`overflows sideways: ${document.scrollingElement.scrollWidth} > ${innerWidth}`);
    const probe = document.createElement("div");
    probe.style.cssText = "position:fixed;padding:env(safe-area-inset-top) 0 env(safe-area-inset-bottom)";
    document.body.append(probe);
    const inset = { top: parseFloat(getComputedStyle(probe).paddingTop), bottom: parseFloat(getComputedStyle(probe).paddingBottom) };
    probe.remove();
    for (const e of document.querySelectorAll(`:is(${scope}) :is(button, [role=menuitem], .menu-item), ${scope}`)) {
      if (!e.matches("button, [role=menuitem], .menu-item")) continue;
      const r = e.getBoundingClientRect();
      if (r.width < 1) continue;
      const name = (e.getAttribute("aria-label") || e.innerText).trim().split("\n")[0];
      // Past the edge of a scroller (or into the padding a sheet scrolls
      // under, over the home indicator) is scrolled to, not lost.
      let scroller = e.parentElement;
      while (scroller && !/(auto|scroll)/.test(getComputedStyle(scroller).overflowY)) scroller = scroller.parentElement;
      const box = scroller?.getBoundingClientRect();
      const padding = scroller && scroller.scrollHeight > scroller.clientHeight
        ? parseFloat(getComputedStyle(scroller).paddingBottom) : 0;
      if (box && (r.top < box.top - 0.5 || r.bottom > box.bottom - padding + 0.5)) continue;
      if (r.left < -0.5 || r.right > innerWidth + 0.5 || r.top < inset.top - 0.5 || r.bottom > innerHeight - inset.bottom + 0.5)
        out.push(`${name}: out of the safe area`);
      const hit = document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2);
      if (!hit || !(e.contains(hit) || hit.contains(e))) out.push(`${name}: covered`);
      if (r.height < (e.closest(".composer, .chat-chrome") ? 36 : 44) - 0.5) out.push(`${name}: ${Math.round(r.height)}px high`);
    }
    return out.length ? out.join("\n") : "ok";
  }, scope);
  const settled = target => target.waitForFunction(() =>
    document.getAnimations().every(a => a.playState !== "running" || a.effect?.getComputedTiming().iterations === Infinity));
  const deleteOnPhone = async (from, w, h, { pwa = false } = {}) => {
    const tag = `${w}x${h}${pwa ? " PWA" : ""}`;
    const context = await browser.newContext({
      viewport: { width: w, height: h }, isMobile: engineName !== "firefox", hasTouch: true,
    });
    const p = await context.newPage();
    p.on("pageerror", error => errors.push(`phone ${tag}: ${error.message}`));
    if (pwa) {
      const cdp = await context.newCDPSession(p);
      await cdp.send("Emulation.setSafeAreaInsetsOverride", { insets: { top: 47, bottom: 34, left: 0, right: 0 } });
    }
    await p.goto(from);
    await p.locator("form input[type=password]").fill(token);
    await p.locator("form button[type=submit]").tap();
    await p.locator(".composer textarea").waitFor();
    await p.locator(".chat-chrome .toggle-sidebar").tap();
    const row = p.locator(".sidebar .session-item.selected [data-session]").first();
    await row.waitFor();
    await settled(p);
    const title = await p.locator(".sidebar .session-item.selected .session-title").innerText();
    const box = await row.boundingBox();
    await row.dispatchEvent("pointerdown", {
      pointerType: "touch", isPrimary: true, clientX: box.x + 30, clientY: box.y + 10, bubbles: true,
    });
    await p.waitForTimeout(700);
    await row.dispatchEvent("pointerup", { pointerType: "touch", bubbles: true });
    await p.locator(".context-menu.placed [data-item=delete]").waitFor();
    await settled(p);
    section(`deleting the session we are in on a phone, ${tag}: its sheet`);
    console.log(await p.evaluate(() => {
      const r = document.querySelector(".context-menu.placed").getBoundingClientRect();
      return `full width: ${Math.round(r.width) === innerWidth}; at the bottom: ${Math.abs(r.bottom - innerHeight) < 1}`;
    }));
    console.log(await audit(p, ".context-menu.placed"));
    await p.locator(".context-menu.placed [data-item=delete]").scrollIntoViewIfNeeded();
    console.log(`Delete…, scrolled to: ${await audit(p, ".context-menu.placed [data-item=delete]")}`);
    await screenshot(`phone ${tag} deleting the session we are in: its sheet`, p);
    await p.locator(".context-menu.placed [data-item=delete]").tap();
    await p.locator(".delete-dialog").waitFor();
    await settled(p);
    section(`deleting the session we are in on a phone, ${tag}: asked`);
    console.log(`deleting: ${title}`);
    console.log(clean(await p.locator(".delete-dialog").innerText()).replace(/\d+(\.\d+)? (B|KB|MB)\b/g, "<SIZE>"));
    // Nothing to type: no keyboard comes up over it.
    console.log(`focused: ${await p.evaluate(() => document.activeElement.matches("input, textarea") ? "a text field" : "no text field")}`);
    console.log(await audit(p, ".delete-dialog"));
    await screenshot(`phone ${tag} deleting the session we are in: asked`, p);
    await p.locator(".delete-dialog .btn.danger").tap();
    await p.locator(".toast", { hasText: "Deleted 1 session" }).first().waitFor();
    await p.locator(".delete-dialog").waitFor({ state: "detached" });
    await p.waitForFunction(t =>
      [...document.querySelectorAll(".session-title")].every(e => e.innerText !== t)
      && document.querySelector(".session-item.selected") && !document.querySelector(".session.opening"), title);
    await settled(p);
    section(`deleting the session we are in on a phone, ${tag}: where we land`);
    console.log(clean(await p.evaluate(() => {
      const id = document.querySelector(".session-item.selected [data-session]")?.dataset.session;
      return `in: ${document.querySelector(".session-item.selected .session-title")?.innerText}; ` +
        `the strip says: ${document.querySelector(".chrome-title")?.innerText ?? "(hidden)"}; ` +
        `the address names it: ${new URL(location.href).searchParams.get("session") === id}`;
    })));
    console.log(await audit(p, ".chat-chrome, .composer"));
    await screenshot(`phone ${tag} deleting the session we are in: landed`, p);
    const landed = p.url();
    await context.close();
    return landed;
  };
  let at = await deleteOnPhone(phoneStart, 390, 844);
  at = await deleteOnPhone(at, 320, 640);
  if (engineName === "chromium") await deleteOnPhone(at, 390, 844, { pwa: true });

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  await screenshot("failure");
  console.error(frames.filter(f => /worktree|sessions/.test(f)).map(f => f.slice(0, 600)).join("\n"));
  console.error(errors.join("\n"));
  throw error;
} finally {
  await end(() => server.kill(), () => browser.close());
}
