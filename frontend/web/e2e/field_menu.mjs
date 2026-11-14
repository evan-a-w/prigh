// The desktop app's browser window (#shell=browser): the browser's context
// menu never shows, a text field's right-click opens prigh's Cut, Copy,
// Paste, Select all menu (over a dialog too), a session's menu is still
// prigh's; on a page opened plainly the browser's menu is left alone. In the
// terminal a right-click (or Shift+F10) opens Copy, Paste, Select all, which
// keep xterm.js's selection, and Ctrl+Shift+C and V copy and paste.
// Chromium is granted the clipboard (as the desktop app grants its window);
// Firefox and WebKit, which would ask the user before a page reads it, get a
// stand-in clipboard for the same steps.
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
if (!engine) throw new Error("usage: field_menu.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "field_menu");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });
setTimeout(() => { console.error("field_menu.mjs: timed out"); process.exit(2); }, 240000).unref();

writeFileSync(join(dir, "script.json"), JSON.stringify([{ text: "Noted." }]));

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
    "-token", token, "-cwd", cwd],
  // The terminal's tmux server is ours; its shell is $SHELL (see
  // terminal_keys.mjs).
  { env: { ...process.env, HOME: home, TMUX_TMPDIR: dir, SHELL: process.env.PRIGH_E2E_SHELL || "/bin/sh" },
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
const context = await browser.newContext({ viewport: { width: 1280, height: 800 } });
context.setDefaultTimeout(20000);
if (engineName === "chromium")
  await context.grantPermissions(["clipboard-read", "clipboard-write"], { origin: new URL(url).origin });
else
  await context.addInitScript(() => {
    let text = "";
    const clipboard = {
      readText: async () => text,
      writeText: async t => { text = t; },
    };
    Object.defineProperty(Navigator.prototype, "clipboard", { get: () => clipboard, configurable: true });
  });
// Whether the browser would show its own menu: a listener after all of the
// page's sees whether one of them cancelled it.
await context.addInitScript(() => {
  window.contextMenus = [];
  window.addEventListener("contextmenu", ev => {
    setTimeout(() => window.contextMenus.push(ev.defaultPrevented ? "cancelled" : "browser's"));
  }, true);
});
const page = await context.newPage();
page.on("console", message => {
  if (message.type() === "error" && !/favicon\.ico|interactive-widget/.test(message.text()))
    errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

const section = label => console.log(`=== ${engineName}: field_menu: ${label} ===`);
let shot = 0;
// The menu's appearing (a slide and a fade), over.
const settled = () => page.waitForFunction(() => document.getAnimations().every(a =>
  a.playState !== "running" || a.effect?.getTiming().iterations === Infinity));
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await settled();
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-field_menu-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};

const composer = () => page.locator(".composer textarea");
const menu = () => page.locator(".field-menu.placed");
const lastMenu = async () => {
  await page.waitForFunction(n => window.contextMenus.length > n, menusSeen);
  menusSeen = await page.evaluate(() => window.contextMenus.length);
  return page.evaluate(() => window.contextMenus.at(-1));
};
let menusSeen = 0;
const items = () => menu().locator("[role=menuitem]").evaluateAll(els => els.map(e =>
  `${e.classList.contains("selected") ? "> " : "  "}${e.querySelector(".menu-label").innerText}`
  + `${e.getAttribute("aria-disabled") ? " (disabled)" : ""}${e.querySelector(".menu-command") ? `  ${e.querySelector(".menu-command").innerText}` : ""}`).join("\n"));
const field = () => page.evaluate(() => {
  const el = document.activeElement;
  const where = el?.closest(".composer") ? "composer" : el?.closest(".modal") ? "dialog's field" : el?.tagName.toLowerCase();
  return `focus: ${where}; ${JSON.stringify(el.value)} selected ${el.selectionStart}-${el.selectionEnd}`;
});
const clipboard = () => page.evaluate(() => navigator.clipboard.readText()).then(JSON.stringify);
// At the end of the composer's line: past its text, so the caret (which a
// right-click may move there) stays at the end.
const past = { x: 600, y: 10 };
const rightClick = async (locator, position = { x: 30, y: 10 }) => {
  await locator.click({ button: "right", position });
  await menu().waitFor();
};
const gone = () => page.waitForFunction(() => !document.querySelector(".field-menu"));
// Where the menu is: its corner at the pointer (to a few pixels: the
// pointer is at [position] in the field).
const placed = async (locator, position = { x: 30, y: 10 }) => {
  await settled();
  const box = await menu().boundingBox();
  const at = await locator.boundingBox();
  const near = (a, b) => Math.abs(a - b) <= 4;
  return near(box.x, at.x + position.x) && near(box.y, at.y + position.y)
    ? "menu at the pointer"
    : `menu at ${box.x},${box.y}, the pointer at ${at.x + position.x},${at.y + position.y}`;
};

const signIn = async hash => {
  await page.goto(url + hash);
  await page.waitForSelector("form input[type=password], .composer textarea");
  if (await page.locator("form input[type=password]").count()) {
    await page.locator("form input[type=password]").fill(token);
    await page.locator("form button[type=submit]").click();
  }
  await composer().waitFor();
};

try {
  // Opened plainly: the browser's menu, none of prigh's.
  section("a plain page");
  await signIn("");
  await composer().click({ button: "right" });
  console.log(`composer: ${await lastMenu()}; field menus: ${await page.locator(".field-menu").count()}`);
  await page.keyboard.press("Escape");

  section("the browser shell");
  await signIn("#shell=browser");
  menusSeen = await page.evaluate(() => window.contextMenus.length);
  await composer().fill("hello brave world");
  // Under the pointer, which a right-click outside the selection would
  // move the caret to.
  await composer().evaluate(ta => ta.setSelectionRange(0, 11));
  await rightClick(composer());
  console.log(`composer: ${await lastMenu()}; ${await placed(composer())}`);
  console.log(await items());
  await screenshot("the composer's menu");
  // Its own menu: nowhere else in the page shows the browser's.
  await page.keyboard.press("Escape");
  await gone();
  await page.locator("#chat").click({ button: "right", position: { x: 300, y: 200 } });
  console.log(`the transcript: ${await lastMenu()}; menus: ${await page.locator(".field-menu, .context-menu").count()}`);

  section("copy");
  await composer().evaluate(ta => { ta.focus(); ta.setSelectionRange(0, 11); });
  await rightClick(composer());
  await menu().locator("[data-item=copy]").click();
  await gone();
  console.log(`clipboard: ${await clipboard()}`);
  console.log(await field());

  section("cut");
  await page.evaluate(() => navigator.clipboard.writeText("old"));
  await rightClick(composer());
  await menu().locator("[data-item=cut]").click();
  await page.waitForFunction(() => document.querySelector(".composer textarea").value === " world");
  console.log(`clipboard: ${await clipboard()}`);
  console.log(await field());

  section("nothing selected");
  await composer().press("End");
  await rightClick(composer(), past);
  console.log(await items());
  // Disabled: a click does nothing, the menu stays.
  await menu().locator("[data-item=cut]").click({ force: true });
  console.log(`menus: ${await page.locator(".field-menu").count()}`);
  await screenshot("nothing selected");

  section("paste");
  await page.evaluate(() => navigator.clipboard.writeText(" again"));
  await menu().locator("[data-item=paste]").click();
  await page.waitForFunction(() => document.querySelector(".composer textarea").value === " world again");
  console.log(await field());

  section("select all");
  await rightClick(composer(), past);
  await menu().locator("[data-item=select_all]").click();
  await gone();
  console.log(await field());

  section("the keyboard");
  await composer().press("End");
  await rightClick(composer(), past);
  await page.keyboard.press("ArrowDown");
  await menu().locator(".selected[data-item=select_all]").waitFor();
  console.log(await items());
  // Esc: closed, nothing done, the field as it was.
  await page.keyboard.press("Escape");
  await gone();
  console.log(await field());
  await rightClick(composer(), past);
  await page.keyboard.press("ArrowUp");
  await page.keyboard.press("Enter");
  await gone();
  console.log(await field());
  // A click outside closes it the same way.
  await rightClick(composer());
  await page.mouse.click(640, 100);
  await gone();
  console.log(await field());

  // What was cut and pasted is what is sent: the composer's state followed.
  section("sent");
  await composer().press("End");
  await composer().press("Enter");
  const sent = page.locator("#chat .msg.user").first();
  await sent.waitFor();
  console.log(`message: ${JSON.stringify(await sent.locator(".bubble").innerText())}`);
  await page.locator("#chat .msg.assistant", { hasText: "Noted." }).waitFor();

  // A session's menu is prigh's as before; its Rename dialog's field has the
  // field menu over the dialog, whose Esc then closes only the menu.
  section("a session's menu and a dialog");
  const row = page.locator("[data-session]").first();
  await row.click({ button: "right" });
  await page.locator(".context-menu.placed [data-item=rename]").waitFor();
  console.log(`session: ${await lastMenu()}; field menus: ${await page.locator(".field-menu").count()}`);
  await page.locator(".context-menu.placed [data-item=rename]").click();
  const input = page.locator(".modal input").first();
  await input.waitFor();
  await input.fill("renamed");
  await input.evaluate(el => el.select());
  await rightClick(input, { x: 20, y: 10 });
  console.log(await items());
  await screenshot("over the rename dialog");
  await page.keyboard.press("Escape");
  await gone();
  console.log(`dialog: ${await page.locator(".modal").count()}; ${await field()}`);
  await page.keyboard.press("Escape");
  await page.waitForFunction(() => !document.querySelector(".modal"));

  section("the terminal");
  const rows = () => page.evaluate(() =>
    [...document.querySelectorAll(".terminal-panel .xterm-rows > div")].map(r => r.textContent.trimEnd()));
  const shows = text => page.waitForFunction(t =>
    [...document.querySelectorAll(".terminal-panel .xterm-rows > div")].some(r => r.textContent.trimEnd() === t), text);
  const termRow = text => page.locator(".terminal-panel .xterm-rows > div").filter({ hasText: new RegExp(`^${text}\\s*$`) }).first();
  // xterm.js's screen takes the pointer over its rows: the mouse, at a row.
  const at = async (text, dx = 20) => {
    const box = await termRow(text).boundingBox();
    return [box.x + dx, box.y + box.height / 2];
  };
  const dblclickAt = async text => page.mouse.dblclick(...await at(text));
  const rightClickAt = async (x, y) => { await page.mouse.click(x, y, { button: "right" }); await menu().waitFor(); };
  const empty = async () => {
    const box = await page.locator(".terminal-panel .xterm-screen").boundingBox();
    return [box.x + box.width - 40, box.y + box.height - 20];
  };
  const termSelection = () => page.evaluate(() => document.querySelector(".terminal-panel .xterm").prighTerminal.selection());
  await page.locator(".terminal-toggle").click();
  await page.waitForFunction(() =>
    document.querySelector(".terminal-panel .xterm-rows")?.textContent.trim().endsWith("$"), null, { timeout: 60000 });
  await page.waitForFunction(() => document.activeElement?.closest(".terminal-host"));
  await page.keyboard.type("echo copy-me-please");
  await page.keyboard.press("Enter");
  await shows("copy-me-please");
  // A double-click selects the word; a right-click keeps it.
  await dblclickAt("copy-me-please");
  console.log(`selected: ${JSON.stringify(await termSelection())}`);
  await page.evaluate(() => navigator.clipboard.writeText("old"));
  const [x, y] = await at("copy-me-please");
  await rightClickAt(x, y);
  await settled();
  const box = await menu().boundingBox();
  console.log(`terminal: ${await lastMenu()}; ${Math.abs(box.x - x) <= 4 && Math.abs(box.y - y) <= 4 ? "menu at the pointer" : `menu at ${box.x},${box.y}, the pointer at ${x},${y}`}`);
  console.log(await items());
  console.log(`still selected: ${JSON.stringify(await termSelection())}`);
  await screenshot("the terminal's menu");
  await menu().locator("[data-item=copy]").click();
  await gone();
  console.log(`clipboard: ${await clipboard()}`);
  console.log(`the shell has the focus: ${await page.evaluate(() => !!document.activeElement?.closest(".terminal-host"))}`);

  // Esc closes the menu, not the shell's: nothing reaches it.
  await rightClickAt(x, y);
  await page.keyboard.press("Escape");
  await gone();
  // From the keyboard: Shift+F10, at the shell's cursor.
  await page.keyboard.press("Shift+F10");
  await menu().waitFor();
  await settled();
  const cursor = await page.locator(".terminal-panel .xterm-helper-textarea").boundingBox();
  const kbox = await menu().boundingBox();
  console.log(`Shift+F10: ${Math.abs(kbox.x - cursor.x) <= 4 && Math.abs(kbox.y - cursor.y - 24) <= 4 ? "menu under the cursor" : `menu at ${kbox.x},${kbox.y}, the cursor at ${cursor.x},${cursor.y}`}`);
  await page.keyboard.press("Escape");
  await gone();

  section("the terminal: paste");
  await page.evaluate(() => navigator.clipboard.writeText("echo pasted-from-the-menu"));
  await rightClickAt(...await empty());
  console.log(await items());
  await menu().locator("[data-item=paste]").click();
  await gone();
  await page.waitForFunction(() =>
    document.querySelector(".terminal-panel .xterm-rows").textContent.includes("$ echo pasted-from-the-menu"));
  await page.keyboard.press("Enter");
  await shows("pasted-from-the-menu");

  section("the terminal: select all");
  await rightClickAt(...await empty());
  await menu().locator("[data-item=select_all]").click();
  await gone();
  const all = await termSelection();
  console.log(`selected: ${["copy-me-please", "pasted-from-the-menu"].every(t => all.includes(t)) ? "the whole screen" : JSON.stringify(all)}`);

  section("the terminal: Ctrl+Shift+C and Ctrl+Shift+V");
  await dblclickAt("pasted-from-the-menu");
  await page.keyboard.press("Control+Shift+C");
  await page.waitForFunction(() => navigator.clipboard.readText().then(t => t !== "echo pasted-from-the-menu"));
  console.log(`clipboard: ${await clipboard()}`);
  await page.evaluate(() => navigator.clipboard.writeText("echo pasted-by-keys"));
  await page.keyboard.press("Control+Shift+V");
  await page.waitForFunction(() =>
    document.querySelector(".terminal-panel .xterm-rows").textContent.includes("$ echo pasted-by-keys"));
  await page.keyboard.press("Enter");
  await shows("pasted-by-keys");
  // Ctrl+C is still the shell's interrupt.
  await page.keyboard.type("sleep 100");
  await page.keyboard.press("Enter");
  await page.keyboard.press("Control+C");
  await page.waitForFunction(() => {
    const rows = [...document.querySelectorAll(".terminal-panel .xterm-rows > div")].map(r => r.textContent.trimEnd()).filter(r => r);
    return rows.at(-1).endsWith("$") && rows.some(r => r.includes("sleep 100"));
  });
  console.log("Ctrl+C: interrupted");
  console.log(`the shell saw: ${JSON.stringify((await rows())
    .filter(r => r.includes("echo") || r.includes("pasted")).map(r => r.replace(/^.*\$ /, "$ ")))}`);
  await page.locator(".terminal-close").click();
  await page.waitForSelector(".terminal-panel", { state: "detached" });

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  await screenshot("failure");
  console.error(await page.evaluate(() => document.querySelector(".toasts")?.innerText));
  console.error(errors.join("\n"));
  throw error;
} finally {
  await end(() => stopServer(), () => browser.close());
}
