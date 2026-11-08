// The terminal's extra keys on phones (terminal_keys_widget.ml): at 390x844
// and 320x640 (Chromium as a mobile browser, Firefox with the viewport and
// touch, WebKit), and as an installed PWA on a notched phone (Chromium: the
// safe area's insets emulated). With the keyboard up (a shorter visual
// viewport) the bar sits at its foot, every key a thumb's size, none off
// screen or covered; a tap keeps the shell's focus. ↑ recalls the last
// command, Tab completes, Ctrl then c interrupts `sleep 100`, a double-tap
// locks Ctrl, an arrow held repeats and follows the cursor keys' mode.
// On a desktop there is no bar. Run by prigh_web.sh; prints what it sees
// (the expected output is in ENGINE.expected) and, with SHOTS, screenshots.
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
if (!engine) throw new Error("usage: terminal_keys.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "terminal_keys");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });
writeFileSync(join(cwd, "uniquely-named-notes.txt"), "completed-by-tab\n");
setTimeout(() => { console.error("terminal_keys.mjs: timed out"); process.exit(2); }, 280000).unref();

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-token", token, "-cwd", cwd],
  // The terminal's tmux server is ours, so its shell is $SHELL.
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
const section = label => console.log(`=== ${engineName}: terminal keys: ${label} ===`);

const shoot = async (page, tag) => {
  if (!process.env.SHOTS) return;
  await page.screenshot({ path: join(process.env.SHOTS, `terminal-keys-${engineName}-${tag}.png`) });
};

// The bar and the panel's controls: where they are, what is wrong.
const audit = page => page.evaluate(() => {
  const out = [];
  const W = innerWidth;
  const H = window.visualViewport?.height ?? innerHeight;
  const sw = document.scrollingElement.scrollWidth;
  if (sw > W + 1) out.push(`the page overflows sideways: ${sw} > ${W}`);
  const bar = document.querySelector(".terminal-keys");
  const b = bar.getBoundingClientRect();
  if (getComputedStyle(bar).display === "none") return "no bar";
  out.push(`the bar: full width: ${Math.abs(b.left) < 1 && Math.abs(b.right - W) < 1}; at the visual viewport's foot: ${Math.abs(b.bottom - H) < 1}; scrolls sideways: ${bar.scrollWidth > bar.clientWidth + 1}`);
  const name = e => e.getAttribute("aria-label") || e.innerText.trim();
  const off = [], covered = [], small = [], unnamed = [];
  for (const e of document.querySelectorAll(".terminal-panel button")) {
    const r = e.getBoundingClientRect();
    if (r.width < 1) continue;
    if (!e.getAttribute("aria-label")) unnamed.push(e.innerText.trim());
    // A key past the bar's edge is scrolled to.
    const box = e.closest(".terminal-keys") ? b : { left: 0, right: W, top: 0, bottom: H };
    if (r.right <= box.left + 1 || r.left >= box.right - 1) continue;
    if (r.top < -1 || r.bottom > H + 1) { off.push(name(e)); continue; }
    const cx = Math.min(Math.max(r.left + r.width / 2, box.left + 1), box.right - 1);
    const hit = document.elementFromPoint(cx, r.top + r.height / 2);
    if (hit && !e.contains(hit)) covered.push(`${name(e)} under ${hit.className || hit.tagName}`);
    const min = e.closest(".terminal-keys") ? 36 : 40;
    if (r.height < 43.5 && e.closest(".terminal-keys") || r.width < min - 0.5) small.push(`${name(e)} ${Math.round(r.width)}x${Math.round(r.height)}`);
  }
  if (off.length) out.push(`off screen: ${off.join(" | ")}`);
  if (covered.length) out.push(`covered: ${covered.join(" | ")}`);
  if (small.length) out.push(`small: ${small.join(" | ")}`);
  if (unnamed.length) out.push(`no aria-label: ${unnamed.join(" | ")}`);
  return out.join("\n");
});

const rows = page => page.evaluate(() =>
  [...document.querySelectorAll(".terminal-panel .xterm-rows > div")].map(r => r.textContent.trimEnd()).filter(r => r));
// [text] [count] times on the screen (a line may wrap).
const has = (page, text, count = 1) => page.waitForFunction(([t, n]) =>
  document.querySelector(".terminal-panel .xterm-rows").textContent.split(t).length > n,
  [text, count]);
const lastRow = page => page.evaluate(() =>
  [...document.querySelectorAll(".terminal-panel .xterm-rows > div")].map(r => r.textContent.trimEnd()).filter(r => r).at(-1));
const lastRowIs = (page, text) => page.waitForFunction(t =>
  [...document.querySelectorAll(".terminal-panel .xterm-rows > div")].map(r => r.textContent.trimEnd()).filter(r => r).at(-1) === t, text);
const focused = page => page.evaluate(() => !!document.activeElement?.closest(".terminal-host"));
const key = (page, label) => page.locator(`.terminal-keys [aria-label^="${label}"]`);

const open = async (page, { touch }) => {
  await page.goto(url, { timeout: 60000 });
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]")[touch ? "tap" : "click"]();
  const editor = page.locator(".composer textarea");
  await editor.waitFor();
  await page.addStyleTag({ content: ".toasts, .toast { display: none !important; } *, *::before, *::after { animation: none !important; transition: none !important; }" });
  // The terminal button; on a phone, in the ⋯ sheet.
  if (touch) {
    await page.getByRole("button", { name: "Panels and settings" }).tap();
    await page.locator(".chrome-menu .terminal-row").tap();
  } else {
    await page.locator(".terminal-toggle").click();
  }
  await page.waitForFunction(() =>
    document.querySelector(".terminal-panel .xterm-rows")?.textContent.trim().endsWith("$"), null, { timeout: 60000 });
  // On a phone the keyboard comes with a tap on the shell.
  if (touch) await page.locator(".terminal-host").tap();
  await page.waitForFunction(() => document.activeElement?.closest(".terminal-host"));
  await page.keyboard.type(String.raw`PS1='$ '; printf '\033[H\033[2J\033[3J'`);
  await page.keyboard.press("Enter");
  await page.waitForFunction(() => document.querySelector(".terminal-panel .xterm-rows").textContent.trim() === "$");
};

const desktop = async () => {
  const context = await browser.newContext({ viewport: { width: 1280, height: 800 } });
  const page = await context.newPage();
  await open(page, { touch: false });
  section("a desktop");
  console.log(`the shell has the focus: ${await focused(page)}; the bar: ${await page.locator(".terminal-keys").isVisible() ? "shown" : "hidden"}`);
  await shoot(page, "desktop");
  await page.locator(".terminal-close").click();
  await page.waitForSelector(".terminal-panel", { state: "detached" });
  await context.close();
};

const phone = async (w, h, { safe = false } = {}) => {
  const tag = `${w}x${h}${safe ? "-pwa" : ""}`;
  const options = { viewport: { width: w, height: h }, hasTouch: true };
  if (engineName !== "firefox") options.isMobile = true;
  const context = await browser.newContext(options);
  context.setDefaultTimeout(20000);
  const page = await context.newPage();
  page.on("console", m => { if (m.type() === "error" && !/favicon\.ico|interactive-widget|WebSocket/.test(m.text())) errors.push(`console: ${m.text()}`); });
  page.on("pageerror", e => errors.push(`page: ${e.message}`));
  if (safe) {
    const cdp = await context.newCDPSession(page);
    await cdp.send("Emulation.setSafeAreaInsetsOverride", { insets: { top: 47, bottom: 34, left: 0, right: 0 } });
  }
  await open(page, { touch: true });

  // The keyboard: the visual viewport shrinks, the panel with it.
  const kh = Math.round(h * 0.55);
  if (engineName === "chromium") {
    const cdp = await context.newCDPSession(page);
    await cdp.send("Emulation.setVisibleSize", { width: w, height: kh }).catch(() => {});
  }
  await page.setViewportSize({ width: w, height: kh });
  await page.waitForFunction(h => Math.abs((window.visualViewport?.height ?? innerHeight) - h) < 1
    && Math.abs(document.querySelector(".terminal-keys").getBoundingClientRect().bottom - h) < 1, kh);
  section(`${tag}: the keyboard up`);
  console.log(await audit(page));
  console.log(`keys: ${await page.locator(".terminal-keys button").evaluateAll(es => es.map(e => e.innerText).join(" "))}`);
  await shoot(page, `${tag}-keyboard`);

  section(`${tag}: ↑ recalls the last command`);
  await page.keyboard.type("echo first-command");
  await page.keyboard.press("Enter");
  await has(page, "first-command", 2);
  await key(page, "Up arrow").tap();
  await lastRowIs(page, "$ echo first-command");
  console.log(`the line: ${await lastRow(page)}; the shell kept the focus: ${await focused(page)}`);
  await page.keyboard.press("Enter");
  await has(page, "first-command", 4);

  section(`${tag}: Tab completes`);
  await page.keyboard.type("cat uniquely-n");
  await key(page, "Tab").tap();
  await lastRowIs(page, "$ cat uniquely-named-notes.txt");
  console.log(`the line: ${await lastRow(page)}`);
  await page.keyboard.press("Enter");
  await has(page, "completed-by-tab");

  section(`${tag}: Ctrl then c interrupts sleep`);
  await page.keyboard.type("sleep 100");
  await page.keyboard.press("Enter");
  await key(page, "Control").tap();
  const ctrl = () => key(page, "Control").evaluate(e =>
    `aria-pressed=${e.getAttribute("aria-pressed")} ${e.classList.contains("locked") ? "locked" : e.classList.contains("latched") ? "latched" : "off"}`);
  console.log(`Ctrl after a tap: ${await ctrl()}`);
  await shoot(page, `${tag}-ctrl-latched`);
  await page.keyboard.type("c");
  // Within a second: not the sleep's 100.
  await page.waitForFunction(() =>
    [...document.querySelectorAll(".terminal-panel .xterm-rows > div")].map(r => r.textContent.trimEnd()).filter(r => r).at(-1) === "$",
    null, { timeout: 5000 });
  console.log(`interrupted: the prompt is back; Ctrl after the key: ${await ctrl()}`);
  await page.keyboard.type("echo after-interrupt");
  await page.keyboard.press("Enter");
  await has(page, "after-interrupt", 2);
  console.log(`a plain c again: ${(await rows(page)).includes("after-interrupt")}`);

  section(`${tag}: a double tap locks Ctrl`);
  await key(page, "Control").tap();
  await key(page, "Control").tap();
  console.log(`Ctrl: ${await ctrl()}`);
  await key(page, "Control").tap();
  console.log(`tapped again: ${await ctrl()}`);

  section(`${tag}: the cursor keys' mode, an arrow held`);
  await page.keyboard.type(String.raw`printf '\033[?1h'; echo app-mode; cat -v`);
  await page.keyboard.press("Enter");
  await has(page, "app-mode", 2);
  await key(page, "Up arrow").tap();
  await key(page, "Left arrow").dispatchEvent("pointerdown");
  await page.waitForFunction(() =>
    (document.querySelector(".terminal-panel .xterm-rows").textContent.match(/\^\[OD/g) ?? []).length >= 4);
  await key(page, "Left arrow").dispatchEvent("pointerup");
  await page.keyboard.press("Enter");
  await has(page, "^[OA^[OD^[OD^[OD^[OD", 2);
  // The line as typed (echoed) and as cat wrote it back: ↑ once, ← four or more times.
  const typed = await page.evaluate(() => {
    const text = document.querySelector(".terminal-panel .xterm-rows").textContent;
    return text.slice(text.lastIndexOf("app-mode") + "app-mode".length).replace(/\s/g, "");
  });
  console.log(`application mode: ↑ sends ^[OA, ← held repeats: ${/^(\^\[OA(?:\^\[OD){4,})\1$/.test(typed)}`);
  await key(page, "Control").tap();
  await page.keyboard.type("c");
  await lastRowIs(page, "$");
  await page.keyboard.type(String.raw`printf '\033[?1l'; echo normal-mode; cat -v`);
  await page.keyboard.press("Enter");
  await has(page, "normal-mode", 2);
  await key(page, "Up arrow").tap();
  await key(page, "Home").tap();
  await key(page, "Page up").tap();
  await page.keyboard.press("Enter");
  await has(page, "^[[A^[[H^[[5~", 2);
  console.log("normal mode: ↑ Home PgUp send ^[[A ^[[H ^[[5~");
  await key(page, "Control").tap();
  await page.keyboard.type("c");
  await lastRowIs(page, "$");
  await page.keyboard.type("clear");
  await page.keyboard.press("Enter");
  await lastRowIs(page, "$");

  section(`${tag}: the bar scrolled to its end`);
  await page.locator(".terminal-keys").evaluate(e => { e.scrollLeft = e.scrollWidth; });
  console.log(await audit(page));
  console.log(`the shell has the focus: ${await focused(page)}`);
  await shoot(page, `${tag}-scrolled`);

  section(`${tag}: the shell without the focus`);
  await page.evaluate(() => document.activeElement.blur());
  console.log(`the bar: ${await page.locator(".terminal-keys").isVisible() ? "shown" : "hidden"}`);
  // Closed: the next visit opens it afresh (its being open is remembered).
  await page.locator(".terminal-close").tap();
  await page.waitForSelector(".terminal-panel", { state: "detached" });
  await context.close();
};

try {
  await desktop();
  await phone(390, 844);
  await phone(320, 640);
  if (engineName === "chromium") await phone(390, 844, { safe: true });
  if (errors.length) {
    section("errors");
    console.log(errors.join("\n"));
  }
} catch (e) {
  for (const c of browser.contexts()) for (const p of c.pages()) { await p.screenshot({ path: join(dir, "failed.png") }).catch(() => {}); console.log(await p.evaluate(() => [document.activeElement?.outerHTML.slice(0, 200), JSON.stringify(document.querySelector(".terminal-panel .xterm-rows")?.textContent)]).catch(() => "")); }
  console.log(`FAILED: ${e.stack}\n${errors.join("\n")}`);
  process.exitCode = 1;
} finally {
  await end(() => browser.close(), () => server.kill());
}
