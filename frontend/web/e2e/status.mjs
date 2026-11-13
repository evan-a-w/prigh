// The session's one status (at the composer's right: activity, context,
// cost), the chips beside the model and host (thinking, mode, transcript)
// and their menus, on a desktop by mouse and keyboard and on phones by
// touch, and the phone's drags. It prints, as stable lines:
//
// - desktop (1280 and 700 wide): the status shows and clears the mic and
//   send; each of thinking, mode and transcript picked from its menu by the
//   mouse and by the keyboard (the chip says the choice); each menu (model,
//   host, +, thinking, mode, transcript) closed by a click outside, which
//   reaches nothing under it; the status's details on hover and on focus;
//   the context opening the session's details;
// - phones (390x844, 320x640): the composer's row fits; the ⋯ sheet's
//   thinking, mode and transcript picked by tap, still so when reopened;
//   the model, host and + sheets closed by a tap outside, which reaches
//   nothing under it; the drags: the drawer from the edge and back, a
//   session's row to its Archive and Delete (put back by a tap), a sheet
//   down by its grip (the model's, the ⋯), a settings section back from
//   the edge, a pending image's tile up (removed), the viewer at fit aside
//   (the next image) and down (closed);
// - an installed PWA on a notched phone (Chromium): the strip under the
//   notch, the composer over the home indicator.
//
// Run by prigh_web.sh; the expected output is in ENGINE.expected. With
// SHOTS, screenshots named status-ENGINE-NN-STEP.png.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { hostname } from "node:os";
import { join } from "node:path";
import { end } from "./end.mjs";
import { png } from "./png.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: status.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "status");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });
setTimeout(() => { console.error("status.mjs: timed out"); process.exit(2); }, 280000).unref();

const reply = i => ({ text: `Reply ${i}.`, usage: { input: 360000 + i, output: 12000, cache_read: 300000 } });
writeFileSync(join(dir, "script.json"), JSON.stringify(Array.from({ length: 12 }, (_, i) => reply(i))));

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
const section = label => console.log(`=== ${engineName}: status: ${label} ===`);
const say = line => console.log(line);
let page;
let shot = 0;
const shoot = async label => {
  if (!process.env.SHOTS) return;
  await page.waitForTimeout(200);
  await page.screenshot({
    path: join(process.env.SHOTS, `status-${engineName}-${String(++shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};

const settle = () => page.waitForTimeout(250);
const chipText = chip => page.locator(`.composer .chip-anchor.${chip} > button`).innerText().then(t => t.trim().replace(/\s+/g, " "));
const popoverOpen = () => page.locator("#chip-popover").count().then(n => n > 0);

// A finger from (x0, y0) to (x1, y1), as touch events on what is under it
// (the page's drags listen to touches).
const swipe = (x0, y0, x1, y1, steps = 8, ms = 160) => page.evaluate(async ([x0, y0, x1, y1, steps, ms]) => {
  const target = document.elementFromPoint(x0, y0);
  const fire = (type, x, y) => {
    // WebKit has no Touch constructor, only the older document.createTouch.
    let t, list = ts => ts;
    try {
      t = new Touch({ identifier: 1, target, clientX: x, clientY: y, pageX: x, pageY: y });
    } catch {
      t = document.createTouch(window, target, 1, x + scrollX, y + scrollY, x, y);
      list = ts => document.createTouchList(...ts);
    }
    const live = list(type === "touchend" ? [] : [t]);
    target.dispatchEvent(new TouchEvent(type, { bubbles: true, cancelable: true, composed: true, touches: live, targetTouches: live, changedTouches: list([t]) }));
  };
  fire("touchstart", x0, y0);
  for (let i = 1; i <= steps; i++) {
    await new Promise(r => setTimeout(r, ms / steps));
    fire("touchmove", x0 + ((x1 - x0) * i) / steps, y0 + ((y1 - y0) * i) / steps);
  }
  fire("touchend", x1, y1);
}, [x0, y0, x1, y1, steps, ms]);
// The same as touch pointers (the image viewer listens to pointers).
const pointerSwipe = (x0, y0, x1, y1, steps = 8, ms = 160) => page.evaluate(async ([x0, y0, x1, y1, steps, ms]) => {
  const target = document.elementFromPoint(x0, y0);
  const fire = (type, x, y) => target.dispatchEvent(new PointerEvent(type, {
    bubbles: true, cancelable: true, composed: true, pointerId: 7, pointerType: "touch", isPrimary: true,
    clientX: x, clientY: y, buttons: type === "pointerup" ? 0 : 1,
  }));
  fire("pointerdown", x0, y0);
  for (let i = 1; i <= steps; i++) {
    await new Promise(r => setTimeout(r, ms / steps));
    fire("pointermove", x0 + ((x1 - x0) * i) / steps, y0 + ((y1 - y0) * i) / steps);
  }
  fire("pointerup", x1, y1);
}, [x0, y0, x1, y1, steps, ms]);

const box = async sel => page.locator(sel).first().boundingBox();
const overlaps = (a, b) => !!a && !!b && a.x < b.x + b.width - 0.5 && b.x < a.x + a.width - 0.5 && a.y < b.y + b.height - 0.5 && b.y < a.y + a.height - 0.5;
// The composer's row: nothing past the composer, no button on another.
const rowFits = () => page.evaluate(() => {
  const c = document.querySelector(".composer-box").getBoundingClientRect();
  const leaves = [...document.querySelectorAll(".composer-row button")]
    .filter(e => e.getBoundingClientRect().width > 0 && !e.closest("#chip-popover, .status-details"));
  const name = e => (e.getAttribute("aria-label") || e.className.split(" ")[0]).slice(0, 30);
  const past = leaves.filter(e => e.getBoundingClientRect().right > c.right + 0.5 || e.getBoundingClientRect().left < c.left - 0.5).map(name);
  const sideways = document.scrollingElement.scrollWidth > innerWidth + 1;
  const on = [];
  for (let i = 0; i < leaves.length; i++) for (let j = i + 1; j < leaves.length; j++) {
    if (leaves[i].contains(leaves[j]) || leaves[j].contains(leaves[i])) continue;
    const a = leaves[i].getBoundingClientRect(), b = leaves[j].getBoundingClientRect();
    if (a.left < b.right - 0.5 && b.left < a.right - 0.5 && a.top < b.bottom - 0.5 && b.top < a.bottom - 0.5)
      on.push(`${name(leaves[i])} on ${name(leaves[j])}`);
  }
  return `the row fits: ${!past.length && !sideways && !on.length}${past.length ? `; past it: ${past.join(", ")}` : ""}${on.length ? `; overlapping: ${on.join(", ")}` : ""}${sideways ? "; the page scrolls sideways" : ""}`;
});

const signIn = async () => {
  await page.goto(url, { timeout: 60000 });
  await page.locator("form input[type=password]").fill(token);
  await page.keyboard.press("Enter");
  await page.locator(".composer textarea").waitFor();
  await page.addStyleTag({ content: ".toasts, .toast { display: none !important; }" });
};
let turn = 0;
const send = async text => {
  const editor = page.locator(".composer textarea");
  await editor.fill(text);
  await editor.press("Enter");
  const want = `Reply ${turn++}.`;
  await page.waitForFunction(w => document.querySelector("#chat")?.innerText.includes(w), want, { timeout: 60000 });
  await page.waitForFunction(() => !document.querySelector(".composer.busy"));
};

const pick = async (chip, option, how) => {
  const button = page.locator(`.composer .chip-anchor.${chip} > button`);
  if (how === "mouse") {
    await button.click();
    await page.locator("#chip-popover").waitFor();
    if (chip === "thinking") await shoot("desktop-thinking-menu");
    await page.locator("#chip-popover .popover-item").filter({ hasText: new RegExp(`^\\s*${option}`, "i") }).first().click();
  } else {
    await button.focus();
    await page.keyboard.press("Enter");
    await page.locator("#chip-popover").waitFor();
    // The transcript's choice is its last: up from it.
    await page.keyboard.press(chip === "verbosity" ? "ArrowUp" : "ArrowDown");
    await page.keyboard.press("Enter");
  }
  await page.locator("#chip-popover").waitFor({ state: "detached", timeout: 5000 }).catch(() => {});
  await settle();
  say(`${chip} by ${how}: ${await chipText(chip)}; menu closed: ${!(await popoverOpen())}`);
};

const desktop = async () => {
  const context = await browser.newContext({ viewport: { width: 1280, height: 800 } });
  context.setDefaultTimeout(20000);
  page = await context.newPage();
  page.on("pageerror", e => errors.push(`page: ${e.message}`));
  await signIn();
  await send("hello");
  for (const w of [1280, 700]) {
    await page.setViewportSize({ width: w, height: 800 });
    await settle();
    section(`desktop ${w}: the status`);
    const status = await box(".composer-status");
    const mic = await box(".composer .mic");
    const sendButton = await box(".composer .send");
    say(`shows: ${!!status && status.width > 0}; clear of the mic and send: ${!overlaps(status, mic) && !overlaps(status, sendButton)}`);
    say(await rowFits());
    say(`it says: ${(await page.locator(".composer-status").innerText()).trim().split("\n")[0]}`);
    await shoot(`desktop-${w}`);
  }
  await page.setViewportSize({ width: 1280, height: 800 });
  await settle();

  section("desktop: the chips' menus act");
  await pick("thinking", "high", "mouse");
  await pick("mode", "ask", "mouse");
  await pick("verbosity", "verbose", "mouse");
  await pick("thinking", "", "keyboard");
  await pick("mode", "", "keyboard");
  await pick("verbosity", "", "keyboard");
  await shoot("desktop-chips");

  section("desktop: a click outside closes, and goes no further");
  for (const [chip, sel] of [["model", ".model-chip"], ["host", ".host-chip"], ["+", ".composer-plus .plus"], ["thinking", ".chip-anchor.thinking > button"], ["mode", ".chip-anchor.mode > button"], ["verbosity", ".chip-anchor.verbosity > button"]]) {
    await page.locator(`.composer ${sel}`).first().click();
    await page.locator("#chip-popover").waitFor();
    // On the filter's button: had the click gone through, its menu would open.
    const filter = await box(".chat-chrome .filter-toggle");
    await page.mouse.click(filter.x + filter.width / 2, filter.y + filter.height / 2);
    await settle();
    say(`${chip}: closed: ${!(await popoverOpen())}; the filter's menu opened: ${await page.locator(".filter-menu").count() > 0}`);
    if (await page.locator(".filter-menu").count()) await page.keyboard.press("Escape");
  }

  section("desktop: the status's details");
  await page.locator(".composer-status .status-gauge").hover();
  await settle();
  say(`on hover: ${await page.locator(".composer-status .status-details").isVisible()}`);
  await shoot("desktop-details");
  await page.mouse.move(640, 200);
  await settle();
  // By the keyboard (focus that shows): back from the mic.
  await page.locator(".composer .mic").focus();
  await page.keyboard.press("Shift+Tab");
  await settle();
  say(`on keyboard focus (${await page.evaluate(() => document.activeElement?.getAttribute("aria-label")?.split(":")[0])}): ${await page.locator(".composer-status .status-details").isVisible()}`);
  await page.locator(".composer-status button.context").click();
  await page.locator(".modal").waitFor();
  say(`the context opens: ${(await page.locator(".modal h2").first().innerText()).trim()}`);
  await page.keyboard.press("Escape");
  await page.waitForSelector(".modal", { state: "detached" });
  await context.close();
};

const tap = (x, y) => page.touchscreen.tap(x, y);
const more = () => page.getByRole("button", { name: "Panels and settings" }).tap();
const closeSheet = async () => {
  if (await page.locator(".chrome-backdrop").count()) await page.locator(".chrome-backdrop").tap({ position: { x: 10, y: 10 }, force: true });
  await settle();
};

const phone = async (w, h, { pwa = false } = {}) => {
  const tag = `${w}x${h}${pwa ? " pwa" : ""}`;
  const options = { viewport: { width: w, height: h }, hasTouch: true };
  if (engineName !== "firefox") options.isMobile = true;
  const context = await browser.newContext(options);
  context.setDefaultTimeout(20000);
  page = await context.newPage();
  page.on("pageerror", e => errors.push(`page: ${e.message}`));
  if (pwa) {
    const cdp = await context.newCDPSession(page);
    await cdp.send("Emulation.setSafeAreaInsetsOverride", { insets: { top: 47, bottom: 34, left: 0, right: 0 } });
  }
  await signIn();
  await send(`hello from ${tag}`);
  await page.addStyleTag({ content: "*, *::before, *::after { animation: none !important; }" });

  if (pwa) {
    section(`${tag}: the safe areas`);
    // The strip runs under the notch; its buttons are below it.
    const strip = await box(".chat-chrome .toggle-sidebar");
    const composer = await box(".composer-box");
    say(`the strip under the notch: ${strip.y >= 46.5}; the composer over the home indicator: ${composer.y + composer.height <= h - 33.5}`);
    say(await rowFits());
    await shoot(tag);
    await context.close();
    return;
  }

  section(`${tag}: the composer's row`);
  say(await rowFits());
  say(`it shows: ${(await page.locator(".composer-row button:visible").evaluateAll(bs => bs.map(b => (b.getAttribute("aria-label") || b.innerText).trim().split("\n")[0]).join(", "))).replaceAll(hostname(), "<HOSTNAME>")}`);
  await shoot(`${tag}-composer`);

  section(`${tag}: the ⋯ sheet's choices act`);
  for (const [name, option] of [["Thinking", "high"], ["Mode", "ask"], ["Transcript", "verbose"]]) {
    await more();
    const sec = page.locator(".chrome-menu .session-section", { has: page.locator(".session-section-title", { hasText: name }) });
    await sec.locator(".session-choice", { hasText: new RegExp(`^${option}$`) }).tap();
    await settle();
    const now = await sec.locator(".session-choice.on").innerText().catch(() => "none");
    // Closed and opened again: the session has it.
    await closeSheet();
    await more();
    const again = await sec.locator(".session-choice.on").innerText().catch(() => "none");
    say(`${name}: ${now}; reopened: ${again}`);
    if (name === "Thinking" || name === "Mode") await shoot(`${tag}-sheet-${name.toLowerCase()}`);
    await closeSheet();
  }

  section(`${tag}: a tap outside closes, and goes no further`);
  const menu = await box(".chat-chrome .toggle-sidebar");
  for (const [chip, sel] of [["model", ".model-chip"], ["host", ".host-chip"], ["+", ".composer-plus .plus"]]) {
    await page.locator(`.composer ${sel}`).first().tap();
    await page.locator("#chip-popover").waitFor();
    // On the ☰: had the tap gone through, the drawer would open.
    await tap(menu.x + menu.width / 2, menu.y + menu.height / 2);
    await settle();
    say(`${chip}: closed: ${!(await popoverOpen())}; the drawer opened: ${await page.locator(".app.sidebar-open").count() > 0}`);
  }

  section(`${tag}: drags`);
  const drawerOpen = async () => { await page.waitForTimeout(400); return page.locator(".app.sidebar-open").count().then(n => n > 0); };
  await swipe(4, h / 2, w * 0.8, h / 2);
  say(`from the left edge: the drawer opens: ${await drawerOpen()}`);
  await shoot(`${tag}-drawer`);
  const row = await box(".sidebar .session");
  await swipe(row.x + row.width - 30, row.y + row.height / 2, row.x + 20, row.y + row.height / 2);
  await settle();
  say(`a row to the left: its actions: ${await page.locator(".session.swiped .session-swipe-action").evaluateAll(bs => bs.map(b => b.innerText.trim()).join(", "))}`);
  await shoot(`${tag}-row`);
  const head = await box(".sidebar-head");
  await tap(head.x + 40, head.y + head.height / 2);
  await settle();
  say(`a tap elsewhere puts it back: ${await page.locator(".session.swiped").count() === 0}`);
  await swipe(w * 0.6, h * 0.75, 10, h * 0.75);
  say(`dragged left: the drawer closes: ${!(await drawerOpen())}`);

  await page.locator(".composer .model-chip").tap();
  await page.locator("#chip-popover").waitFor();
  let sheet = await box("#chip-popover");
  await swipe(sheet.x + sheet.width / 2, sheet.y + 8, sheet.x + sheet.width / 2, sheet.y + 300);
  await page.waitForTimeout(400);
  say(`the model's sheet down by its grip: closed: ${!(await popoverOpen())}`);
  await more();
  sheet = await box(".chrome-menu");
  await swipe(sheet.x + sheet.width / 2, sheet.y + 6, sheet.x + sheet.width / 2, sheet.y + 300);
  await page.waitForTimeout(400);
  say(`the ⋯ sheet down by its grip: closed: ${await page.locator(".chrome-menu").count() === 0}`);

  await more();
  await page.locator(".chrome-menu .settings-row").tap();
  await page.locator(".settings-home-row[data-settings-tab=appearance]").tap();
  await page.locator(".modal-back").waitFor();
  await swipe(4, h / 2, w * 0.7, h / 2);
  await settle();
  say(`a settings section, from the left edge: back to the list: ${await page.locator(".settings-home").count() > 0}`);
  await page.getByRole("button", { name: "Close" }).first().tap();
  await page.waitForSelector(".modal", { state: "detached" });

  const image = (r, g, b) => ({ name: `i${r}${g}.png`, mimeType: "image/png", buffer: png(40, 30, () => [r, g, b]) });
  await page.locator("#image-attach-input").setInputFiles([image(200, 40, 40), image(40, 200, 40), image(40, 40, 200)]);
  await page.waitForFunction(() => document.querySelectorAll(".pending-image").length === 3);
  const tile = await box(".pending-image");
  await swipe(tile.x + tile.width / 2, tile.y + tile.height / 2, tile.x + tile.width / 2, tile.y - 90);
  await settle();
  say(`a pending image's tile up: removed: ${await page.locator(".pending-image").count() === 2}`);
  await send("two images");
  await page.locator("#chat [data-viewer]").first().tap();
  await page.locator("dialog.viewer").waitFor();
  await settle();
  const count = () => page.locator("dialog.viewer .viewer-count").innerText().then(t => t.trim()).catch(() => "");
  const before = await count();
  const img = await box("dialog.viewer .viewer-img");
  const cx = img.x + img.width / 2, cy = img.y + img.height / 2;
  await pointerSwipe(cx + 60, cy, cx - 100, cy);
  await settle();
  say(`the viewer at fit, aside: ${before} to ${await count()}`);
  await pointerSwipe(cx, cy - 20, cx, cy + 200);
  await settle();
  say(`down: closed: ${await page.locator("dialog.viewer").count() === 0}`);
  await context.close();
};

try {
  await desktop();
  await phone(390, 844);
  await phone(320, 640);
  if (engineName === "chromium") await phone(390, 844, { pwa: true });
  if (errors.length) {
    section("errors");
    say(errors.join("\n"));
  }
} catch (e) {
  if (process.env.SHOTS && page) await page.screenshot({ path: join(process.env.SHOTS, `status-${engineName}-failed.png`) }).catch(() => {});
  console.log(`FAILED: ${e.stack}\n${errors.join("\n")}`);
  process.exitCode = 1;
} finally {
  await end(() => browser.close(), () => server.kill());
}
