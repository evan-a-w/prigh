// The composer's row (attach and the context ring at the left; the host,
// the model and send at the right), the model's menu (opaque from its first
// frame; its reasoning and access mode) and the ring's card, on a desktop
// by mouse and keyboard and on phones by touch, and the phone's drags. It
// prints, as stable lines:
//
// - desktop (1280 and 700 wide): the row's buttons in order, the chips
//   apart from the ring, the model's name whole; the model's menu opaque as
//   it opens; the reasoning and the mode picked in it by the mouse and by
//   the keyboard (Alt+T, Alt+M: the chip and the ring's card say the
//   choice); each menu (model, host, +) closed by a click outside, which
//   reaches nothing under it; the ring's card on hover, on focus and on a
//   click; its Details opening the session's details;
// - phones (390x844, 320x640): the composer's row fits, in order, the
//   model's name whole, no button under 40px (the composer's dense row, as
//   in mobile.mjs); the model sheet's reasoning and access picked by tap,
//   and the ⋯ sheet's transcript, still so when reopened; the ring's card
//   as a sheet; the model, host and + sheets closed by a tap outside, which
//   reaches nothing under it; the drags: the drawer from the edge and back,
//   a session's row to its Archive and Delete (put back by a tap), a sheet
//   down by its grip (the model's, the ⋯), a settings section back from the
//   edge, a pending image's tile up (removed), the viewer at fit aside (the
//   next image) and down (closed);
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
const popoverGone = () => page.locator("#chip-popover").waitFor({ state: "detached", timeout: 5000 }).catch(() => {});
// The model chip as it shows: its short name.
const modelChip = () => page.locator(".composer .model-chip").evaluate(b =>
  [...b.querySelectorAll(".chip-label")].filter(e => getComputedStyle(e).display !== "none").map(e => e.textContent.trim()).join(" · "));
// The row's buttons left to right, and whether they are in the order wanted.
// On a desktop the chips are apart from the ring (a phone's row is full).
const rowOrder = ({ apart = false } = {}) => page.evaluate(apart => {
  const bs = [...document.querySelectorAll(".composer-row button")]
    .filter(b => b.getBoundingClientRect().width > 0 && !b.closest("#chip-popover, .context-card"))
    .sort((a, b) => a.getBoundingClientRect().left - b.getBoundingClientRect().left);
  const kind = b => b.matches(".plus") ? "attach" : b.matches(".host-chip") ? "host"
    : b.matches(".model-chip") ? "model" : b.matches(".context-ring") ? "ring" : b.matches(".send, .stop") ? "send"
    : (b.getAttribute("aria-label") || b.className.split(" ")[0]);
  const kinds = bs.map(kind);
  const want = ["attach", "ring", "host", "model", "send"];
  const inOrder = want.every(k => kinds.includes(k)) && want.every((k, i) => i === 0 || kinds.indexOf(want[i - 1]) < kinds.indexOf(k));
  const at = k => bs[kinds.indexOf(k)]?.getBoundingClientRect();
  const gap = at("ring") && at("host") ? at("host").left - at("ring").right : 0;
  return `the row: ${kinds.join(", ")}; in order: ${inOrder}${apart ? `; the chips at the right, apart from the ring: ${gap > 24}` : ""}`;
}, apart);
// The model's name whole: its label not cut.
const nameWhole = () => page.locator(".composer .model-chip .chip-label").evaluate(e => e.scrollWidth <= e.clientWidth + 0.5);
// The menu's opacity (with its ancestors') at the first frame it is there:
// no fade in.
const openOpacity = () => page.waitForFunction(() => {
  const e = document.querySelector("#chip-popover");
  if (!e) return false;
  let o = 1;
  for (let n = e; n; n = n.parentElement) o *= Number(getComputedStyle(n).opacity);
  return String(o);
}, null, { polling: "raf" }).then(h => h.jsonValue());
const cardLine = cls => page.locator(`.context-card .${cls} .status-detail-value`).textContent().then(t => t.trim());
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
    .filter(e => e.getBoundingClientRect().width > 0 && !e.closest("#chip-popover, .context-card"));
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

// The reasoning or the mode picked in the model's menu by the mouse, or by
// the keyboard (Alt+T cycles the reasoning, Alt+M the mode).
const pick = async (what, option, how) => {
  if (how === "mouse") {
    await page.locator(".composer .model-chip").click();
    await page.locator("#chip-popover.model-menu").waitFor();
    if (what === "reasoning") {
      await shoot("desktop-model-menu");
      await page.locator("#chip-popover .reasoning-row", { has: page.locator(".reasoning-label", { hasText: new RegExp(`^${option}$`, "i") }) }).click();
      await page.locator("#chip-popover .reasoning-row.on .reasoning-label", { hasText: new RegExp(`^${option}$`, "i") }).waitFor();
    } else {
      await page.locator("#chip-popover .model-access-button").click();
      await page.locator("#chip-popover .model-access-menu").waitFor();
      await shoot("desktop-access-menu");
      await page.locator("#chip-popover .model-access-item", { has: page.locator(".menu-label", { hasText: new RegExp(`^${option}$`) }) }).click();
      await page.locator("#chip-popover .model-access-button .chip-label", { hasText: new RegExp(`^${option}$`) }).waitFor();
    }
    await page.keyboard.press("Escape");
    await popoverGone();
  } else {
    await page.locator(".composer textarea").focus();
    await page.keyboard.press(what === "reasoning" ? "Alt+T" : "Alt+M");
  }
  await page.waitForFunction(() => !document.querySelector(".composer .awaiting, .composer .model-chip.pending"));
  await settle();
  say(`${what} by ${how}: the chip: ${await modelChip()}; the card: ${await cardLine(what === "reasoning" ? "reasoning-line" : "mode-line")}; menu closed: ${!(await popoverOpen())}`);
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
    section(`desktop ${w}: the row`);
    say(await rowOrder({ apart: true }));
    const ring = await box(".composer .context-ring");
    const sendButton = await box(".composer .send");
    say(`the ring shows: ${!!ring && ring.width > 0}; clear of send: ${!overlaps(ring, sendButton)}`);
    say(await rowFits());
    say(`the model: ${await modelChip()}; whole: ${await nameWhole()}`);
    say(`the ring says: ${await page.locator(".composer .context-ring").getAttribute("aria-label")}`);
    await page.locator(".composer textarea").focus();
    say(`no glow around the focused composer: ${await page.locator(".composer-box").evaluate(e => getComputedStyle(e).boxShadow === "none")}`);
    await shoot(`desktop-${w}`);
  }
  await page.setViewportSize({ width: 1280, height: 800 });
  await settle();

  section("desktop: the model's menu opens opaque");
  await page.locator(".composer .model-chip").click();
  say(`opacity as it appears: ${await openOpacity()}`);
  await page.locator("#chip-popover.model-menu").waitFor();
  say(`the reasoning column: ${await page.locator("#chip-popover .model-reasoning-title").count() > 0}; the access mode: ${(await page.locator("#chip-popover .model-access-button .chip-label").innerText()).trim()}`);
  await page.keyboard.press("Escape");
  await popoverGone();

  section("desktop: the model's menu acts");
  await pick("reasoning", "low", "mouse");
  await pick("mode", "Ask first", "mouse");
  await pick("reasoning", "", "keyboard");
  await pick("mode", "", "keyboard");
  await shoot("desktop-chips");

  section("desktop: a click outside closes, and goes no further");
  for (const [chip, sel] of [["model", ".model-chip"], ["host", ".host-chip"], ["+", ".composer-plus .plus"]]) {
    await page.locator(`.composer ${sel}`).first().click();
    await page.locator("#chip-popover").waitFor();
    // On the files' toggle: had the click gone through, the files would open.
    const files = await box(".chat-chrome .files-toggle");
    await page.mouse.click(files.x + files.width / 2, files.y + files.height / 2);
    await settle();
    const filesOpen = () => page.locator(".chat-chrome .files-toggle:is([aria-expanded=true], [aria-pressed=true])").count();
    say(`${chip}: closed: ${!(await popoverOpen())}; the files opened: ${await filesOpen() > 0}`);
    if (await filesOpen()) await page.locator(".chat-chrome .files-toggle").click();
  }

  section("desktop: the ring's card");
  const cardShown = () => page.locator(".context-card").isVisible();
  await page.locator(".composer .context-ring").hover();
  await page.locator(".context-card").waitFor({ state: "visible" });
  say(`on hover: ${await cardShown()}; it says: ${(await page.locator(".context-card-sub").innerText()).trim()}`);
  say(`status: ${await cardLine("status-line")}; reasoning: ${await cardLine("reasoning-line")}; mode: ${await cardLine("mode-line")}`);
  await shoot("desktop-details");
  await page.mouse.move(640, 200);
  await page.locator(".context-card").waitFor({ state: "hidden" });
  // By the keyboard (focus that shows): on from the paperclip.
  await page.locator(".composer .composer-plus .plus").focus();
  await page.keyboard.press("Tab");
  await page.locator(".context-card").waitFor({ state: "visible" });
  say(`on keyboard focus (${await page.evaluate(() => document.activeElement?.getAttribute("aria-label")?.split(":")[0])}): ${await cardShown()}`);
  await page.locator(".composer .context-ring").click();
  await page.locator(".context-ring-anchor.open").waitFor();
  say(`a click keeps it open: ${await cardShown()}`);
  await page.locator(".context-card .context-details").click();
  await page.locator(".modal").waitFor();
  say(`its Details open: ${(await page.locator(".modal h2").first().innerText()).trim()}`);
  await page.keyboard.press("Escape");
  await page.waitForSelector(".modal", { state: "detached" });
  await context.close();
};

const tap = (x, y) => page.touchscreen.tap(x, y);
const more = () => page.getByRole("button", { name: "Panels and settings" }).tap();
const closeChipSheet = async () => {
  await page.locator(".popover-backdrop").tap({ position: { x: 10, y: 10 }, force: true });
  await popoverGone();
};
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
  say(await rowOrder());
  say(`the model: ${await modelChip()}; whole: ${await nameWhole()}`);
  const small = await page.locator(".composer-row button:visible").evaluateAll(bs => bs
    .filter(b => !b.closest("#chip-popover, .context-card"))
    .filter(b => Math.min(b.getBoundingClientRect().width, b.getBoundingClientRect().height) < 39.5)
    .map(b => b.getAttribute("aria-label") || b.className));
  say(`under 40px: ${small.join(", ") || "none"}`);
  await shoot(`${tag}-composer`);

  section(`${tag}: the sheets' choices act`);
  for (const [name, option] of [["Reasoning", "low"], ["Access", "Auto-edit"]]) {
    await page.locator(".composer .model-chip").tap();
    await page.locator("#chip-popover.model-menu").waitFor();
    if (name === "Reasoning") {
      await page.locator("#chip-popover .reasoning-row", { has: page.locator(".reasoning-label", { hasText: new RegExp(`^${option}$`, "i") }) }).tap();
      await page.locator("#chip-popover .reasoning-row.on .reasoning-label", { hasText: new RegExp(`^${option}$`, "i") }).waitFor();
      await shoot(`${tag}-sheet-reasoning`);
    } else {
      await page.locator("#chip-popover .model-access-button").tap();
      await page.locator("#chip-popover .model-access-item", { has: page.locator(".menu-label", { hasText: new RegExp(`^${option}$`) }) }).tap();
      await page.locator("#chip-popover .model-access-button .chip-label", { hasText: new RegExp(`^${option}$`) }).waitFor();
      await shoot(`${tag}-sheet-access`);
    }
    await closeChipSheet();
    await page.waitForFunction(() => !document.querySelector(".composer .model-chip.pending"));
    await page.locator(".composer .model-chip").tap();
    await page.locator("#chip-popover.model-menu").waitFor();
    const again = name === "Reasoning"
      ? await page.locator("#chip-popover .reasoning-row.on .reasoning-label").innerText()
      : await page.locator("#chip-popover .model-access-button .chip-label").innerText();
    say(`${name}: reopened: ${again.trim()}; the chip: ${await modelChip()}`);
    await closeChipSheet();
  }
  {
    await more();
    const sec = page.locator(".chrome-menu .session-section", { has: page.locator(".session-section-title", { hasText: "Transcript" }) });
    await sec.locator(".session-choice", { hasText: /^verbose$/ }).tap();
    await sec.locator(".session-choice.on", { hasText: /^verbose$/ }).waitFor();
    await closeSheet();
    await more();
    say(`Transcript: reopened: ${await sec.locator(".session-choice.on").innerText().catch(() => "none")}`);
    await closeSheet();
  }

  section(`${tag}: the ring's card`);
  await page.locator(".composer .context-ring").tap();
  await page.locator(".context-ring-anchor.open .context-card").waitFor({ state: "visible" });
  const card = await box(".context-card");
  say(`a sheet at the foot, the screen's width: ${Math.abs(card.y + card.height - h) < 2 && card.width >= w - 1}; mode: ${await cardLine("mode-line")}`);
  await shoot(`${tag}-ring-card`);
  await page.locator(".context-backdrop").tap({ position: { x: 10, y: 10 }, force: true });
  await page.locator(".context-ring-anchor.open").waitFor({ state: "detached" });
  say(`a tap outside closes it: ${!(await page.locator(".context-card").isVisible())}`);

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
