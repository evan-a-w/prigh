// Long pastes into the composer: on a desktop, a phone and a small phone
// (PART: desktop, phone or small).
//
// 1. A paste of 420 lines never reaches the box (its default is prevented):
//    a Pasted text chip instead, opening a dialog with the text.
// 2. Ctrl+Shift+V pastes the same text into the box as text. A chip removed
//    with its × comes back with Ctrl+Z; Ctrl+Shift+Z removes it again.
// 3. Sent with a question: the message shows the question and a folded
//    chip; opened, the text; after a reload, the chip again.
//
// Run by prigh_web.sh; prints normalised snapshots (the expected output is
// in ENGINE.expected) and, with SHOTS, screenshots.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName, part = "desktop"] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
const viewports = {
  desktop: { width: 1280, height: 800 },
  phone: { width: 390, height: 844 },
  small: { width: 320, height: 640 },
};
const viewport = viewports[part];
if (!engine || !viewport) throw new Error("usage: pasted.mjs chromium|firefox|webkit desktop|phone|small");
const touch = part !== "desktop";
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, `pasted-${part}`);
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });
writeFileSync(join(dir, "script.json"), JSON.stringify([{ text: "Line 3 divides by zero." }]));

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
const page = await (await browser.newContext({
  viewport,
  hasTouch: touch,
  ...(touch && engineName !== "firefox" ? { isMobile: true } : {}),
})).newPage();
page.on("console", message => {
  if (message.type() === "error" && !/favicon\.ico|interactive-widget|WebSocket/.test(message.text())) errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

const section = label => console.log(`=== ${engineName}: pasted (${part}): ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  await page.waitForTimeout(300);
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-pasted-${part}-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const clean = text => text
  .replace(/\b\d\d:\d\d\b/g, "<TIME>")
  .replace(/\$\d+\.\d+/g, "$<COST>")
  .split("\n").map(l => l.trimEnd()).filter(l => l !== "").join("\n");
const composer = page.locator(".composer textarea");
const idle = () => page.waitForFunction(() => !document.querySelector(".streaming, .composer .btn.stop"));
const tap = locator => (touch ? locator.tap() : locator.click());
const overflow = () => page.evaluate(() => document.documentElement.scrollWidth > window.innerWidth
  ? `overflows: ${document.documentElement.scrollWidth}px` : "fits the width");
// A paste into the box as the browser makes it; [plain]: after the keydown
// of Ctrl+Shift+V. Says whether the page kept it from the box.
const paste = (text, plain = false) => page.evaluate(([text, plain]) => {
  const box = document.querySelector(".composer textarea");
  box.focus();
  const key = type => {
    const event = new KeyboardEvent(type, { key: "V", code: "KeyV", ctrlKey: true, shiftKey: true, bubbles: true });
    // Only Chromium takes [keyCode] from the init.
    Object.defineProperty(event, "keyCode", { value: 86 });
    box.dispatchEvent(event);
  };
  if (plain) key("keydown");
  const data = new DataTransfer();
  data.setData("text/plain", text);
  const event = new ClipboardEvent("paste", { bubbles: true, cancelable: true });
  // Firefox ignores [clipboardData] in a script's ClipboardEvent.
  Object.defineProperty(event, "clipboardData", { value: data });
  box.dispatchEvent(event);
  if (plain) key("keyup");
  // What the browser does with a paste no one prevented.
  if (!event.defaultPrevented) box.setRangeText(text, box.selectionStart, box.selectionEnd, "end");
  box.dispatchEvent(new Event("input", { bubbles: true }));
  return event.defaultPrevented ? "kept from the box" : "into the box";
}, [text, plain]);
// Within the screen, and big enough for a finger.
const fits = async selector => {
  const box = await page.locator(selector).first().boundingBox();
  const inside = box.x >= 0 && box.x + box.width <= viewport.width + 0.5;
  return `${inside ? "on screen" : `off screen: x ${box.x} w ${box.width}`}` +
    (touch ? `, ${box.height >= 44 || selector.includes("remove") ? "tappable" : `${box.height}px high`}` : "");
};

const log = Array.from({ length: 420 }, (_, i) =>
  i === 2 ? "ZeroDivisionError: division by zero" : `  at frame ${i} (worker.py:${i + 10})`).join("\n");

try {
  await page.goto(url);
  await page.locator("form input[type=password]").fill(token);
  await tap(page.locator("form button[type=submit]"));
  await composer.waitFor();

  // 1. Long: a chip, not in the box.
  await composer.fill("why does this fail?");
  section("a long paste");
  console.log(await paste(log));
  await page.locator(".pasted-chip").waitFor();
  console.log(`box: ${JSON.stringify(await composer.inputValue())}`);
  console.log(clean(await page.locator(".pasted-chips").innerText()));
  console.log(`chip: ${await fits(".pasted-chip")}; remove: ${await fits(".pasted-chip .attach-chip-remove")}; ${await overflow()}`);
  await screenshot("chip");
  await tap(page.locator(".pasted-chip-open"));
  await page.locator(".pasted-dialog").waitFor();
  section("its dialog");
  console.log(clean(await page.locator(".pasted-dialog .modal-head").first().innerText()));
  console.log(clean(await page.locator(".pasted-dialog .modal-buttons").innerText()));
  console.log(`lines shown: ${(await page.locator(".pasted-dialog .text-block").innerText()).split("\n").length}`);
  await screenshot("dialog");
  await page.keyboard.press("Escape");
  await page.waitForSelector(".pasted-dialog", { state: "detached" });

  // 2. Ctrl+Shift+V: as text, no chip.
  section("Ctrl+Shift+V");
  const before = await page.locator(".pasted-chip").count();
  console.log(await paste("\n" + log.split("\n").slice(0, 50).join("\n"), true));
  await page.waitForFunction(() => document.querySelector(".composer textarea").value.includes("frame 49"));
  console.log(`chips: ${before} → ${await page.locator(".pasted-chip").count()}; box lines: ${(await composer.inputValue()).split("\n").length}`);
  // Its keyup ends it, also when no paste came (the terminal reads the
  // clipboard itself): the next paste (from a menu, no keys) is a chip.
  await page.evaluate(() => {
    for (const type of ["keydown", "keyup"]) {
      const event = new KeyboardEvent(type, { key: "V", code: "KeyV", ctrlKey: true, shiftKey: true, bubbles: true });
      Object.defineProperty(event, "keyCode", { value: 86 });
      document.body.dispatchEvent(event);
    }
  });
  console.log(`Ctrl+Shift+V without a paste, then a paste from a menu: ${await paste(log)}`);
  await page.locator(".pasted-chip").nth(1).waitFor();
  console.log(clean(await page.locator(".pasted-chips").innerText()));
  await tap(page.locator(".pasted-chip").nth(1).locator(".attach-chip-remove"));
  await page.waitForFunction(() => document.querySelectorAll(".pasted-chip").length === 1);

  // 2b. Ctrl+Z brings the removed chip back, Ctrl+Shift+Z takes it out again.
  section("Ctrl+Z after removing a chip");
  const chipCount = n => page.waitForFunction(n => document.querySelectorAll(".pasted-chip").length === n, n);
  await composer.focus();
  await page.keyboard.press("Control+z");
  await chipCount(2);
  console.log(`undone: ${await page.locator(".pasted-chip").count()} chips; box lines: ${(await composer.inputValue()).split("\n").length}`);
  await screenshot("undone");
  await page.keyboard.press("Control+Shift+z");
  await chipCount(1);
  console.log(`redone: ${await page.locator(".pasted-chip").count()} chip`);
  await composer.fill("why does this fail?");

  // 3. Sent: the question and a folded chip.
  await tap(page.locator(".btn.send"));
  await page.locator(".msg.user .pasted").waitFor();
  await page.waitForFunction(() => document.getElementById("chat").innerText.includes("divides by zero"));
  await idle();
  section("sent");
  console.log(`chips left: ${await page.locator(".pasted-chip").count()}`);
  console.log(clean(await page.locator(".msg.user").last().innerText()));
  console.log(`text in the page: ${(await page.locator(".msg.user .pasted-text").count()) > 0}; ${await overflow()}`);
  console.log(`head: ${await fits(".msg.user .pasted-head")}`);
  await screenshot("sent");
  await tap(page.locator(".msg.user .pasted-head"));
  await page.locator(".msg.user .pasted-text").waitFor();
  section("opened");
  const shown = await page.locator(".msg.user .pasted-text").innerText();
  console.log(`the whole text: ${shown.trim() === log.trim()}; ${await overflow()}`);
  await screenshot("opened");

  // After a reload: from the session, folded again.
  await page.reload();
  await page.locator(".msg.user .pasted").waitFor();
  section("reloaded");
  console.log(clean(await page.locator(".msg.user").last().innerText()));
  await screenshot("reloaded");

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
