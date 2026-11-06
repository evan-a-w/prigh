// Images too large for the model, and image files: on a desktop, a phone
// and a small phone (PART: desktop, phone or small).
//
// 1. A reply the model refused for an image too large: the error card
//    offers "Downsize images and retry", which runs /downsize-images (the
//    model's limit tightened, the reply sent again).
// 2. Image files picked from + → Files become images in the prompt, an
//    image with no type included (it is sniffed): drawable as images are.
// 3. One wider than the model takes is flagged on its tile; Downsize makes
//    it smaller in the page, and it is sent.
//
// Run by prigh_web.sh; prints normalised snapshots (the expected output is
// in ENGINE.expected) and, with SHOTS, screenshots.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN and
// optionally SHOTS and THEME.
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";
import { png } from "./png.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName, part = "desktop"] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
const viewports = {
  desktop: { width: 1280, height: 800 },
  phone: { width: 390, height: 844 },
  small: { width: 320, height: 640 },
};
const viewport = viewports[part];
if (!engine || !viewport) throw new Error("usage: images.mjs chromium|firefox|webkit desktop|phone|small");
const touch = part !== "desktop";
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, `images-${part}`);
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });

const refused = "HTTP 400: invalid_request_error: messages.0.content.0: image exceeds 5 MB maximum";
writeFileSync(
  join(dir, "script.json"),
  JSON.stringify([
    // Refused, then refused again for each of the backend's own two tries.
    { stop_reason: "error", error: refused },
    { stop_reason: "error", error: refused },
    { stop_reason: "error", error: refused },
    { text: "Got it, smaller this time." },
    { text: "Two images: a wide strip and a small square." },
  ]),
);

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
    "-model", "anthropic/claude-opus-4-6", "-token", token, "-cwd", cwd],
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

const section = label => console.log(`=== ${engineName}: images (${part}): ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  await page.waitForTimeout(300);
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-images-${part}-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const clean = text => text
  .replace(/\b\d+(m \d\d)?s\b/g, "<T>")
  .replace(/\b\d\d:\d\d\b/g, "<TIME>")
  .replace(/\$\d+\.\d+/g, "$<COST>")
  .split("\n").map(l => l.trimEnd()).filter(l => l !== "").join("\n");
const composer = page.locator(".composer textarea");
const chatHas = text => page.waitForFunction(t => document.getElementById("chat").innerText.includes(t), text);
const idle = () => page.waitForFunction(() => !document.querySelector(".streaming, .composer .btn.stop"));
const tap = locator => (touch ? locator.tap() : locator.click());
const send = async text => {
  await composer.fill(text);
  await tap(page.locator(".btn.send"));
};
// Each tile: its classes, its image's size, and the downsize button's text.
const tiles = async label => {
  await page.waitForFunction(() =>
    [...document.querySelectorAll(".pending-image img")].every(img => img.complete && img.naturalWidth > 0));
  section(label);
  const rows = await page.locator(".pending-image").evaluateAll(list => list.map(tile => {
    const img = tile.querySelector("img");
    const button = tile.querySelector(".downsize-image");
    const box = button?.getBoundingClientRect();
    return `${tile.className} ${img.naturalWidth}x${img.naturalHeight}` +
      (button ? ` [${button.innerText.replace(/\s+/g, " ").trim()}] ${box.height >= 32 ? "tappable" : `too small: ${box.height}px`}` : "");
  }));
  console.log(rows.join("\n") || "(none)");
};
// Whether anything sticks out of the page sideways.
const overflow = () => page.evaluate(() => document.documentElement.scrollWidth > window.innerWidth
  ? `overflows: ${document.documentElement.scrollWidth}px` : "fits the width");

const wide = png(9000, 40, x => (x % 600 < 300 ? [40, 80, 220] : [220, 40, 40]));
const square = png(48, 48, () => [30, 160, 90]);

try {
  await page.goto(url);
  await page.locator("form input[type=password]").fill(token);
  await tap(page.locator("form button[type=submit]"));
  await composer.waitFor();
  // THEME=light: the screenshots in the light theme (the output is the same).
  if (process.env.THEME) {
    await composer.fill(`/theme ${process.env.THEME}`);
    await composer.press("Enter");
    await page.waitForFunction(() => !document.querySelector(".composer textarea").value);
  }

  // 1. Refused: the card offers the way out.
  await send("hello");
  await page.locator(".stop.error .stop-action").waitFor();
  await idle();
  section("refused as too large: the error card");
  console.log(clean(await page.locator(".stop.error").last().innerText()));
  const action = await page.locator(".stop.error .stop-action").boundingBox();
  console.log(`button: ${action.height >= (touch ? 44 : 32) ? "big enough" : `${action.height}px`}; ${await overflow()}`);
  await screenshot("refused");
  await tap(page.locator(".stop.error .stop-action"));
  await chatHas("Got it, smaller this time.");
  await idle();
  section("downsized and retried");
  console.log((await page.locator(".toast").allInnerTexts()).map(clean).join("\n") || "(no toast)");
  console.log(clean(await page.locator(".msg.assistant").last().innerText()));
  await screenshot("retried");

  // 2. Image files from + → Files: images in the prompt (one with no type).
  await page.locator("#chat-attach-input").setInputFiles([
    { name: "strip.png", mimeType: "image/png", buffer: wide },
    { name: "square.png", mimeType: "", buffer: square },
  ]);
  await page.waitForFunction(() => document.querySelectorAll(".pending-image").length === 2);
  console.log(`uploaded as files: ${await page.locator(".attach-chip").count()}`);
  // The model's limit is now 2000 px (learned from the refusal).
  await tiles("image files attached: the wide one too large");
  console.log(await overflow());
  await screenshot("too large");

  // The one with no type is an image like any other: it can be drawn on.
  await tap(page.locator(".pending-image").nth(1).locator(".pending-thumb"));
  await page.locator(".annotate-dialog #annotate-canvas").waitFor();
  section("an image file opens in the drawing dialog");
  console.log(clean(await page.locator(".annotate-dialog .modal-buttons").innerText()));
  await page.keyboard.press("Escape");
  await page.waitForSelector(".annotate-dialog", { state: "detached" });

  // 3. Downsize: smaller in the page, then sent.
  await tap(page.locator(".downsize-image"));
  await page.waitForSelector(".pending-image.too-large", { state: "detached" });
  await tiles("downsized");
  section("the toast");
  console.log((await page.locator(".toast").allInnerTexts()).map(clean).filter(t => t.startsWith("Downsized")).join("\n")
    .replace(/[\d.]+ (B|KB|MB) → [\d.]+ (B|KB|MB)/, "<SIZE> → <SIZE>"));
  await screenshot("downsized");
  await send("what are these?");
  await chatHas("Two images: a wide strip and a small square.");
  await idle();
  section("sent");
  console.log(`composer cleared: ${(await page.locator(".pending-image").count()) === 0}`);
  const sizes = await page.locator(".msg.user").last().locator("img.thumb").evaluateAll(list =>
    list.map(img => `${img.naturalWidth}x${img.naturalHeight}`));
  console.log(sizes.join("\n"));
  await screenshot("sent");

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
