// The image viewer: a tall image, a wide one and a tiny one in a message,
// each shown whole (never cropped) on a desktop and a phone; zoom (the
// buttons, the keys, Ctrl+wheel, double-click) about a point and pan (drag,
// arrows) clamped to the image; ← → through the message's images; Esc
// closes and gives the thumbnail its focus back; Draw opens the drawing
// dialog, which fits the image too and zooms with drawing still under the
// pointer.
// Run by prigh_web.sh; prints normalised results (the expected output is in
// ENGINE.expected).
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
if (!engine) throw new Error("usage: viewer.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "viewer");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
const sessions = join(home, ".prigh", "sessions");
mkdirSync(sessions, { recursive: true });
mkdirSync(cwd, { recursive: true });
setTimeout(() => { console.error("viewer.mjs: timed out"); process.exit(2); }, 240000).unref();

const id = "1a9e1a9e1a9e1a9e";
const stripes = (w, h, n) =>
  png(w, h, (x, y) => (Math.floor((y / h) * n) % 2 ? [40, 150, 120] : [230, 140, 60]));
const tall = stripes(300, 1500, 10);
const wide = png(1500, 250, (x, y) => (Math.floor((x / 1500) * 10) % 2 ? [40, 90, 200] : [220, 60, 50]));
const tiny = png(12, 12, () => [200, 40, 160]);
const b64 = buf => ({ mime_type: "image/png", data: buf.toString("base64") });
{
  let seq = 0;
  let parent = null;
  let at = Math.floor(Date.now() / 1000) - 3600;
  const created = new Date(at * 1000).toISOString().replace("T", " ").replace(/\.(\d+)Z$/, ".$1000Z");
  const lines = [JSON.stringify(["Header", { id, cwd, created_at: created }])];
  const entry = payload => {
    const eid = `${id.slice(0, 8)}${String(seq++).padStart(8, "0")}`;
    lines.push(JSON.stringify(["Entry", { id: eid, parent, at: (at += 7), payload }]));
    parent = eid;
  };
  const usage = { input: 1200, output: 300, cache_read: 0 };
  entry(["Name", { name: "three images" }]);
  entry(["Message", ["User", { text: "Look at these.", images: [b64(tall), b64(wide), b64(tiny)] }]]);
  entry(["Message", ["Assistant", { content: [["Text", "Stripes, both ways, and a dot."]], stop_reason: ["End_turn"], usage, model: "faux" }]]);
  writeFileSync(join(sessions, `20261001-100000000_${id}.jsonl`), lines.join("\n") + "\n");
}
writeFileSync(join(dir, "script.json"), "[]");

let server;
const startServer = () =>
  new Promise((resolve, reject) => {
    server = spawn(
      backend,
      ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
        "-token", token, "-cwd", cwd],
      { env: { ...process.env, HOME: home }, stdio: ["ignore", "ignore", "pipe"] },
    );
    let err = "";
    const timer = setTimeout(() => reject(new Error(`backend did not start:\n${err}`)), 30000);
    server.stderr.on("data", data => {
      err += data;
      const m = err.match(/^prigh: prigh-web on (\S+)$/m);
      if (m) { clearTimeout(timer); resolve(m[1]); }
    });
    server.on("exit", code => { clearTimeout(timer); reject(new Error(`backend exited (${code}):\n${err}`)); });
  });
const stopServer = () =>
  new Promise(resolve => {
    server.removeAllListeners("exit");
    if (server.exitCode !== null || server.signalCode !== null) return resolve();
    const timer = setTimeout(() => { server.kill("SIGKILL"); }, 5000);
    server.on("exit", () => { clearTimeout(timer); resolve(); });
    server.kill();
  });

const url = await startServer();
const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
const errors = [];
let page;
const section = label => console.log(`=== ${engineName}: viewer: ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-viewer-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};

// The image's box against the window's: whole and inside, or how it is cut.
const placed = () => page.evaluate(() => {
  const r = document.querySelector("dialog.viewer .viewer-img").getBoundingClientRect();
  const s = document.querySelector("dialog.viewer .viewer-stage").getBoundingClientRect();
  const bar = document.querySelector("dialog.viewer .viewer-bar").getBoundingClientRect();
  const inside = r.left >= -0.5 && r.top >= -0.5 && r.right <= innerWidth + 0.5 && r.bottom <= innerHeight + 0.5;
  const clear = r.top >= bar.bottom - 0.5 || r.right <= bar.left + 0.5 || r.left >= bar.right - 0.5;
  const covers = r.left <= s.left + 0.5 && r.right >= s.right - 0.5 || r.top <= s.top + 0.5 && r.bottom >= s.bottom - 0.5;
  return { r, s, inside, clear, covers, w: Math.round(r.width), h: Math.round(r.height) };
});
const pct = () => page.locator("dialog.viewer .viewer-pct").innerText();
// Until the image stops moving (its transitions and any pending zoom frames
// are done), not a fixed wait: a loaded machine can take longer than any.
const waitSettled = async () => {
  await page.evaluate(() => { delete window.__settleKey; });
  await page.waitForTimeout(100);
  await settled();
};
const settled = () => page.waitForFunction(() => {
  const img = document.querySelector("dialog.viewer img");
  if (!img) return true;
  // A loaded machine can stall a transition's frames: a rect that has not
  // moved for a while can still be mid-way.
  if (img.getAnimations().some(a => a.playState === "running")) return false;
  const r = img.getBoundingClientRect();
  const key = [r.left, r.top, r.width, r.height].join(",");
  const w = window;
  const now = performance.now();
  if (w.__settleKey !== key) { w.__settleKey = key; w.__settleAt = now; return false; }
  return now - w.__settleAt >= 150;
}, null, { polling: 50, timeout: 10000 });
const describe = async () => {
  const p = await placed();
  const name = await page.locator("dialog.viewer .viewer-count").innerText();
  return `${name || "1 of 1"}: ${await pct()}; whole and inside the window: ${p.inside}; clear of the toolbar: ${p.clear}`;
};

let sessionUrl;
const visit = async (viewport, tag) => {
  const context = await browser.newContext({ viewport, hasTouch: tag === "phone", isMobile: tag === "phone" });
  context.setDefaultTimeout(20000);
  page = await context.newPage();
  page.on("console", m => { if (m.type() === "error" && !/favicon\.ico/.test(m.text())) errors.push(`console: ${m.text()}`); });
  page.on("pageerror", e => errors.push(`page: ${e.message}`));
  if (sessionUrl) {
    await page.goto(sessionUrl);
    await page.waitForSelector("form input[type=password]");
  } else await page.goto(url);
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await page.locator(".composer textarea").waitFor();
  if (!sessionUrl) {
    await page.locator(`[data-session="${id}"]`).click();
    await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("Stripes"));
    sessionUrl = page.url();
  }
  const thumbs = page.locator(".msg.user button.image[data-viewer]");
  await thumbs.first().waitFor();
  return { context, thumbs };
};

try {
  // 1. A desktop: each image whole, then zoom and pan the tall one.
  {
    const { context, thumbs } = await visit({ width: 1280, height: 800 }, "desktop");
    section("desktop: fit");
    console.log(`thumbnails: ${await thumbs.count()}, named: ${await thumbs.first().getAttribute("aria-label")}`);
    await thumbs.first().click();
    await page.locator("dialog.viewer[open] .viewer-img").waitFor();
    await page.waitForFunction(() => document.querySelector("dialog.viewer .viewer-img").naturalWidth > 0);
    await waitSettled();
    console.log(`focused: ${await page.evaluate(() => document.activeElement?.getAttribute("aria-label"))}`);
    console.log(await describe());
    await screenshot("tall fit");
    await page.keyboard.press("ArrowRight");
    await waitSettled();
    console.log(await describe());
    await page.keyboard.press("ArrowRight");
    await waitSettled();
    console.log(`${await describe()}; tiny shown larger: ${(await placed()).w > 12}`);
    await page.keyboard.press("ArrowLeft");
    await page.keyboard.press("ArrowLeft");
    await waitSettled();

    section("desktop: zoom and pan");
    await page.keyboard.press("1");
    await waitSettled();
    let p = await placed();
    console.log(`1: ${await pct()}; ${p.w}x${p.h}; fills the stage's height: ${p.covers}`);
    // Pan down with the keys, then drag back up; it can't leave the image.
    const top0 = p.r.top;
    for (let i = 0; i < 4; i++) await page.keyboard.press("ArrowDown");
    await waitSettled();
    p = await placed();
    console.log(`arrows down: moved up by the image: ${p.r.top < top0}`);
    const s = p.s;
    // Drags inside the window (a browser need not send moves outside it).
    const drag = async dy => {
      for (let i = 0; i < 4; i++) {
        const y0 = dy > 0 ? s.y + 20 : s.y + s.height - 20;
        await page.mouse.move(s.x + s.width / 2, y0);
        await page.mouse.down();
        await page.mouse.move(s.x + s.width / 2, y0 + dy, { steps: 8 });
        await page.mouse.up();
        await page.waitForTimeout(400); // not a double-click
      }
    };
    await drag(s.height - 40);
    await waitSettled();
    p = await placed();
    console.log(`dragged far down: stops with its top at the stage's margin: ${Math.abs(p.r.top - p.s.top - 24) < 1}`);
    await drag(-(s.height - 40));
    await waitSettled();
    p = await placed();
    console.log(`dragged far up: stops with its bottom at the stage's margin: ${Math.abs(p.s.bottom - 24 - p.r.bottom) < 1}`);
    await screenshot("tall panned");
    await page.keyboard.press("0");
    await waitSettled();
    console.log(`0: ${await describe()}`);
    // Zoom in about a point: what is under the pointer stays there.
    const fit = await placed();
    const px = fit.r.left + fit.r.width * 0.5, py = fit.r.top + fit.r.height * 0.25;
    const under = { u: (px - fit.r.left) / fit.r.width, v: (py - fit.r.top) / fit.r.height };
    await page.mouse.move(px, py);
    for (let i = 0; i < 2; i++) {
      await page.keyboard.down("Control");
      await page.mouse.wheel(0, -15);
      await page.keyboard.up("Control");
    }
    // mouse.wheel returns before the page has handled the event: on a busy
    // machine the steps land later. Wait for both (each scales by
    // exp(15 × 0.01)) before measuring.
    const zoomed = fit.w * Math.exp(2 * 15 * 0.01);
    await page.waitForFunction(w => {
      const img = document.querySelector("dialog.viewer img");
      return img && Math.abs(img.getBoundingClientRect().width - w) < 2;
    }, zoomed, { polling: 50, timeout: 10000 }).catch(() => {});
    await waitSettled();
    p = await placed();
    const still = Math.abs(p.r.left + under.u * p.r.width - px) < 2 && Math.abs(p.r.top + under.v * p.r.height - py) < 2;
    console.log(`ctrl+wheel: zoomed in: ${p.w > fit.w}; the point under the pointer stays: ${still}`);
    await page.locator('dialog.viewer [data-v="in"]').click();
    await waitSettled();
    console.log(`zoom in button: larger: ${(await placed()).w > p.w}`);
    await page.mouse.dblclick(px, py);
    await waitSettled();
    console.log(`double-click: back to fit: ${(await placed()).w === fit.w}`);
    await page.mouse.dblclick(px, py);
    await waitSettled();
    console.log(`double-click: ${await pct()}`);
    for (let i = 0; i < 40; i++) await page.keyboard.press("+");
    await waitSettled();
    console.log(`+ many times: at most ${await pct()}`);
    for (let i = 0; i < 40; i++) await page.keyboard.press("-");
    await waitSettled();
    console.log(`- many times: ${await describe()}`);

    section("desktop: draw and close");
    await page.locator("dialog.viewer .viewer-draw").click();
    await page.locator(".annotate-dialog #annotate-canvas").waitFor();
    await page.waitForFunction(() => document.querySelector("#annotate-canvas").width > 0);
    await page.waitForTimeout(300);
    const frame = await page.evaluate(() => {
      const r = document.querySelector(".annotate-frame").getBoundingClientRect();
      const s = document.querySelector(".annotate-stage").getBoundingClientRect();
      return r.top >= s.top - 0.5 && r.bottom <= s.bottom + 0.5 && r.left >= s.left - 0.5 && r.right <= s.right + 0.5;
    });
    console.log(`viewer closed: ${!(await page.locator("dialog.viewer").count())}; the drawing's image whole: ${frame}`);
    await page.locator('.annotate-dialog [data-annotate-zoom="in"]').click();
    await page.locator('.annotate-dialog [data-annotate-zoom="in"]').click();
    await page.waitForTimeout(300);
    const c = await page.locator("#annotate-canvas").boundingBox();
    const st = await page.locator(".annotate-stage").boundingBox();
    const cx = st.x + st.width / 2, cy = st.y + st.height / 2;
    await page.mouse.move(cx, cy);
    await page.mouse.down();
    await page.mouse.move(cx + 30, cy + 10, { steps: 4 });
    await page.mouse.up();
    // The stroke's first pixel is where the pointer went down.
    const hit = await page.evaluate(({ cx, cy }) => {
      const canvas = document.querySelector("#annotate-canvas");
      const r = canvas.getBoundingClientRect();
      const x = Math.round(((cx - r.left) / r.width) * canvas.width);
      const y = Math.round(((cy - r.top) / r.height) * canvas.height);
      return canvas.getContext("2d").getImageData(x, y, 1, 1).data[3] > 0;
    }, { cx, cy });
    console.log(`zoomed: ${c.width > 0 && (await page.locator(".annotate-zoom-pct").innerText()) !== ""}; the stroke under the pointer: ${hit}`);
    await screenshot("drawing zoomed");
    await page.keyboard.press("Escape");
    await page.locator(".annotate-dialog").waitFor({ state: "detached" });
    await thumbs.nth(1).click();
    await page.locator("dialog.viewer[open]").waitFor();
    await page.keyboard.press("Escape");
    await page.locator("dialog.viewer").waitFor({ state: "detached" });
    console.log(`Esc: closed; focus back on: ${await page.evaluate(() => document.activeElement?.getAttribute("aria-label"))}`);
    await context.close();
  }

  // 2. A phone: whole, and pinch-free zoom by double-tap and the buttons.
  {
    const { context, thumbs } = await visit({ width: 390, height: 844 }, "phone");
    section("phone");
    await thumbs.first().tap();
    await page.locator("dialog.viewer[open] .viewer-img").waitFor();
    await page.waitForFunction(() => document.querySelector("dialog.viewer .viewer-img").naturalWidth > 0);
    await waitSettled();
    console.log(await describe());
    const bar = await page.evaluate(() => {
      const b = document.querySelector("dialog.viewer .viewer-tools").getBoundingClientRect();
      return b.left >= 0 && b.right <= innerWidth;
    });
    console.log(`toolbar inside the screen: ${bar}`);
    await screenshot("phone tall fit");
    console.log(`size buttons hidden: ${!(await page.locator('dialog.viewer [data-v="actual"]').isVisible())}`);
    await page.locator('dialog.viewer [data-v="in"]').tap();
    await waitSettled();
    console.log(`zoom in: ${await pct()}`);
    // A double-tap: back to fit, and again to 100%.
    const tapAt = async () => {
      const r = (await placed()).s;
      await page.touchscreen.tap(r.x + r.width / 2, r.y + r.height / 2);
      await page.waitForTimeout(60);
      await page.touchscreen.tap(r.x + r.width / 2, r.y + r.height / 2);
      await waitSettled();
    };
    await tapAt();
    console.log(`double-tap: ${await describe()}`);
    await tapAt();
    console.log(`double-tap: ${await pct()}`);
    await page.keyboard.press("0");
    await waitSettled();
    await page.locator('dialog.viewer [data-v="next"]').tap();
    await waitSettled();
    console.log(await describe());
    await page.locator('dialog.viewer [data-v="close"]').tap();
    await page.locator("dialog.viewer").waitFor({ state: "detached" });
    console.log("closed");
    await context.close();
  }

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  await screenshot("failure").catch(() => {});
  console.error(errors.join("\n"));
  throw error;
} finally {
  await end(() => stopServer(), () => browser.close());
}
