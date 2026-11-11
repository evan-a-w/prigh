// Computer use: the agent driving the user's screen, on a desktop, a phone
// and a small phone (PART: desktop, phone or small).
//
// 1. A session where it clicked and typed: each card says what it did and
//    shows the screen after it, the point it clicked marked where it is on
//    the screenshot; tapping the screenshot opens the viewer.
// 2. A run that calls computer: it asks first (a dialog, a sheet from the
//    bottom on a phone); allowed, a strip over the composer says prigh is
//    controlling the computer, with Stop, which aborts the run (and the
//    strip goes).
// 3. The next run asks again (on Chromium's phone, as an installed app on
//    a notched phone: the safe area's insets emulated); denied with Esc or
//    Deny, the model is told and no strip shows.
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
if (!engine || !viewport) throw new Error("usage: computer.mjs chromium|firefox|webkit desktop|phone|small");
const touch = part !== "desktop";
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, `computer-${part}`);
const home = join(dir, "home");
const cwd = join(dir, "cwd");
const sessions = join(home, ".prigh", "sessions");
mkdirSync(sessions, { recursive: true });
mkdirSync(cwd, { recursive: true });
setTimeout(() => { console.error("computer.mjs: timed out"); process.exit(2); }, 240000).unref();

// A screen (scaled to 1280x800): a desktop, a window, and in it a button
// whose centre is (640, 312): where the agent clicks.
const screen = (typed = 0) => png(1280, 800, (x, y) => {
  if (x >= 560 && x < 720 && y >= 292 && y < 332) return [214, 120, 40];
  if (y >= 380 && y < 410 && x >= 400 && x < 400 + typed * 14) return [230, 230, 230];
  if (x >= 300 && x < 980 && y >= 160 && y < 560) return y < 190 ? [70, 76, 90] : [42, 46, 56];
  return [24 + Math.floor(y / 40), 60, 90];
});
const b64 = buf => ({ mime_type: "image/png", data: buf.toString("base64") });

const id = "c0c0c0c0c0c0c0c0";
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
  const call = (cid, args) => ["Assistant", {
    content: [["Tool_call", { id: cid, name: "computer", arguments: JSON.stringify(args) }]],
    stop_reason: ["Tool_use"], usage, model: "faux",
  }];
  const result = (cid, text, image) => ["Tool_result", {
    tool_call_id: cid, tool_name: "computer", text, is_error: false, images: [b64(image)],
  }];
  entry(["Name", { name: "fill the form" }]);
  entry(["Message", ["User", { text: "Press Start and type hello." }]]);
  entry(["Message", call("k1", { action: "click", x: 640, y: 312 })]);
  entry(["Message", result("k1", "click (640, 312) → (1280, 624) on the screen\nscreenshot 1280×800", screen())]);
  entry(["Message", call("k2", { action: "type", text: "hello" })]);
  entry(["Message", result("k2", "typed 5 characters\nscreenshot 1280×800", screen(5))]);
  entry(["Message", ["Assistant", { content: [["Text", "Pressed Start and typed **hello**."]], stop_reason: ["End_turn"], usage, model: "faux" }]]);
  writeFileSync(join(sessions, `20261001-100000000_${id}.jsonl`), lines.join("\n") + "\n");
}

// A run that drives the screen, then waits (for the gate, which only
// teardown opens): Stop ends it.
const waitFor = file => `timeout 300 sh -c 'until [ -f ${file} ]; do sleep 0.1; done'; echo ${file}`;
writeFileSync(
  join(dir, "script.json"),
  JSON.stringify([
    {
      text: "Opening a tab.",
      tool_calls: [
        { id: "k3", name: "computer", arguments: { action: "key", combo: "ctrl+t" } },
        { id: "w1", name: "bash", arguments: { command: waitFor("gate") } },
      ],
    },
    // The aborted run asks for nothing more: the next prompt's.
    {
      text: "Looking again.",
      tool_calls: [{ id: "k4", name: "computer", arguments: { action: "screenshot" } }],
    },
    { text: "Understood: no computer use." },
  ]),
);

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
const context = await browser.newContext({
  viewport,
  hasTouch: touch,
  ...(touch && engineName !== "firefox" ? { isMobile: true } : {}),
});
context.setDefaultTimeout(20000);
const page = await context.newPage();
page.on("console", message => {
  if (message.type() === "error" && !/favicon\.ico|interactive-widget|WebSocket/.test(message.text())) errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));
// What Stop sends: the backend's abort.
let sawAbort;
const aborted = new Promise(resolve => { sawAbort = resolve; });
page.on("websocket", ws => ws.on("framesent", frame => {
  if (typeof frame.payload === "string" && /"method":"abort"/.test(frame.payload)) sawAbort();
}));

const section = label => console.log(`=== ${engineName}: computer (${part}): ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  await page.waitForTimeout(300);
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-computer-${part}-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const tap = locator => (touch ? locator.tap() : locator.click());
const composer = page.locator(".composer textarea");
const overflow = () => page.evaluate(() => document.documentElement.scrollWidth > window.innerWidth
  ? `overflows: ${document.documentElement.scrollWidth}px` : "fits the width");
// Whether [selector]'s box is whole in the window and nothing covers its
// middle.
const inView = selector => page.evaluate(sel => {
  const el = document.querySelector(sel);
  if (!el) return "missing";
  const r = el.getBoundingClientRect();
  if (r.left < 0 || r.top < 0 || r.right > window.innerWidth + 0.5 || r.bottom > window.innerHeight + 0.5)
    return `off screen: ${Math.round(r.left)},${Math.round(r.top)} ${Math.round(r.width)}x${Math.round(r.height)}`;
  const hit = document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2);
  return el.contains(hit) ? "in view" : `covered by ${hit?.className}`;
}, selector);

try {
  await page.goto(`${url.replace(/\/$/, "")}/?session=${id}`);
  await page.locator("form input[type=password]").fill(token);
  await tap(page.locator("form button[type=submit]"));
  await composer.waitFor();
  await page.waitForFunction(() => document.getElementById("chat")?.innerText.includes("typed hello"));

  // 1. The cards.
  await page.waitForFunction(() =>
    [...document.querySelectorAll(".tool-computer img.thumb")].every(img => img.complete && img.naturalWidth > 0)
    && document.querySelectorAll(".tool-computer img.thumb").length === 2);
  section("cards");
  const cards = await page.locator(".tool-computer").evaluateAll(list => list.map(card => {
    const head = card.querySelector(".tool-head").innerText.replace(/\s+/g, " ").trim();
    const img = card.querySelector("img.thumb");
    const ir = img.getBoundingClientRect();
    const mark = card.querySelector(".screen-mark");
    let where = "no mark";
    if (mark) {
      const m = mark.getBoundingClientRect();
      // The mark's centre, in the screenshot's own pixels.
      const x = Math.round(((m.left + m.width / 2 - ir.left) / ir.width) * img.naturalWidth);
      const y = Math.round(((m.top + m.height / 2 - ir.top) / ir.height) * img.naturalHeight);
      where = `mark at ${Math.round(x / 4) * 4},${Math.round(y / 4) * 4}`;
    }
    const caption = card.querySelector(".caption")?.innerText.replace(/\s+/g, " ").trim();
    return `${head} | ${img.naturalWidth}x${img.naturalHeight}, ${ir.width <= window.innerWidth ? "fits" : "too wide"} | ${where} | ${caption}`;
  }));
  console.log(cards.join("\n"));
  console.log(await overflow());
  await page.locator(".tool-computer").first().scrollIntoViewIfNeeded();
  await screenshot("cards");

  section("the screenshot in the viewer");
  await tap(page.locator(".tool-computer button.screen-shot").first());
  await page.locator("dialog.viewer[open] .viewer-img").waitFor();
  await page.waitForFunction(() => document.querySelector("dialog.viewer .viewer-img").naturalWidth > 0);
  console.log(`opened: ${await page.locator("dialog.viewer .viewer-img").evaluate(img => `${img.naturalWidth}x${img.naturalHeight}`)}`);
  await screenshot("viewer");
  await page.keyboard.press("Escape");
  await page.waitForSelector("dialog.viewer[open]", { state: "detached" });
  console.log("closed");

  // 2. A run that drives the screen: it asks first.
  await composer.fill("open a tab");
  await tap(page.locator(".btn.send"));
  const consent = page.locator(".modal.computer-consent");
  await consent.waitFor();
  section("asks first");
  const consentHost = await page.evaluate(() =>
    document.querySelector(".computer-consent .confirm-question").innerText.match(/control (.*)'s screen/)[1]);
  const consentText = async () =>
    (await consent.innerText()).replaceAll(consentHost, "<HOST>").replace(/\s+/g, " ").trim();
  // Each answer: whole on the screen, uncovered, and how tall.
  const answers = async () => {
    for (const cls of ["allow", "deny", "reason"]) {
      const sel = `.computer-consent .confirm-answer.${cls}`;
      console.log(`${cls}: ${await inView(sel)}, ${Math.round((await page.locator(sel).boundingBox()).height) >= (touch ? 44 : 28) ? "tall enough" : "too small"}`);
    }
    // On a phone a sheet from the bottom: its foot at the screen's.
    const box = await consent.boundingBox();
    console.log(`at the bottom: ${Math.abs(box.y + box.height - viewport.height) < 1}`);
  };
  console.log(await consentText());
  console.log(`the host is the composer's: ${(await page.locator(".composer").innerText()).includes(consentHost)}`);
  console.log(`banner while asking: ${await page.locator(".computer-banner").count()}`);
  await answers();
  console.log(await overflow());
  await screenshot("asks first");
  await tap(page.locator(".computer-consent .confirm-answer.allow"));
  await consent.waitFor({ state: "detached" });
  await page.locator(".computer-banner").waitFor();
  await page.waitForFunction(() => document.getElementById("chat").innerText.includes("Opening a tab."));
  section("controlling");
  // The host is the machine's own (prigh web's tool host): its name varies.
  const host = await page.locator(".computer-banner strong").innerText();
  console.log((await page.locator(".computer-banner").innerText()).replace(host, "<HOST>").replace(/\s+/g, " ").trim());
  console.log(`the host is the composer's: ${(await page.locator(".composer").innerText()).includes(host)}`);
  console.log(`Stop: ${await inView(".computer-banner-stop")}, ${Math.round((await page.locator(".computer-banner-stop").boundingBox()).height)}px tall`);
  console.log(await overflow());
  await screenshot("controlling");
  await tap(page.locator(".computer-banner-stop"));
  await aborted;
  console.log("Stop: abort sent");
  // The run ends (the command it waits on let go, as Stop may leave a
  // command on a tool host running): the strip goes.
  writeFileSync(join(cwd, "gate"), "");
  await page.locator(".computer-banner").waitFor({ state: "detached" });
  await page.waitForFunction(() => !document.querySelector(".composer .btn.stop"));
  section("stopped");
  console.log(`banner: ${await page.locator(".computer-banner").count()}`);
  await screenshot("stopped");

  // 3. The next run asks again; denied.
  if (part === "phone" && engineName === "chromium") {
    const cdp = await context.newCDPSession(page);
    await cdp.send("Emulation.setSafeAreaInsetsOverride", { insets: { top: 47, bottom: 34, left: 0, right: 0 } });
  }
  await composer.fill("look again");
  await tap(page.locator(".btn.send"));
  await consent.waitFor();
  section("asks again");
  console.log(await consentText());
  await answers();
  if (part === "phone" && engineName === "chromium") {
    // Over the home indicator: the answers end above the inset.
    const bottom = await page.locator(".computer-consent .confirm-answer.reason").evaluate(el => el.getBoundingClientRect().bottom);
    console.log(`clear of the home indicator: ${bottom <= viewport.height - 34}`);
    await screenshot("asks again pwa");
  } else {
    console.log("clear of the home indicator: true");
    await screenshot("asks again");
  }
  if (touch) await tap(page.locator(".computer-consent .confirm-answer.deny"));
  else await page.keyboard.press("Escape");
  await consent.waitFor({ state: "detached" });
  await page.waitForFunction(() => document.getElementById("chat").innerText.includes("Understood: no computer use."));
  section("denied");
  const denied = await page.locator(".tool-computer").last().innerText();
  console.log(`the model was told: ${/declined/.test(denied) || /did not allow/.test(denied)}`);
  console.log(`banner: ${await page.locator(".computer-banner").count()}`);
  await screenshot("denied");

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  await screenshot("failure");
  console.error(errors.join("\n"));
  throw error;
} finally {
  writeFileSync(join(cwd, "gate"), "");
  await end(() => server.kill(), () => browser.close());
}
