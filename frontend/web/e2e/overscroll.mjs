// The transcript's end while a reply streams into a long chat (a subagent's
// row coming meanwhile). Scrolling on past the end (the wheel, a swipe), the
// chat stays at the end, no rows overlap, no message shows twice, the jump
// button never shows there, and the page sets scrollTop only when the chat
// is off its end: a write there changes nothing but cancels what the browser
// is doing with the user's scroll (the elastic overscroll, a smooth scroll),
// and flickered at every chunk. A turn up the chat does not take (the
// overscroll giving way, which scrolls nothing) leaves it following, with no
// jump button; a real turn up stops following and shows it. On a desktop and
// two phones (by touch where the browser makes touches).
// Run by prigh_web.sh; prints normalised results (the expected output is in
// ENGINE.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";
import { normalVerbosity } from "./verbose_prefs.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName, part] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
const viewports = {
  desktop: { width: 1280, height: 800 },
  phone: { width: 390, height: 844 },
  small: { width: 320, height: 640 },
};
const viewport = viewports[part];
if (!engine || !viewport) throw new Error("usage: overscroll.mjs chromium|firefox|webkit desktop|phone|small");
const touch = part !== "desktop";
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, `overscroll-${part}`);
const home = join(dir, "home");
normalVerbosity(home);
const cwd = join(dir, "cwd");
const sessions = join(home, ".prigh", "sessions");
mkdirSync(sessions, { recursive: true });
mkdirSync(cwd, { recursive: true });
setTimeout(() => { console.error("overscroll.mjs: timed out"); process.exit(2); }, 240000).unref();

// A long chat: 12 turns.
const id = "7b7e7b7e7b7e7b7e";
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
  entry(["Name", { name: "a long chat" }]);
  for (let t = 1; t <= 12; t++) {
    entry(["Message", ["User", { text: `Step ${t}: look at module ${t}.` }]]);
    entry(["Message", ["Assistant", {
      content: [["Text", `Module ${t} is fixed.\n\n- Three browser test scripts pass in Chromium, and I checked their screenshots.\n- No test opens a subagent's chat on a phone yet.`]],
      stop_reason: ["End_turn"], usage, model: "faux",
    }]]);
  }
  writeFileSync(join(sessions, `20261001-100000000_${id}.jsonl`), lines.join("\n") + "\n");
}
// Replies of 24 points, streamed in 120 chunks (a few seconds).
const points = what => Array.from({ length: 24 }, (_, i) =>
  `- ${what} ${i + 1}: three browser test scripts pass in Chromium, long enough to wrap.\n`).join("");
writeFileSync(join(dir, "script.json"), JSON.stringify([
  { text: "Merging #268 (the subagent chat fix) into staging hit conflicts.\n\n" + points("Conflict"), chunks: 120,
    tool_calls: [{ id: "s1", name: "subagent", arguments: { task: "Resolve the conflicts" } }] },
  // The subagent, the main agent after the call, after the subagent's report
  // (in whichever order they come), then the two turns of 2. and 3.
  ...Array.from({ length: 5 }, () => ({ text: "Checked.\n\n" + points("Checked"), chunks: 120 })),
]));

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

// The backend goes with the script, however it ends.
process.on("exit", () => server.kill());

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
const errors = [];
const options = { viewport };
if (touch) {
  options.hasTouch = true;
  if (engineName !== "firefox") options.isMobile = true;
}
const context = await browser.newContext(options);
context.setDefaultTimeout(20000);
const page = await context.newPage();
page.on("console", message => {
  if (message.type() === "error" && !/favicon\.ico|interactive-widget|WebSocket/.test(message.text()))
    errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

// Every frame while [__watch]: whether the chat is at its end, the jump
// button shows, rows of the transcript overlap or a message is there twice;
// and every write to the chat's scrollTop while it was at its very end.
await page.addInitScript(() => {
  window.__frames = [];
  window.__idleWrites = 0;
  const scrollTop = Object.getOwnPropertyDescriptor(Element.prototype, "scrollTop");
  Object.defineProperty(Element.prototype, "scrollTop", {
    configurable: true,
    get() { return scrollTop.get.call(this); },
    set(value) {
      if (window.__watch && this.id === "chat"
        && this.scrollHeight - this.clientHeight - scrollTop.get.call(this) < 1)
        window.__idleWrites++;
      scrollTop.set.call(this, value);
    },
  });
  const sample = () => {
    const chat = document.getElementById("chat");
    if (chat && window.__watch) {
      const rows = [...chat.querySelectorAll(".entries > *")].filter(e => e.getClientRects().length);
      let overlap = false;
      for (let i = 1; i < rows.length; i++) {
        const a = rows[i - 1].getBoundingClientRect(), b = rows[i].getBoundingClientRect();
        if (a.height > 0 && b.height > 0 && b.top < a.bottom - 1) overlap = true;
      }
      const ids = [...chat.querySelectorAll("[data-message-entry]")].map(e => e.getAttribute("data-message-entry"));
      window.__frames.push({
        atEnd: chat.scrollTop >= chat.scrollHeight - chat.clientHeight - 1,
        jump: !!document.querySelector(".jump-to-bottom"),
        overlap,
        twice: new Set(ids).size !== ids.length,
      });
    }
    requestAnimationFrame(sample);
  };
  requestAnimationFrame(sample);
});

const section = label => console.log(`=== ${engineName}: overscroll ${part}: ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-overscroll-${part}-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};

const frame = () => page.evaluate(() => new Promise(r => requestAnimationFrame(() => r())));
const idle = () => page.waitForFunction(() => !document.querySelector(".streaming, .btn.stop"));
const watch = () => page.evaluate(() => { window.__frames = []; window.__idleWrites = 0; window.__watch = true; });
const unwatch = () => page.evaluate(() => {
  window.__watch = false;
  const f = window.__frames;
  return {
    frames: f.length,
    jump: f.filter(s => s.jump).length,
    jumpAtEnd: f.filter(s => s.jump && s.atEnd).length,
    overlap: f.filter(s => s.overlap).length,
    twice: f.filter(s => s.twice).length,
    idleWrites: window.__idleWrites,
  };
});
const chatState = () => page.evaluate(() => {
  const chat = document.getElementById("chat");
  return {
    atEnd: chat.scrollTop >= chat.scrollHeight - chat.clientHeight - 1,
    jump: !!document.querySelector(".jump-to-bottom"),
  };
});
const send = async text => {
  await page.locator(".composer textarea").fill(text);
  await page.keyboard.press("Enter");
  await page.waitForFunction(() => !!document.querySelector("#chat .msg.assistant.streaming"));
};
const middle = async () => {
  const b = await page.locator("#chat").boundingBox();
  return { x: b.x + b.width / 2, y: b.y + b.height / 2 };
};
// The user scrolling the chat: a swipe on a phone in Chromium, the wheel, or
// where Playwright has none that scrolls (WebKit: none on a phone, and a
// turn that hardly moves elsewhere), a scroll the page did not make.
let cdp = null;
const userScroll = async dy => {
  if (engineName === "webkit") {
    await page.evaluate(dy => document.getElementById("chat").scrollBy(0, dy), dy);
  } else if (touch && engineName === "chromium") {
    cdp ??= await context.newCDPSession(page);
    const b = await page.locator("#chat").boundingBox();
    const x = b.x + b.width / 2, y = b.y + b.height / 2;
    const from = y + Math.sign(dy) * 100, to = y - Math.sign(dy) * 100;
    await cdp.send("Input.dispatchTouchEvent", { type: "touchStart", touchPoints: [{ x, y: from }] });
    for (let i = 1; i <= 8; i++)
      await cdp.send("Input.dispatchTouchEvent", { type: "touchMove", touchPoints: [{ x, y: from + (to - from) * i / 8 }] });
    await cdp.send("Input.dispatchTouchEvent", { type: "touchEnd", touchPoints: [] });
  } else await page.mouse.wheel(0, dy);
};
// What the page hears when a turn up goes to the overscroll past the end
// giving way: the events, and no scroll. A swipe where the browser makes
// touches (not WebKit), else the wheel.
const unscrolledTurnUp = () => page.evaluate(touch => {
  const chat = document.getElementById("chat");
  const target = chat.querySelector(".msg.assistant.streaming") ?? chat;
  const r = chat.getBoundingClientRect();
  const x = r.left + r.width / 2, y = r.top + r.height / 2;
  const at = y => [new Touch({ identifier: 1, target, clientX: x, clientY: y })];
  let touches = touch;
  try { at(y); } catch { touches = false; }
  if (touches) {
    const fire = (type, touches) => target.dispatchEvent(new TouchEvent(type, {
      bubbles: true, cancelable: true, touches, targetTouches: touches, changedTouches: at(y + 60),
    }));
    fire("touchstart", at(y));
    fire("touchmove", at(y + 30));
    fire("touchmove", at(y + 60));
    fire("touchend", []);
    return "touch";
  }
  target.dispatchEvent(new WheelEvent("wheel", { bubbles: true, cancelable: true, deltaY: -40, clientX: x, clientY: y }));
  return "wheel";
}, touch);

try {
  const sessionUrl = new URL(url);
  sessionUrl.searchParams.set("session", id);
  await page.goto(sessionUrl.href);
  await page.waitForSelector("form input[type=password]");
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("Module 12 is fixed."));

  // 1. A reply streams (a subagent's row meanwhile) while the wheel turns on
  // past the end.
  await send("merge it");
  await watch();
  const { x, y } = await middle();
  await page.mouse.move(x, y);
  // Until the subagent has reported and the main agent has answered.
  const done = () => page.evaluate(() => {
    const msgs = [...document.querySelectorAll("#chat .entries > .msg")];
    const report = msgs.findIndex(m => /\bReport\b/.test(m.innerText));
    return report >= 0 && msgs.slice(report + 1).some(m => m.matches(".assistant"))
      && !document.querySelector(".streaming, .btn.stop");
  });
  while (!(await done())) {
    await userScroll(300);
    await frame();
  }
  await idle();
  await frame();
  await screenshot("scrolled past the end");
  section("scrolled on past the end while replies streamed");
  const passed = await unwatch();
  console.log(`subagent's rows: ${await page.locator("#chat .tool-subagent").count()}`);
  console.log(`at the end: ${(await chatState()).atEnd}`);
  console.log(`frames with the jump button at the end: ${passed.jumpAtEnd}`);
  console.log(`frames with rows overlapping: ${passed.overlap}`);
  console.log(`frames with a message twice: ${passed.twice}`);
  console.log(`scrollTop set at the end: ${passed.idleWrites} times`);

  // 2. A turn up the chat does not take, early in a reply.
  await send("check it");
  await page.waitForFunction(() => document.querySelector("#chat .msg.assistant.streaming")?.innerText.includes("Checked 1:"));
  await watch();
  const how = await unscrolledTurnUp();
  await idle();
  await frame();
  await screenshot("a turn up not taken");
  section(`a turn up (${how === "touch" ? "a swipe" : "the wheel"}) the chat does not take`);
  const untaken = await unwatch();
  const after = await chatState();
  console.log(`following (at the end): ${after.atEnd}`);
  console.log(`frames with the jump button: ${untaken.jump}`);

  // 3. A real turn up: it stops following, and the jump button shows.
  await send("check it again");
  await page.waitForFunction(() => document.querySelector("#chat .msg.assistant.streaming")?.innerText.includes("Checked 1:"));
  await page.mouse.move(x, y);
  await userScroll(-600);
  await page.locator(".jump-to-bottom").waitFor();
  await idle();
  await frame();
  await screenshot("a turn up");
  section("a real turn up");
  const up = await chatState();
  console.log(`at the end: ${up.atEnd}`);
  console.log(`jump button: ${up.jump}`);
  await page.locator(".jump-to-bottom").click();
  await page.locator(".jump-to-bottom").waitFor({ state: "detached" });
  await page.waitForFunction(() => {
    const chat = document.getElementById("chat");
    return chat.scrollTop >= chat.scrollHeight - chat.clientHeight - 1;
  });
  console.log("back at the end: true");

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (e) {
  console.log(`FAILED: ${e.stack ?? e}`);
  process.exitCode = 1;
} finally {
  await end(() => browser.close(), () => new Promise(resolve => {
    server.removeAllListeners("exit");
    if (server.exitCode !== null) return resolve();
    server.on("exit", resolve);
    server.kill();
  }));
}
