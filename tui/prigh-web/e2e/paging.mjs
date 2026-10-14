// A long session opens at its newest page: scrolling up loads the older
// ones, keeping what shows in place, until the first message; a long output
// comes cut and loads whole when unfolded; a reload scrolled up into an
// older page comes back to the same place (loading the pages above it).
// Jumping around it: the rail of the user's messages along the chat's edge
// (those not loaded too), a mark's tooltip, a jump to a loaded message and
// to one not loaded (loading the pages down to it), the jump list (Ctrl+J)
// and, on a phone, its button.
// Run by prigh_web.sh; prints normalised results (the expected output is in
// prigh_web.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: paging.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "paging");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
const sessions = join(home, ".prigh", "sessions");
mkdirSync(sessions, { recursive: true });
mkdirSync(cwd, { recursive: true });
// Nothing may wait without a deadline.
setTimeout(() => { console.error("paging.mjs: timed out"); process.exit(2); }, 240000).unref();

// [turns] turns of a prompt, a call with its output, and a reply: 3
// messages a turn. Every tenth output is long.
const turns = 40;
const id = "9a9e9a9e9a9e9a9e";
const writeSession = () => {
  let seq = 0;
  let parent = null;
  // Recent: older sessions are archived.
  let at = Math.floor(Date.now() / 1000) - 3600;
  const created = new Date(at * 1000).toISOString().replace("T", " ").replace(/\.(\d+)Z$/, ".$1000Z");
  const lines = [JSON.stringify(["Header", { id, cwd, created_at: created }])];
  const entry = payload => {
    const eid = `${id.slice(0, 8)}${String(seq++).padStart(8, "0")}`;
    lines.push(JSON.stringify(["Entry", { id: eid, parent, at: (at += 7), payload }]));
    parent = eid;
  };
  const usage = { input: 1200, output: 300, cache_read: 0 };
  const model = "faux";
  entry(["Name", { name: "a long session" }]);
  for (let t = 1; t <= turns; t++) {
    entry(["Message", ["User", { text: `Step ${t}: look at module ${t}.` }]]);
    entry(["Message", ["Assistant", {
      content: [
        ["Text", `Running the tests of module ${t}.`],
        ["Tool_call", { id: `b${t}`, name: "bash", arguments: JSON.stringify({ command: `dune test m${t}` }) }],
      ],
      stop_reason: ["Tool_use"], usage, model,
    }]]);
    const output = Array.from({ length: t % 10 === 0 ? 2000 : 8 }, (_, i) => `test_${t}_${i} ... ok`).join("\n");
    entry(["Message", ["Tool_result", { tool_call_id: `b${t}`, tool_name: "bash", text: output, is_error: false }]]);
    entry(["Message", ["Assistant", {
      content: [["Text", `Module ${t} is fixed.\n\nIts handler returns the error now instead of dropping it, and the tests pass.`]],
      stop_reason: ["End_turn"], usage, model,
    }]]);
  }
  writeFileSync(join(sessions, `20261001-100000000_${id}.jsonl`), lines.join("\n") + "\n");
};
writeSession();
writeFileSync(join(dir, "script.json"), "[]");

let server;
const startServer = listen =>
  new Promise((resolve, reject) => {
    server = spawn(
      backend,
      ["serve", "-prigh-web", listen, "-prigh-web-root", site, "-faux-script", join(dir, "script.json"),
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

const url = await startServer("127.0.0.1:0");

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
const context = await browser.newContext({ viewport: { width: 1280, height: 800 } });
context.setDefaultTimeout(20000);
const errors = [];
const newPage = async () => {
  const page = await context.newPage();
  page.on("console", message => {
    if (message.type() === "error" && !/favicon\.ico/.test(message.text()))
      errors.push(`console: ${message.text()}`);
  });
  page.on("pageerror", error => errors.push(`page: ${error.message}`));
  return page;
};
let page = await newPage();

const section = label => console.log(`=== ${engineName}: paging: ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-paging-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};

const clean = text => text.replace(/(Yesterday )?\b\d\d:\d\d\b/g, "<TIME>").replace(/\n+/g, " | ");

const steps = () => page.evaluate(() =>
  [...document.querySelectorAll("#chat .msg.user")].map(el => Number(el.innerText.match(/Step (\d+)/)?.[1])));
const loaded = async () => {
  const s = await steps();
  return `steps ${Math.min(...s)} to ${Math.max(...s)} (${s.length})`;
};
const older = () => page.evaluate(() => document.querySelector("#chat [data-older]")?.innerText.replace(/\s+/g, " ") ?? "none");
// Where step [n]'s message is, from the top of the chat's view.
const offset = n => page.evaluate(n => {
  const chat = document.getElementById("chat");
  const el = [...chat.querySelectorAll(".msg.user")].find(el => el.innerText.includes(`Step ${n}:`));
  return el ? el.getBoundingClientRect().top - chat.getBoundingClientRect().top : null;
}, n);
const scrollTo = n => page.evaluate(n => {
  const chat = document.getElementById("chat");
  const el = [...chat.querySelectorAll(".msg.user")].find(el => el.innerText.includes(`Step ${n}:`));
  chat.scrollTop += el.getBoundingClientRect().top - chat.getBoundingClientRect().top - 100;
}, n);
const kept = (before, after) => {
  const moved = after === null ? Infinity : Math.abs(after - before);
  return `${moved <= 4}${moved <= 4 ? "" : ` (moved ${moved}px)`}`;
};

try {
  await page.goto(url);
  await page.waitForSelector("form input[type=password]");
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await page.locator(".composer textarea").waitFor();

  // 1. Opened: the newest page, at its end.
  await page.locator(`[data-session="${id}"]`).click();
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("Module 40 is fixed."));
  await page.waitForTimeout(300);
  section("opened");
  console.log(await loaded());
  console.log(`earlier: ${await older()}`);
  console.log(`at the end: ${await page.evaluate(() => {
    const chat = document.getElementById("chat");
    return chat.scrollHeight - chat.scrollTop - chat.clientHeight < 40;
  })}`);
  await screenshot("opened");

  // 2. Scrolled to the top: the page before comes above, what showed stays.
  const first = Math.min(...await steps());
  await scrollTo(first);
  const at = await offset(first);
  await page.evaluate(() => {
    const chat = document.getElementById("chat");
    chat.scrollTop = Math.max(0, chat.scrollTop - 1);
  });
  await page.waitForFunction(n => Math.min(...[...document.querySelectorAll("#chat .msg.user")]
    .map(el => Number(el.innerText.match(/Step (\d+)/)?.[1]))) < n, first);
  await page.waitForFunction(() => !document.querySelector("#chat .older.busy"));
  await page.waitForTimeout(300);
  section("scrolled up");
  console.log(await loaded());
  console.log(`earlier: ${await older()}`);
  console.log(`step ${first} kept in place: ${kept(at - 1, await offset(first))}`);
  await screenshot("scrolled up");

  // 3. Up to the first message.
  for (let i = 0; i < 10 && (await older()) !== "none"; i++) {
    await page.evaluate(() => { document.getElementById("chat").scrollTop = 0; });
    await page.waitForFunction(() => !document.querySelector("#chat .older.busy"));
    await page.waitForTimeout(200);
  }
  section("at the start");
  console.log(await loaded());
  console.log(`earlier: ${await older()}`);
  await screenshot("at the start");

  // 4. A long output comes cut; unfolded, it loads whole.
  await scrollTo(10);
  const tool = page.locator(".tool", { hasText: "dune test m10" }).first();
  section("a long output");
  console.log(`shown lines: ${await tool.locator(".output pre").evaluateAll(pres =>
    pres.map(p => p.innerText.split("\n").length).join(" + "))}`);
  console.log(`toggle: ${await tool.locator("button.fold").innerText()}`);
  await tool.locator("button.fold").evaluate(button => button.click());
  await page.waitForFunction(() => [...document.querySelectorAll("#chat .tool .output pre")]
    .some(p => p.innerText.includes("test_10_1000 ")));
  console.log(`loaded whole: ${await tool.locator(".output pre").evaluateAll(pres =>
    pres.map(p => p.innerText.split("\n").length).join(" + "))} lines`);
  await screenshot("a long output");
  await tool.locator("button.fold").evaluate(button => button.click());

  // 5. Reloaded while reading an older page: back at the same place.
  await scrollTo(5);
  await page.waitForTimeout(300);
  const before = await offset(5);
  await page.reload();

  await page.waitForFunction(() => [...document.querySelectorAll("#chat .msg.user")]
    .some(el => el.innerText.includes("Step 5:")));
  await page.waitForTimeout(1500);
  section("reloaded");
  console.log(await loaded());
  console.log(`step 5 at the same place: ${kept(before, await offset(5))}`);
  await screenshot("reloaded");

  // 6. A fresh tab: only the newest page, and a mark for every message of
  //    the user's along the chat's edge, those not loaded faint.
  await page.close();
  page = await newPage();
  await page.goto(`${url}?session=${id}`);
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("Module 40 is fixed."));
  await page.waitForFunction(() => document.querySelectorAll(".rail .tick").length === 40);
  await page.waitForTimeout(300);
  const tick = n => page.locator(`.rail .tick[aria-label*="Step ${n}:"]`);
  const atTop = n => page.waitForFunction(n => {
    const chat = document.getElementById("chat");
    const el = [...chat.querySelectorAll(".msg.user")].find(el => el.innerText.includes(`Step ${n}:`));
    if (!el) return false;
    const top = el.getBoundingClientRect().top - chat.getBoundingClientRect().top;
    return top > 0 && top < 40;
  }, n);
  const current = () => page.evaluate(() =>
    document.querySelector(".rail .tick[aria-current]")?.getAttribute("aria-label").match(/Step \d+/)?.[0] ?? "none");
  section("jump: the rail");
  console.log(await loaded());
  console.log(`marks: ${await page.locator(".rail .tick").count()}, not loaded: ${await page.locator(".rail .tick.unloaded").count()}`);
  console.log(`button: ${await page.locator(".jump-list").getAttribute("aria-label")}`);
  console.log(`the one in view: ${await current()}`);
  const box = await page.locator(".rail").boundingBox();
  const chatBox = await page.locator("#chat").boundingBox();
  console.log(`along the chat's right edge: ${box.x > chatBox.x + chatBox.width - 60 && box.y >= chatBox.y}`);
  await screenshot("jump rail");

  // 7. A mark names its message on hover; a click goes there, marked.
  await tick(30).hover();
  await page.waitForFunction(() => [...document.querySelectorAll(".rail .tip")]
    .some(tip => getComputedStyle(tip).display !== "none"));
  section("jump: a mark's tooltip");
  console.log(clean(await page.evaluate(() => [...document.querySelectorAll(".rail .tip")]
    .find(tip => getComputedStyle(tip).display !== "none").innerText)));
  await screenshot("jump tooltip");
  await tick(30).click();
  await atTop(30);
  await page.waitForTimeout(100);
  section("jump: to a loaded message");
  console.log(`step 30 at the top, marked: ${await page.evaluate(() =>
    !!document.querySelector("#chat .msg.user.jumped")?.innerText.includes("Step 30:"))}`);
  await page.waitForTimeout(300);
  console.log(`the one in view: ${await current()}`);
  await screenshot("jump to a loaded message");

  // 8. One not loaded: the pages down to it, then there.
  await tick(3).click();
  await atTop(3);
  await page.waitForTimeout(300);
  section("jump: to a message not loaded");
  console.log(await loaded());
  console.log(`not loaded: ${await page.locator(".rail .tick.unloaded").count()}`);
  console.log(`the one in view: ${await current()}`);
  await screenshot("jump to a message not loaded");

  // 9. Ctrl+J: the list, newest first, filtered as typed; Enter goes there.
  await page.locator(".composer textarea").focus();
  await page.keyboard.press("Control+j");
  await page.locator(".picker-dialog").waitFor();
  await page.waitForFunction(() => document.activeElement?.id === "picker-input");
  section("jump: the list");
  console.log(clean(await page.locator(".picker-dialog .picker-item").first().innerText()));
  await page.keyboard.type("Step 17:");
  await page.waitForFunction(() => document.querySelectorAll(".picker-dialog .picker-item").length < 40);
  console.log(clean(await page.locator(".picker-dialog .picker-item").first().innerText()));
  await screenshot("jump list");
  await page.keyboard.press("Enter");
  await page.locator(".picker-dialog").waitFor({ state: "detached" });
  await atTop(17);
  console.log("step 17 at the top");

  // 10. On a phone: no rail, the button opens the list.
  await page.setViewportSize({ width: 390, height: 800 });
  await page.waitForSelector(".app.narrow");
  await page.waitForTimeout(300);
  section("jump: on a phone");
  console.log(`marks: ${await page.locator(".rail .tick").count()}`);
  await screenshot("jump phone");
  await page.locator(".jump-list").click();
  await page.locator(".picker-dialog").waitFor();
  console.log(`the list: ${await page.locator(".picker-dialog .picker-item").count()} messages`);
  await page.waitForTimeout(300);
  await screenshot("jump phone list");
  await page.keyboard.press("Escape");
  await page.locator(".picker-dialog").waitFor({ state: "detached" });

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  await screenshot("failure");
  console.error(errors.join("\n"));
  throw error;
} finally {
  await browser.close();
  await stopServer();
}
