// The transcript filter (/filter): its button left of the files', its menu
// (arrows, Space, Enter, Esc, Show all), what it hides and the line saying
// so, the button's count, the filter kept across a reload and per session
// (another has its own), the jump rail with the user's messages hidden, the
// image viewer over the button, and all of it on a phone.
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
if (!engine) throw new Error("usage: filter.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "filter");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
const sessions = join(home, ".prigh", "sessions");
mkdirSync(sessions, { recursive: true });
mkdirSync(cwd, { recursive: true });
// Nothing may wait without a deadline.
setTimeout(() => { console.error("filter.mjs: timed out"); process.exit(2); }, 240000).unref();

// Two sessions: one of each kind the filter knows, and a short one.
const id = "f11efa11f11efa11";
const other = "0f0f0f0f0f0f0f0f";
const writeSession = (sid, name, minutesAgo, messages) => {
  let seq = 0;
  let parent = null;
  let at = Math.floor(Date.now() / 1000) - minutesAgo * 60;
  const created = new Date(at * 1000).toISOString().replace("T", " ").replace(/\.(\d+)Z$/, ".$1000Z");
  const lines = [JSON.stringify(["Header", { id: sid, cwd, created_at: created }])];
  const entry = payload => {
    const eid = `${sid.slice(0, 8)}${String(seq++).padStart(8, "0")}`;
    lines.push(JSON.stringify(["Entry", { id: eid, parent, at: (at += 7), payload }]));
    parent = eid;
  };
  entry(["Name", { name }]);
  for (const m of messages) entry(["Message", m]);
  writeFileSync(join(sessions, `20261001-100000000_${sid}.jsonl`), lines.join("\n") + "\n");
};
const usage = { input: 1200, output: 300, cache_read: 0 };
const model = "faux";
const image = png(48, 32, (x, y) => (y < 16 ? [40, 90, 200] : [220, 60, 50]));
writeSession(id, "every kind", 60, [
  ["User", { text: "Why does the build fail?", images: [{ mime_type: "image/png", data: image.toString("base64") }] }],
  ["Assistant", {
    content: [
      ["Thinking", "The Makefile probably lost its default rule."],
      ["Text", "Let me look at the build."],
      ["Tool_call", { id: "b1", name: "bash", arguments: JSON.stringify({ command: "make" }) }],
      ["Tool_call", { id: "s1", name: "subagent", arguments: JSON.stringify({ task: "Read the docs on building" }) }],
    ],
    stop_reason: ["Tool_use"], usage, model,
  }],
  ["Tool_result", { tool_call_id: "b1", tool_name: "bash", text: "make: *** No rule to make target.\n[exit code 2]", is_error: true }],
  ["Tool_result", { tool_call_id: "s1", tool_name: "subagent", text: "The docs say: run make all.", is_error: false }],
  ["Assistant", { content: [["Text", "The default rule is missing: `make all` builds."]], stop_reason: ["End_turn"], usage, model }],
  ["User", { text: "Add the rule back." }],
  ["Assistant", { content: [["Text", "Adding it"]], stop_reason: ["Aborted"], usage, model }],
]);
writeSession(other, "a short one", 120, [
  ["User", { text: "Hello there." }],
  ["Assistant", { content: [["Thinking", "A greeting."], ["Text", "Hello! What shall we build?"]], stop_reason: ["End_turn"], usage, model }],
]);
writeFileSync(join(dir, "script.json"), "[]");

let server;
const startServer = listen =>
  new Promise((resolve, reject) => {
    server = spawn(
      backend,
      ["serve", "-web", listen, "-web-root", site, "-faux-script", join(dir, "script.json"),
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
const errors = [];
const watch = page => {
  page.on("console", message => {
    if (message.type() === "error" && !/favicon\.ico/.test(message.text()))
      errors.push(`console: ${message.text()}`);
  });
  page.on("pageerror", error => errors.push(`page: ${error.message}`));
};
const context = await browser.newContext({ viewport: { width: 1280, height: 800 } });
context.setDefaultTimeout(20000);
let page = await context.newPage();
watch(page);

const section = label => console.log(`=== ${engineName}: filter: ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.waitForFunction(() => document.getAnimations().every(a => a.playState !== "running"));
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-filter-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};

const clean = text => text.replace(/(Yesterday )?\b\d\d:\d\d\b/g, "<TIME>").replace(/\s*\n+\s*/g, " | ");

// What the transcript shows, a line per kind of block.
const shown = () => page.evaluate(() => {
  const chat = document.querySelector("#chat > .entries");
  const has = sel => chat.querySelectorAll(sel).length;
  return [
    `user ${has(".msg.user")}`,
    `replies ${has(".msg.assistant")}`,
    `thinking ${has(".thinking")}`,
    `tools ${has(".tool:not(.tool-subagent)")}`,
    `subagents ${has(".tool-subagent")}`,
    `stopped ${has(".stop")}`,
  ].join(", ");
});
const button = () => page.evaluate(() => {
  const b = document.getElementById("filter-toggle");
  const next = b.nextElementSibling;
  return `${b.getAttribute("aria-label")}; ${b.classList.contains("active") ? "accented" : "plain"}; ` +
    `badge ${b.querySelector(".badge")?.innerText ?? "none"}; next: ${next?.getAttribute("aria-label")}`;
});
const note = async () =>
  (await page.locator(".filter-note").count()) ? clean(await page.locator(".filter-note").innerText()) : "none";
const menu = () => page.evaluate(() =>
  [...document.querySelectorAll(".filter-menu .menu-item")].map(el =>
    `${el.classList.contains("selected") ? ">" : " "} ${el.getAttribute("aria-checked") === "true" ? "[x]"
      : el.getAttribute("aria-checked") === "false" ? "[ ]" : "   "} ${el.querySelector(".menu-label").innerText}`)
    .join("\n"));
// The menu under its button and inside the window.
const placement = () => page.evaluate(() => {
  const m = document.querySelector(".filter-menu").getBoundingClientRect();
  const b = document.getElementById("filter-toggle").getBoundingClientRect();
  const inside = m.left >= 0 && m.right <= innerWidth && m.top >= 0 && m.bottom <= innerHeight;
  return `below the button: ${m.top >= b.bottom - 1}; inside the window: ${inside}; focused: ${document.activeElement?.classList.contains("filter-menu")}`;
});
const openMenu = async () => {
  await page.locator("#filter-toggle").click();
  await page.locator(".filter-menu.placed").waitFor();
};
const closed = () => page.locator(".filter-menu").waitFor({ state: "detached" });

const signIn = async () => {
  await page.goto(url);
  await page.waitForSelector("form input[type=password]");
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await page.locator(".composer textarea").waitFor();
};
const openSession = async (sid, text) => {
  await page.locator(`[data-session="${sid}"]`).click();
  await page.waitForFunction(text => document.querySelector("#chat")?.innerText.includes(text), text);
};

try {
  await signIn();
  await openSession(id, "Adding it");

  // 1. Everything shows; the button sits left of the files'.
  section("opened");
  console.log(await shown());
  console.log(await button());
  console.log(`note: ${await note()}`);
  await screenshot("everything");

  // 2. The menu, under the button, owns the keyboard.
  await openMenu();
  section("menu");
  console.log(await menu());
  console.log(await placement());
  await screenshot("menu");

  // 3. Space hides what is selected at once; Enter keeps it.
  await page.keyboard.press("ArrowDown");
  await page.keyboard.press("ArrowDown");
  await page.keyboard.press(" ");
  await page.keyboard.press("ArrowDown");
  await page.keyboard.press(" ");
  await page.waitForFunction(() => !document.querySelector("#chat .thinking, #chat .tool:not(.tool-subagent)"));
  section("thinking and tools hidden");
  console.log(await menu());
  console.log(await shown());
  await screenshot("hiding in the menu");
  await page.keyboard.press("Enter");
  await closed();
  console.log(await button());
  console.log(`note: ${await note()}`);
  await page.mouse.move(640, 790);
  await screenshot("filtered");

  // 4. Esc puts back the filter the menu opened with.
  await openMenu();
  await page.keyboard.press(" ");
  await page.waitForFunction(() => !document.querySelector("#chat .msg.user"));
  await page.keyboard.press("Escape");
  await closed();
  await page.waitForFunction(() => document.querySelector("#chat .msg.user"));
  section("Esc");
  console.log(await shown());
  console.log(await button());

  // 5. A click outside keeps it, like Enter.
  await openMenu();
  await page.locator(".filter-menu .menu-item", { hasText: "Subagents" }).click();
  await page.waitForFunction(() => !document.querySelector("#chat .tool-subagent"));
  await page.mouse.click(640, 400);
  await closed();
  section("clicked outside");
  console.log(await shown());
  console.log(await button());

  // 6. Kept across a reload (the session keeps it); another session has its
  // own (none here), and coming back brings this one's back.
  await page.reload();
  await page.locator(".composer textarea").waitFor();
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("Adding it"));
  await page.locator(".filter-note").waitFor();
  section("reloaded");
  console.log(await shown());
  console.log(await button());
  await openSession(other, "What shall we build?");
  section("another session");
  console.log(await shown());
  console.log(`note: ${await note()}`);
  await openSession(id, "Adding it");
  await page.locator(".filter-note").waitFor();
  section("back");
  console.log(await shown());
  console.log(`note: ${await note()}`);

  // 7. Your messages hidden: the rail still lists them and jumps to them.
  await openMenu();
  await page.keyboard.press(" ");
  await page.keyboard.press("Enter");
  await closed();
  await page.waitForFunction(() => !document.querySelector("#chat .msg.user"));
  section("your messages hidden");
  console.log(await shown());
  console.log(`rail: ${await page.locator(".rail .tick").count()} marks`);
  await page.locator("#chat").evaluate(el => { el.scrollTop = el.scrollHeight; });
  await page.locator(".rail .tick").first().click();
  await page.waitForFunction(() => {
    const target = document.querySelector('#chat [data-entry="0"]');
    const chat = document.getElementById("chat");
    return target && Math.abs(target.getBoundingClientRect().top - chat.getBoundingClientRect().top) < chat.clientHeight / 2;
  });
  console.log("the rail's first mark: jumped to where the message is");
  await page.keyboard.press("Control+j");
  await page.locator(".picker-option, [role=option]").first().waitFor();
  console.log(`Ctrl+J lists: ${await page.locator("#dialog [role=option]").count()} messages`);
  await page.keyboard.press("Escape");
  await page.locator("#dialog").waitFor({ state: "detached" });
  await screenshot("your messages hidden");

  // 8. Show all, from the line over the transcript.
  await page.locator(".filter-show-all").click();
  await page.locator(".filter-note").waitFor({ state: "detached" });
  section("show all");
  console.log(await shown());
  console.log(await button());

  // 9. The image viewer covers the button and the composer's chips.
  await page.locator('[data-entry="0"] img.thumb').click();
  await page.locator("details.image[open] .lightbox").waitFor();
  const covered = await page.evaluate(() =>
    ["#filter-toggle", ".composer-controls .chip-button"].map(sel => {
      const r = document.querySelector(sel).getBoundingClientRect();
      const top = document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2);
      return `${sel}: ${top?.closest(".lightbox") ? "under the image" : "on top"}`;
    }).join("; "));
  section("image viewer");
  console.log(covered);
  await screenshot("image viewer");
  await page.keyboard.press("Escape");
  await page.locator("details.image[open]").waitFor({ state: "detached" });

  const sessionUrl = page.url();
  // 10. On a phone: the button in the slim bar, the menu inside the screen.
  await page.close();
  const phone = await browser.newContext({ viewport: { width: 390, height: 760 } });
  phone.setDefaultTimeout(20000);
  page = await phone.newPage();
  watch(page);
  // The session the desktop page was on, whose ?session= it kept.
  await page.goto(sessionUrl);
  await page.waitForSelector("form input[type=password]");
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("Adding it"));
  await page.locator("#filter-toggle").click();
  await page.locator(".filter-menu.placed").waitFor();
  section("phone");
  console.log(await placement());
  await screenshot("phone menu");
  await page.locator(".filter-menu .menu-item", { hasText: "Thinking" }).click();
  await page.locator(".filter-menu .menu-item", { hasText: "Tool calls" }).click();
  await page.waitForFunction(() => !document.querySelector("#chat .thinking, #chat .tool:not(.tool-subagent)"));
  await page.mouse.click(200, 700);
  await closed();
  console.log(await shown());
  console.log(await button());
  console.log(`note: ${await note()}`);
  const fits = await page.evaluate(() => {
    const n = document.querySelector(".filter-note").getBoundingClientRect();
    const b = document.getElementById("filter-toggle").getBoundingClientRect();
    const all = document.querySelector(".filter-show-all").getBoundingClientRect();
    const top = document.elementFromPoint(all.left + all.width / 2, all.top + all.height / 2);
    return `note inside the screen: ${n.left >= 0 && n.right <= innerWidth}; Show all ${top?.closest(".filter-show-all") ? "clickable" : "covered"}; button inside: ${b.left >= 0 && b.right <= innerWidth}`;
  });
  console.log(fits);
  await screenshot("phone filtered");
  await page.locator("#filter-toggle").click();
  await page.locator(".filter-menu.placed").waitFor();
  await page.locator(".filter-menu .menu-item", { hasText: "Show all" }).click();
  await page.waitForFunction(() => !document.querySelector(".filter-note"));
  await page.locator(".filter-menu .menu-item", { hasText: "Show all" }).waitFor();
  console.log(`after Show all: ${await shown()}`);
  await page.keyboard.press("Enter");
  await closed();

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  await screenshot("failure");
  console.error(await page.evaluate(() => document.querySelector("#chat")?.outerHTML.slice(0, 3000)));
  console.error(errors.join("\n"));
  throw error;
} finally {
  await end(() => stopServer(), () => browser.close());
}
