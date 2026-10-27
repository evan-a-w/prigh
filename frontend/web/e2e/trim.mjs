// The actions on a message, one row shown on hover (on a phone, on a tap):
// trimming the context before it (the user's message or a reply), the
// confirmation saying what it frees, the transcript then starting at the
// divider (Show unfolds the trimmed messages read-only above it), the
// rail's mark for it, and Undo bringing the messages back, its toast
// replacing the trim's, beside the jump-to-the-end button. Then deleting a
// message (at once, with Undo on its toast), and trimming everything after
// a message: confirmed, a divider with Show and Undo, and what is sent next
// below it.
// Run by prigh_web.sh; prints normalised results (the expected output is in
// ENGINE.expected).
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
if (!engine) throw new Error("usage: trim.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "trim");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
const sessions = join(home, ".prigh", "sessions");
mkdirSync(sessions, { recursive: true });
mkdirSync(cwd, { recursive: true });
// Nothing may wait without a deadline.
setTimeout(() => { console.error("trim.mjs: timed out"); process.exit(2); }, 240000).unref();

// Five turns of a prompt and a reply.
const turns = 5;
const id = "7c1d7c1d7c1d7c1d";
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
  entry(["Name", { name: "a session to trim" }]);
  for (let t = 1; t <= turns; t++) {
    entry(["Message", ["User", { text: `Step ${t}: look at module ${t}.` }]]);
    entry(["Message", ["Assistant", {
      content: [["Text", `Module ${t} is fixed.\n\n${"Its handler returns the error now instead of dropping it. ".repeat(6)}`]],
      stop_reason: ["End_turn"], usage, model: "faux",
    }]]);
  }
  writeFileSync(join(sessions, `20261001-100000000_${id}.jsonl`), lines.join("\n") + "\n");
};
writeSession();
writeFileSync(join(dir, "script.json"), JSON.stringify([{ text: "Noted after the trim." }]));

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
const newPage = async options => {
  const context = await browser.newContext(options);
  context.setDefaultTimeout(20000);
  const page = await context.newPage();
  page.on("console", message => {
    if (message.type() === "error" && !/favicon\.ico/.test(message.text()))
      errors.push(`console: ${message.text()}`);
  });
  page.on("pageerror", error => errors.push(`page: ${error.message}`));
  return page;
};
let page = await newPage({ viewport: { width: 1280, height: 800 } });

const section = label => console.log(`=== ${engineName}: trim: ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-trim-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};

const clean = text => text.replace(/(Yesterday )?\b\d\d:\d\d\b/g, "<TIME>").replace(/\s*\n+\s*/g, " | ");

// The steps of the user's messages in the transcript (not the trimmed ones
// shown above the divider).
const steps = () => page.evaluate(() =>
  [...document.querySelectorAll("#chat > .entries > .msg.user")]
    .map(el => Number(el.innerText.match(/Step (\d+)/)?.[1])).join(" "));
const rail = () => page.evaluate(() =>
  [...document.querySelectorAll(".rail .tick")]
    .map(t => `${t.classList.contains("trimmed") ? "trimmed" : "user"} "${t.getAttribute("aria-label")}"`)
    .join("\n"));
const step = n => page.locator("#chat > .entries > .msg.user", { hasText: `Step ${n}:` });
const reply = n => page.locator("#chat > .entries > .msg.assistant", { hasText: `Module ${n} is fixed.` });
// What the transcript shows, in order: the steps and replies, and dividers.
const transcript = () => page.evaluate(() =>
  [...document.querySelectorAll("#chat > .entries > *")].map(el => {
    if (el.classList.contains("hidden-range")) return `[${el.querySelector(".trim-divider").innerText.replace(/\s*\n+\s*/g, " ")}]`;
    const t = el.innerText;
    return t.match(/Step \d+|Module \d+|Noted after the trim/)?.[0] ?? null;
  }).filter(x => x).join(", "));
const actionsShown = el => el.locator(":scope > .msg-actions").evaluate(a =>
  new Promise(resolve => {
    const check = () => getComputedStyle(a).opacity === "1" ? resolve() : requestAnimationFrame(check);
    check();
  }));
// Hovers [el] and waits until its message's actions are shown.
const hoverActions = async el => {
  await el.hover();
  await actionsShown(el);
};
const labels = el => el.locator(":scope > .msg-actions button").allTextContents().then(t => t.join(", "));
// Whether the row of [el]'s actions covers any of its text.
const overlapsText = el => el.evaluate(msg => {
  const row = msg.querySelector(":scope > .msg-actions").getBoundingClientRect();
  const walker = document.createTreeWalker(msg, NodeFilter.SHOW_TEXT);
  const range = document.createRange();
  for (let n = walker.nextNode(); n; n = walker.nextNode()) {
    if (n.parentElement.closest(".msg-actions") || !n.textContent.trim()) continue;
    range.selectNodeContents(n);
    for (const r of range.getClientRects())
      if (r.right > row.left && r.left < row.right && r.bottom > row.top && r.top < row.bottom) return true;
  }
  return false;
});
const signIn = async () => {
  await page.goto(url);
  await page.waitForSelector("form input[type=password]");
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await page.locator(".composer textarea").waitFor();
  // On a phone the sessions are in a drawer.
  if (await page.evaluate(() => document.querySelector(".app.narrow:not(.sidebar-open)") !== null))
    await page.locator(".session-bar .toggle-sidebar").click();
  await page.locator(`[data-session="${id}"]`).click();
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("Module 5 is fixed."));
  // The drawer slid away.
  await page.waitForFunction(() => document.querySelector(".sidebar").getBoundingClientRect().right <= 0
    || !document.querySelector(".app.narrow"));
};
// Scrolls the chat to its middle, so that the jump-to-the-end button shows.
const scrollUp = async () => {
  await page.evaluate(() => {
    const chat = document.getElementById("chat");
    chat.scrollTop = Math.round((chat.scrollHeight - chat.clientHeight) / 2);
  });
  await page.locator(".jump-to-bottom").waitFor();
};
// The toasts of the context (a trim, its undo), and whether one covers the
// jump-to-the-end button.
const contextToasts = () => page.evaluate(() => {
  const jump = document.querySelector(".jump-to-bottom")?.getBoundingClientRect();
  const toasts = [...document.querySelectorAll(".toast")].filter(t => /^(Trimmed|Back in the context)/.test(t.innerText));
  const covers = toasts.some(t => {
    const r = t.getBoundingClientRect();
    return jump && r.right > jump.left && r.left < jump.right && r.bottom > jump.top && r.top < jump.bottom;
  });
  return `${toasts.map(t => t.innerText).join(" / ")}; jump button: ${jump ? "shown" : "none"}; covered: ${covers}`;
});

try {
  await signIn();

  // 1. Each message but the first can be trimmed before: one row of
  // actions, the same on every message, shown on hover, not over its text.
  section("opened");
  console.log(`steps: ${await steps()}`);
  console.log(`trim before: ${await page.locator("#chat .trim-before").count()}`);
  console.log(`on the first: ${await labels(step(1))}`);
  console.log(`on a message: ${await labels(step(3))}`);
  console.log(`on a reply: ${await labels(reply(3))}`);
  const action = step(3).locator(".trim-before");
  console.log(`opacity before hover: ${await step(3).locator(".msg-actions").evaluate(el => getComputedStyle(el).opacity)}`);
  await hoverActions(step(3));
  console.log(`over its text: ${await overlapsText(step(3))}`);
  await action.hover();
  await screenshot("hover user message");
  await hoverActions(reply(3));
  console.log(`over a reply's text: ${await overlapsText(reply(3))}`);
  await reply(3).locator(".trim-before").hover();
  await screenshot("hover reply");

  // 2. The confirmation says what it frees; Esc cancels.
  await hoverActions(step(3));
  await action.click();
  await page.locator(".trim-confirm").waitFor();
  await page.locator(".trim-confirm .rewind-quote").waitFor();
  section("confirmation");
  console.log(clean(await page.locator(".trim-confirm").innerText()));
  await screenshot("confirmation");
  await page.keyboard.press("Escape");
  await page.locator(".trim-confirm").waitFor({ state: "detached" });
  console.log(`after Esc, steps: ${await steps()}; divider: ${await page.locator(".trim-divider").count()}`);

  // 3. Enter trims: the transcript starts at the divider.
  await hoverActions(step(3));
  await action.click();
  await page.locator(".trim-confirm .rewind-quote").waitFor();
  await page.keyboard.press("Enter");
  await page.locator(".trim-divider").waitFor();
  await page.locator(".trim-confirm").waitFor({ state: "detached" });
  await page.waitForFunction(() => !document.querySelector("#chat")?.innerText.includes("Step 1:"));
  section("trimmed");
  console.log(`divider: ${clean(await page.locator(".trim-divider").innerText())}`);
  console.log(`steps: ${await steps()}`);
  console.log(`trim before: ${await page.locator("#chat > .entries .trim-before").count()}`);
  console.log(`divider in view: ${await page.locator(".trim-divider").isVisible()}`);
  console.log(`toast: ${clean(await page.locator(".toast").first().innerText())}`);
  await page.mouse.move(640, 790);
  // Marked for a moment as jumped to.
  await page.waitForFunction(() => !document.querySelector(".trim-divider.jumped"));
  await screenshot("divider");

  // 4. The rail: one mark for what is trimmed, at its top.
  await page.waitForFunction(() => document.querySelectorAll(".rail .tick").length === 4);
  section("rail");
  console.log(clean(await rail()));
  await page.locator(".rail .tick.trimmed").hover();
  await page.locator(".rail .tick.trimmed .tip").waitFor();
  console.log(`tooltip: ${clean(await page.locator(".rail .tick.trimmed .tip").innerText())}`);
  await screenshot("rail marker");

  // 5. Show: the trimmed messages above the divider, dimmed, read-only.
  await page.locator(".trim-divider button", { hasText: "Show" }).click();
  await page.locator(".trimmed-messages").waitFor();
  section("shown");
  console.log(`head: ${clean(await page.locator(".trimmed-head").innerText())}`);
  console.log(`trimmed: ${await page.evaluate(() =>
    [...document.querySelectorAll(".trimmed-messages .msg.user")].map(el => el.innerText.match(/Step \d+/)?.[0]).join(", "))}`);
  console.log(`actions on them: ${await page.locator(".trimmed-messages button.trim-before").count()}`);
  console.log(`dimmed: ${await page.locator(".trimmed-messages > .entries").evaluate(el => Number(getComputedStyle(el).opacity) < 1)}`);
  console.log(`divider: ${clean(await page.locator(".trim-divider").innerText())}`);
  await page.evaluate(() => { document.getElementById("chat").scrollTop = 0; });
  await page.mouse.move(640, 790);
  await screenshot("shown");
  await page.locator(".trim-divider button", { hasText: "Hide" }).click();
  await page.locator(".trimmed-messages").waitFor({ state: "detached" });
  console.log("hidden again");

  // 6. Undo: the messages are back in the transcript and on the rail.
  await page.locator(".trim-divider button", { hasText: "Undo" }).click();
  await page.locator(".trim-divider").waitFor({ state: "detached" });
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("Step 1:"));
  await page.waitForFunction(() => document.querySelectorAll(".rail .tick").length === 5);
  section("undone");
  console.log(`steps: ${await steps()}`);
  console.log(clean(await rail()));
  await screenshot("undone");

  // 6b. Before a reply: the context starts with it (the provider puts a
  // user turn ahead of it). Undo's toast replaces the trim's, beside the
  // jump-to-the-end button.
  await hoverActions(reply(2));
  await reply(2).locator(".trim-before").click();
  await page.locator(".trim-confirm .rewind-quote").waitFor();
  section("trim before a reply");
  console.log(clean(await page.locator(".trim-confirm").innerText()));
  await page.keyboard.press("Enter");
  await page.locator(".trim-divider").waitFor();
  await page.locator(".trim-confirm").waitFor({ state: "detached" });
  await page.waitForFunction(() => !document.querySelector("#chat")?.innerText.includes("Step 2:"));
  await page.waitForFunction(() => document.querySelectorAll(".rail .tick").length === 4);
  console.log(`transcript: ${await transcript()}`);
  console.log(clean(await rail()));
  console.log(`on the reply now first: ${await labels(reply(2))}`);
  await page.mouse.move(640, 790);
  await page.waitForFunction(() => !document.querySelector(".trim-divider.jumped"));
  await screenshot("trimmed at a reply");
  await scrollUp();
  console.log(`toasts: ${await contextToasts()}`);
  await page.locator(".trim-divider button", { hasText: "Undo" }).click();
  await page.locator(".trim-divider").waitFor({ state: "detached" });
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("Step 2:"));
  await page.locator(".toast", { hasText: "Back in the context" }).waitFor();
  await scrollUp();
  console.log(`undone: ${await transcript()}`);
  console.log(`toasts: ${await contextToasts()}`);
  await page.mouse.move(310, 400);
  await page.locator(".jump-to-bottom").evaluate(el => Promise.all(el.getAnimations().map(a => a.finished)));
  await screenshot("toasts");
  await page.locator(".jump-to-bottom").click();

  // 7. Delete: at once, no question; the toast's Undo brings it back.
  section("delete");
  console.log(`actions on the newest: ${await labels(reply(5))}`);
  await hoverActions(reply(2));
  await reply(2).locator(".msg-delete").hover();
  // What would go is dimmed.
  await page.waitForFunction(() =>
    getComputedStyle(document.querySelector("#chat [data-delete-entry='3']").closest(".msg").querySelector(".markdown")).opacity < 0.5);
  await screenshot("delete hover");
  await reply(2).locator(".msg-delete").click();
  await page.locator(".toast", { hasText: "Message deleted" }).waitFor();
  await page.waitForFunction(() => !document.querySelector("#chat")?.innerText.includes("Module 2 is fixed."));
  console.log(`toast: ${clean(await page.locator(".toast", { hasText: "Message deleted" }).innerText())}`);
  console.log(`transcript: ${await transcript()}`);
  console.log(`dividers: ${await page.locator(".trim-divider").count()}`);
  await page.mouse.move(640, 790);
  await screenshot("deleted");
  await hoverActions(step(2));
  await step(2).locator(".msg-delete").click();
  await page.waitForFunction(() => !document.querySelector("#chat")?.innerText.includes("Step 2:"));
  await page.waitForFunction(() => document.querySelectorAll(".rail .tick").length === 4);
  console.log(`after deleting step 2: ${await transcript()}`);
  console.log(clean(await rail()));
  await page.locator(".toast", { hasText: "Message deleted" }).last().locator(".toast-undo").click();
  await page.waitForFunction(() => document.querySelectorAll(".rail .tick").length === 5);
  console.log(`undone: ${await transcript()}`);
  await page.locator(".toast", { hasText: "Message deleted" }).first().locator(".toast-undo").click();
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("Module 2 is fixed."));
  console.log(`undone: ${await transcript()}`);

  // 8. Trim after: confirmed; then a divider, Show and Undo.
  await hoverActions(step(3));
  await step(3).locator(".trim-after").hover();
  await screenshot("trim after hover");
  await step(3).locator(".trim-after").click();
  await page.locator(".trim-confirm .rewind-quote").waitFor();
  section("trim after: confirmation");
  console.log(clean(await page.locator(".trim-confirm").innerText()));
  await screenshot("trim after confirmation");
  await page.keyboard.press("Escape");
  await page.locator(".trim-confirm").waitFor({ state: "detached" });
  console.log(`after Esc: ${await transcript()}`);
  await hoverActions(step(3));
  await step(3).locator(".trim-after").click();
  await page.locator(".trim-confirm .rewind-quote").waitFor();
  await page.keyboard.press("Enter");
  await page.locator(".trim-divider.after").waitFor();
  await page.locator(".trim-confirm").waitFor({ state: "detached" });
  await page.waitForFunction(() => !document.querySelector("#chat")?.innerText.includes("Step 4:"));
  await page.waitForFunction(() => document.querySelectorAll(".rail .tick").length === 3);
  section("trimmed after");
  console.log(`transcript: ${await transcript()}`);
  console.log(clean(await rail()));
  await page.mouse.move(640, 790);
  await page.waitForFunction(() => !document.querySelector(".trim-divider.jumped"));
  await screenshot("trimmed after");
  await page.locator(".trim-divider.after button", { hasText: "Show" }).click();
  await page.locator(".hidden-range .trimmed-messages").waitFor();
  console.log(`shown: ${await page.evaluate(() =>
    [...document.querySelectorAll(".hidden-range .trimmed-messages .msg")].map(el => el.innerText.match(/Step \d+|Module \d+/)?.[0]).join(", "))}`);
  console.log(`actions on them: ${await page.locator(".hidden-range .trimmed-messages button").count()}`);
  console.log(`dimmed: ${await page.locator(".hidden-range .trimmed-messages > .entries").evaluate(el => Number(getComputedStyle(el).opacity) < 1)}`);
  await page.locator(".trim-divider.after").evaluate(el => el.scrollIntoView({ block: "start" }));
  await page.mouse.move(640, 790);
  await screenshot("trimmed after shown");
  await page.locator(".trim-divider.after button", { hasText: "Hide" }).click();
  await page.locator(".hidden-range .trimmed-messages").waitFor({ state: "detached" });

  // 9. What is sent next goes below the divider.
  await page.locator(".composer textarea").fill("Step 6: after the trim.");
  await page.keyboard.press("Enter");
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("Noted after the trim."));
  await page.waitForFunction(() => document.querySelectorAll(".rail .tick").length === 4);
  section("sent after the trim");
  console.log(`transcript: ${await transcript()}`);
  await page.mouse.move(640, 790);
  await page.waitForFunction(() => !document.querySelector(".trim-divider.jumped"));
  await screenshot("sent after trim");

  // 10. Undo: the trimmed messages are back, before what was sent after.
  await page.locator(".trim-divider.after button", { hasText: "Undo" }).click();
  await page.locator(".trim-divider.after").waitFor({ state: "detached" });
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("Step 4:"));
  await page.waitForFunction(() => document.querySelectorAll(".rail .tick").length === 6);
  section("trim after undone");
  console.log(`transcript: ${await transcript()}`);
  await screenshot("trim after undone");

  // 11. A phone: no hover; a tap on a message shows its actions, a tap on
  // one asks.
  await page.context().close();
  page = await newPage({
    viewport: { width: 390, height: 844 },
    hasTouch: true,
    ...(engineName === "chromium" ? { isMobile: true } : {}),
  });
  await signIn();
  section("phone");
  await reply(3).scrollIntoViewIfNeeded();
  await reply(3).locator(".markdown").tap();
  await actionsShown(reply(3));
  console.log(`tapped: ${await labels(reply(3))}`);
  console.log(`over its text: ${await overlapsText(reply(3))}`);
  await screenshot("phone tap");
  await reply(3).locator(".trim-before").tap();
  await page.locator(".trim-confirm .rewind-quote").waitFor();
  console.log(clean(await page.locator(".trim-confirm").innerText()));
  await screenshot("phone confirmation");
  await page.locator(".trim-confirm button", { hasText: "Cancel" }).tap();
  await page.locator(".trim-confirm").waitFor({ state: "detached" });

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
