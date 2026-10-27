// A message's context menu (a right-click, its ⋯ on hover, Shift+F10 on the
// focused message, a long press on a phone): copying it, trimming the context before it (the user's message or a reply), the
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
const more = el => el.locator(":scope > .msg-more");
// Hovers [el] and waits until its ⋯ shows.
const hoverMore = async el => {
  await el.hover({ position: { x: 40, y: 8 } });
  await more(el).evaluate(b => new Promise(resolve => {
    const check = () => getComputedStyle(b).opacity === "1" ? resolve() : requestAnimationFrame(check);
    check();
  }));
};
// The open menu, placed, once its fade in is over.
const menu = async () => {
  const m = page.locator(".context-menu.placed");
  await m.waitFor();
  await m.evaluate(el => Promise.all(el.getAnimations({ subtree: true }).map(a => a.finished)));
  return m;
};
const menuText = async () => clean(await (await menu()).evaluate(m =>
  [m.querySelector(".context-menu-head").innerText.replace(/\n/g, " — "),
    ...[...m.querySelectorAll(".menu-item")].map(i =>
      `${i.classList.contains("disabled") ? "(disabled) " : ""}${i.innerText.split("\n").join(" — ")}`)].join("\n")));
const closed = () => page.locator(".context-menu").waitFor({ state: "detached" });
const rightClick = async el => {
  await el.click({ button: "right", position: { x: 40, y: 8 } });
  await menu();
};
const viaMore = async el => {
  await hoverMore(el);
  await more(el).click();
  await menu();
};
const viaKeyboard = async el => {
  // Away from where the menu opens, which a still pointer would highlight.
  await page.mouse.move(640, 790);
  await el.focus();
  await page.keyboard.press("Shift+F10");
  await menu();
};
const choose = async item => {
  await (await menu()).locator(`[data-item=${item}]`).click();
  await closed();
};
// The items of [el]'s menu (by a right-click), then Esc.
const items = async el => {
  await rightClick(el);
  const text = await menuText();
  await page.keyboard.press("Escape");
  await closed();
  return text;
};
// Whether [el]'s ⋯ covers any of its text.
const overlapsText = el => el.evaluate(msg => {
  const row = msg.querySelector(":scope > .msg-more").getBoundingClientRect();
  const walker = document.createTreeWalker(msg, NodeFilter.SHOW_TEXT);
  const range = document.createRange();
  for (let n = walker.nextNode(); n; n = walker.nextNode()) {
    if (n.parentElement.closest(".msg-more") || !n.textContent.trim()) continue;
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

  // 1. Every message has a menu, the same everywhere, by a right-click; its
  // ⋯ shows on hover, not over its text. What does not apply is disabled,
  // saying why.
  section("opened");
  console.log(`steps: ${await steps()}`);
  console.log(`⋯: ${await page.locator("#chat .msg-more").count()}`);
  console.log(`opacity before hover: ${await more(step(3)).evaluate(el => getComputedStyle(el).opacity)}`);
  await hoverMore(step(3));
  console.log(`over its text: ${await overlapsText(step(3))}`);
  await hoverMore(reply(3));
  console.log(`over a reply's text: ${await overlapsText(reply(3))}`);
  await screenshot("hover reply");
  section("the first message's menu");
  console.log(await items(step(1)));
  section("a reply's menu");
  console.log(await items(reply(3)));
  // Highlighting a trim draws where it cuts.
  await rightClick(step(3));
  section("a message's menu");
  console.log(await menuText());
  await page.locator(".context-menu.placed [data-item=trim_before]").hover();
  await page.locator(".msg-more.preview-trim-before").waitFor();
  console.log(`cut line: ${await step(3).evaluate(el => getComputedStyle(el, "::after").borderTopStyle)}`);
  await screenshot("menu trim before hover");
  // Esc closes it, nothing else.
  await page.keyboard.press("Escape");
  await closed();
  console.log(`after Esc: ${await steps()}; preview: ${await page.locator(".msg-more[class*=preview]").count()}`);

  // A right-click on text selected in the message is the browser's, to
  // copy it.
  section("selected text");
  console.log(`menu prevented: ${await step(3).evaluate(msg => {
    const range = document.createRange();
    range.selectNodeContents(msg.querySelector(".bubble") ?? msg);
    getSelection().removeAllRanges();
    getSelection().addRange(range);
    const r = msg.getBoundingClientRect();
    const ev = new MouseEvent("contextmenu", { bubbles: true, cancelable: true, clientX: r.left + 40, clientY: r.top + 8 });
    msg.dispatchEvent(ev);
    getSelection().removeAllRanges();
    return ev.defaultPrevented;
  })}`);

  // Copy: the message's text (the clipboard stubbed: headless browsers
  // refuse it).
  await page.evaluate(() => {
    navigator.clipboard.writeText = async text => { window.copied = text; };
  });
  await rightClick(reply(3));
  await choose("copy_message");
  await page.locator(".toast", { hasText: "Copied" }).first().waitFor();
  section("copy");
  console.log(`toast: ${clean(await page.locator(".toast", { hasText: "Copied" }).first().innerText())}`);
  console.log(`copied: ${clean(await page.evaluate(() => window.copied))}`);

  // 2. The ⋯ opens it under itself; the confirmation says what a trim
  // frees; Esc cancels.
  await viaMore(step(3));
  await screenshot("menu by the more button");
  await choose("trim_before");
  await page.locator(".trim-confirm").waitFor();
  await page.locator(".trim-confirm .rewind-quote").waitFor();
  section("confirmation");
  console.log(clean(await page.locator(".trim-confirm").innerText()));
  await screenshot("confirmation");
  await page.keyboard.press("Escape");
  await page.locator(".trim-confirm").waitFor({ state: "detached" });
  console.log(`after Esc, steps: ${await steps()}; divider: ${await page.locator(".trim-divider").count()}`);

  // 3. From the keyboard: Shift+F10 on the focused message, ↓ to the item,
  // Enter; Enter trims: the transcript starts at the divider.
  await viaKeyboard(step(3));
  await page.keyboard.press("ArrowDown");
  await page.locator(".context-menu.placed [data-item=trim_before].selected").waitFor();
  await page.locator(".msg-more.preview-trim-before").waitFor();
  section("keyboard");
  console.log(`highlighted: ${await page.locator(".context-menu.placed .menu-item.selected .menu-label").innerText()}`);
  await screenshot("menu by the keyboard");
  await page.keyboard.press("Enter");
  await closed();
  await page.locator(".trim-confirm .rewind-quote").waitFor();
  await page.keyboard.press("Enter");
  await page.locator(".trim-divider").waitFor();
  await page.locator(".trim-confirm").waitFor({ state: "detached" });
  await page.waitForFunction(() => !document.querySelector("#chat")?.innerText.includes("Step 1:"));
  section("trimmed");
  console.log(`divider: ${clean(await page.locator(".trim-divider").innerText())}`);
  console.log(`steps: ${await steps()}`);
  console.log(`⋯: ${await page.locator("#chat > .entries .msg-more").count()}`);
  console.log(`divider in view: ${await page.locator(".trim-divider").isVisible()}`);
  console.log(`toast: ${clean(await page.locator(".toast", { hasText: "Trimmed" }).innerText())}`);
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
  console.log(`⋯ on them: ${await page.locator(".trimmed-messages .msg-more").count()}`);
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
  await rightClick(reply(2));
  await choose("trim_before");
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
  console.log(`on the reply now first: ${await items(reply(2))}`);
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
  console.log(`the newest's menu: ${await items(reply(5))}`);
  await rightClick(reply(2));
  await page.locator(".context-menu.placed [data-item=delete]").hover();
  // What would go is dimmed.
  await page.waitForFunction(() =>
    getComputedStyle(document.querySelector("#chat .msg-more.preview-delete").closest(".msg").querySelector(".markdown")).opacity < 0.5);
  await screenshot("delete hover");
  await choose("delete");
  await page.locator(".toast", { hasText: "Message deleted" }).waitFor();
  await page.waitForFunction(() => !document.querySelector("#chat")?.innerText.includes("Module 2 is fixed."));
  console.log(`toast: ${clean(await page.locator(".toast", { hasText: "Message deleted" }).innerText())}`);
  console.log(`transcript: ${await transcript()}`);
  console.log(`dividers: ${await page.locator(".trim-divider").count()}`);
  await page.mouse.move(640, 790);
  await screenshot("deleted");
  await viaMore(step(2));
  await choose("delete");
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
  await viaMore(step(3));
  await page.locator(".context-menu.placed [data-item=trim_after]").hover();
  await page.locator(".msg-more.preview-trim-after").waitFor();
  await screenshot("trim after hover");
  await choose("trim_after");
  await page.locator(".trim-confirm .rewind-quote").waitFor();
  section("trim after: confirmation");
  console.log(clean(await page.locator(".trim-confirm").innerText()));
  await screenshot("trim after confirmation");
  await page.keyboard.press("Escape");
  await page.locator(".trim-confirm").waitFor({ state: "detached" });
  console.log(`after Esc: ${await transcript()}`);
  await viaKeyboard(step(3));
  await page.keyboard.press("ArrowDown");
  await page.keyboard.press("ArrowDown");
  await page.locator(".context-menu.placed [data-item=trim_after].selected").waitFor();
  await page.keyboard.press("Enter");
  await closed();
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
  console.log(`⋯ on them: ${await page.locator(".hidden-range .trimmed-messages .msg-more").count()}`);
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

  // 11. A phone: no hover; a long press on a message opens its menu, a
  // sheet at the foot of the screen; a tap on an item asks.
  await page.context().close();
  page = await newPage({
    viewport: { width: 390, height: 844 },
    hasTouch: true,
    ...(engineName === "chromium" ? { isMobile: true } : {}),
  });
  await signIn();
  section("phone");
  await reply(3).scrollIntoViewIfNeeded();
  const target = reply(3).locator(".markdown");
  const box = await target.boundingBox();
  await target.dispatchEvent("pointerdown", {
    pointerType: "touch", isPrimary: true, clientX: box.x + 30, clientY: box.y + 10, bubbles: true,
  });
  await menu();
  await target.dispatchEvent("pointerup", { pointerType: "touch", bubbles: true });
  console.log(await menuText());
  await screenshot("phone long press");
  await page.locator(".context-menu.placed [data-item=trim_before]").tap();
  await closed();
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
